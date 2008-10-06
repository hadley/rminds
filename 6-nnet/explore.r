library(classifly)

accuracy <- function(x) {
	x$nnet$lev <- levels(w$class)
	tb <- table(predict(x$nnet, type="class"), w$class)
	1 - (tb[2,1] + tb[1,2]) / sum(tb)
}

coef2 <- function(obj) {
	params <- obj$n[1] + 1
	size <- obj$n[2]
	classes <- obj$n[3]
	x <- coef(obj)

	hidden <- t(matrix(x[1:(size*params)], nrow=params))
	rownames(hidden) <- 1:nrow(hidden)
	colnames(hidden) <- c("b", 1:(ncol(hidden)-1))
	
	rem <- x[-(1:(size*params))]
	output <- matrix(rem, nrow=classes, byrow=TRUE)
	rownames(output) <- 1:nrow(output)
	colnames(output) <- c("b", 1:(ncol(output)-1))
	
	list(hidden=hidden, output=output)
}

pred_fs <- function(obj) {
	cf <- coef2(obj)
	
	hiddenf <- lapply(1:nrow(cf$hidden), function(i) {
	  force(i)
	  function(x) plogis(cf$hidden[i,1] + x %*% cf$hidden[i,-1])
	})

	outputf <- lapply(1:nrow(cf$output), function(i) {
	  force(i)
	  function(x) plogis(cf$output[i,1] + sapply(1:nrow(cf$hidden), function(j) hiddenf[[j]](x)) %*% cf$output[i, -1])
	})

	list(hidden=hiddenf, output=outputf)
}


gen_preds <- function(response, explanatory, ...) {
	nn <- nnet(response, class.ind(explanatory), ..., trace=FALSE)
	fs <- pred_fs(nn)

	newdf <- generate_data(response, 1000)
		
	hidden <- do.call(rbind, 
		lapply(1:length(fs$hidden), function(i) 
			cbind(newdf, node=i, pred=fs$hidden[[i]](as.matrix(newdf)))
		)
	)
	output <- do.call(rbind, 
		lapply(1:length(fs$output), function(i) 
			cbind(newdf, node=i, pred=fs$output[[i]](as.matrix(newdf)))
		)
	)
	
	structure(list(hidden=hidden, output=output, nnet=nn, coef=coef2(nn)), class="nnetvis")
}

plot.nnetvis <- function(x, filename) {
	height <- 4
	width <- x$nn$n[2] * height * 1.7

	output <- rbind(
		x$hidden,
		transform(subset(x$output, node == 1), node ="output")
	)

	if (!missing(filename)) pdf(file=filename, height=height, width=width)#, surface="png")

	p <- ggplot(output, aes(x=x, y=y, fill=pred, z=pred), formula=. ~ node) + 
		geom_tile() + 
		geom_point(aes(x=x, y=y, size=class, fill=1, z=1),data=as.data.frame(w)) + 
		scale_fill_gradient("Predicted", low="green", high="red", range = c(0,1))
		
	p$aspect.ratio <- 1
	p$legend.position <- "none"

	print(p)

	if (!missing(filename)) dev.off()
	
}

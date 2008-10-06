source("nnet-explore.r")

accuracy <- function(x) {
	x$nnet$lev <- levels(wiggly$class)
	tb <- table(predict(x$nnet, type="class"), wiggly$class)
	1 - (tb[2,1] + tb[1,2]) / sum(tb)
}

manypred <- function(n=100, size=size, ...) {
	fit <- function(...) {
		gen_preds(wiggly[,-3], wiggly[,3], maxit = 500, size=size, ...)
	}
	
	multi <- replicate(n, fit(), simplify=FALSE)

	coefs <- lapply(1:length(multi), function(i) {
		m <- multi[[i]]
		cbind(coef2(m$nnet)$hidden, value=m$nnet$value, accuracy=accuracy(m), i=i)
	})
	coefs <- data.frame(do.call("rbind", coefs))
	coefs$i <- factor(coefs$i)
	
	list(all=multi, coef=coefs)
}

# many2 <- manypred(200, size=2)
# many2$coef$size <- 2
# many3 <- manypred(200, size=3)
# many3$coef$size <- 3
# many4 <- manypred(200, size=4)
# many4$coef$size <- 4
# 
# save(many2, many3, many4, file="wiggly-multi.rdata", compress=TRUE)
load("wiggly-multi.rdata")
many <- rbind(many2$coef, many3$coef, many4$coef)

many <- rename(many, c(size = "nodes"))
many$id <- (many$nodes - 2) * 200 + as.integer(many$i)
qplot(value, accuracy, data=many, facet = . ~ nodes)
ggsave(file="many-fits.pdf")


source("nnet-explore.r")
load("wiggly-multi.rdata")

g <- ggobi(many4$coef)
gSignalConnect(g, "identify-point", function(gg, plot, i, data) { 
	if (i == -1) return()
	print(i)
	i <- as.numeric(as.character(data[i + 1,]$i))
	# i <- as.numeric(as.character(many4$coef[i,]$i))
	print(i)
	
	
  plot(many4$all[[i]])
})

# GGobi code to draw and animate

# pdf("~/desktop/hidden-layer-values.pdf", width=8, height=8)
# qplot(X1, X2, data=multim, xlim=c(-500, 500), ylim=c(-500, 500), size=crit)
# qplot(X1, X2, data=multim, xlim=c(-100, 100), ylim=c(-100, 100), size=crit)
# qplot(X1, X2, data=multim, xlim=c(-50, 50), ylim=c(-50, 50), size=crit)
# qplot(X1, X2, data=multim, xlim=c(-10, 10), ylim=c(-10, 10), size=crit)
# dev.off()
# 
# multim$a <- (as.numeric(multim$i) - 1) %/% 10
# multim$b <- (as.numeric(multim$i) - 1) %% 10
# 
# pdf("~/desktop/hidden-layer-individual.pdf", width=8, height=8)
# qplot(X1, X2, data=multim, xlim=c(-50, 50), ylim=c(-50, 50), size=crit, facets=a~b) + stat_sort_angle(colour=alpha("grey50", 0.5), fill=NA)
# dev.off()

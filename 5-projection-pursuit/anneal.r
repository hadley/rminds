library(scagnostics)

scagnostics.anneal <- function(df, idx, cool=0.99, temp=1, start=NULL, max.steps = 20, min.delta = 0.001, max.tries = max.steps * 10, ...) {
  finished <- function() {
    if (i >= max.steps) return(TRUE)
    if (try >= max.tries) return(TRUE)
    # if (abs(deltaI) <= min.delta) return(TRUE)
    
    FALSE
  }
  
  i <- 1
  try <- 0
  df <- as.matrix(df)
  
  if (is.null(start)) {
    A0 <- proj(df)
  } else {
    A0 <- ortho(start)
  }
  
  res <- scagnostics(as.data.frame(df %*% A0))
  I0 <- res[idx]
  
  index <- vector("numeric", max.steps)
  tries <- vector("numeric", max.steps)
  temps <- vector("numeric", max.steps)
  probs <- vector("numeric", max.steps)
  projs <- vector("list", max.steps)
  
  deltaI <- Inf
  index[1] <- I0
  tries[1] <- try
  temps[1] <- temp
  projs[[1]] <- A0
  
  
  while (!finished()) {
    try <- try + 1
    B <- proj(df)
    Ai <- ortho(A0 + (cool ^ i) * B)
    Ii <- scagnostics(as.data.frame(df %*% Ai))[idx]
    Ti <- temp / log(i + 1)
    deltaI <- Ii - I0
    rho <- min(exp(deltaI / (Ii * Ti)), 1, na.rm=TRUE)
    cat(".")
  
    if (rbinom(1,1,rho)>0) {
      cat("|")
      i <- i+1

      projs[[i]] <- A0 <- Ai
      index[i] <- I0 <- Ii
      tries[i] <- try
      temps[i] <- Ti
      probs[i] <- rho
    }

  }
  cat("   .\n")
  data.frame(step = seq_len(max.steps), try = tries, index=index, temp=temps, prob=probs, projs = I(projs))[1:i, ]
}

getproj <- function(p, x=NULL) {
	pp <- rnorm(p)
	if (length(x) > 0) {
		pp <- pp - crossprod(x,pp) * x
	}

	return(pp / sqrt(crossprod(pp)))
}

proj <- function(df) {
	df <- as.matrix(df)
	cols <- ncol(df)
	pp1 <- getproj(cols)
	pp2 <- getproj(cols, pp1)

#	plot(df %*% pp1, df %*% pp2)
	return(cbind(pp1,pp2))
}

ortho <- function(x) {
	pp1 <- x[,1]/sqrt(crossprod(x[,1]))
	pp2 <- x[,2] - crossprod(x[,2],pp1)*pp1
	pp2 <- pp2/sqrt(crossprod(pp2))
	return(cbind(pp1,pp2))
}
conf.ellipse <- function(data, npoints=1000, cl=0.95, mean=colMeans(data), cov=var(data), n=nrow(data)) {
  norm.vec  <-  function(x) x / sqrt(sum(x^2))

  p <- length(mean)
  ev <- eigen(cov)

  normsamp <- matrix(rnorm(npoints*p), ncol=p)
  sphere <- t(apply(normsamp, 1, norm.vec))

  ellipse <- sphere %*% diag(sqrt(ev$values)) %*% t(ev$vectors)
  conf.region <- ellipse * sqrt(p * (n-1) * qf(cl, p, n-p) / (n * (n-p)))
  if (!missing(data)) colnames(conf.region) <- colnames(data)

  data.frame(conf.region + rep(mean, each=npoints) )
}
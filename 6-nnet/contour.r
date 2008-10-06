contour <- function(x, y, z, levels = 0.5) {
  gridise <- function(x) {
    unique <- sort(unique(x[!is.na(x)]))
    group <- match(x, unique)
    list(unique=unique, group=group)
  }

  gridx <- gridise(x)
  gridy <- gridise(y)
  
  gridz <- matrix(NA, nrow = length(gridx$unique), ncol = length(gridy$unique))
  gridz[(gridy$group - 1) * length(gridx$unique) + gridx$group] <- z

  cl <- contourLines(x = gridx$unique, y = gridy$unique, z = gridz, levels = levels)  

  cl <- mapply(function(x, piece) {
    rbind(data.frame(x, piece=piece), c(NA, NA, NA))
  }, cl, seq_along(cl), SIMPLIFY=FALSE)
  do.call("rbind", cl)  
}


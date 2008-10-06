library(ggplot2)
#ggopt(grid.fill = "grey95")
#source("~/documents/reshape/reshape/load.r")

wine <- read.csv("../1-data/wine.csv")
winer <- rescaler(wine, "range")

wgobi <- function(...) {
  g <- ggobi(...)
  glyph_colour(g[[1]]) <- c(3, 4, 5)[wine$type]  
  invisible(g)
}

col <- scale_colour_manual("Variety", values = c("#E41A1C", "#377EB8", "#4DAF4A"))
fill <- scale_fill_manual("Variety", values = alpha(c("#E41A1C", "#377EB8", "#4DAF4A"), 0.3))

recolour <- function(g = ggobi_get()[[1]]) {
  glyph_colour(g) <- glyph_colour(g) + 1  
}
winefly <- function(..., formula = type ~ color + phenols + flavanoids, n=1e5) {
  print(classifly(wine, formula, ..., n=1e5))
  recolour()
}


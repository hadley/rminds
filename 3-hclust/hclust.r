library(reshape)
library(clusterfly)

source("../1-data/wine.r")


sngl2 <- hierfly(winer[, -1], method="single")
ward2 <- hierfly(winer[, -1], method="ward")

source("dendro.r")
gdendro(ward2)
ggsave(file = "wine-ward.pdf", width=6, height=4)
gdendro(sngl2)
ggsave(file = "wine-single.pdf", width=6, height=4)

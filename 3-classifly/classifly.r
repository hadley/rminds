library(classifly)

source("../1-data/wine.r")

qplot(color, phenols, data=wine, colour=type) + col

wine_lda <- classifly(winer, type ~ color + phenols, lda, n = 100 * 100, method="grid")
grid <- subset(wine_lda, .TYPE == "simulated")

ggplot(grid, aes(color, phenols)) + geom_tile(aes(fill = type), colour=NA) + geom_point(aes(colour=type), data=winer) + col + fill + coord_equal()
ggsave(file = "class-2d-shade.pdf", width=5, height=4)

ggplot(grid, aes(color, phenols, colour=type)) + geom_point(data=winer) + geom_contour(data = grid, size = 0.5) + scale_z_continuous(breaks=0.05) + col + coord_equal()
ggsave(file = "class-2d-boundary.pdf", width=5, height=4)


w <- rescaler(read.csv("../nnet/wiggly.csv")[, -1], "range")
w$class <- factor(w$class)
classifly(w, class ~ ., randomForest)

winefly(lda)
winefly(svm, probability=T, kernel="linear")

f5 <- type ~ color + phenols + flavanoids + proline + dilution
winefly(svm, probability=T, kernel="polynomial", formula = f5, n=1e6)

winefly(svm, probability=T, kernel="radial")



# Random forests don't give clean boundaries

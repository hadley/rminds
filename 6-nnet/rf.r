library(randomForest)
library(ggplot2)


col <- scale_colour_manual("Class", values = c("#E41A1C", "#377EB8"))
fill <- scale_fill_manual("Class", values = alpha(c("#E41A1C", "#377EB8"), 0.3))



w <- read.csv("wiggly.csv")[ ,2:4]
w <- transform(w, class = factor(class, labels = c("A", "B")))
w <- rescaler(w, "range")

qplot(x, y, data=w, colour=class) + col

rf <- randomForest(class ~ x + y, w)

grid <- expand.grid(x = seq(0, 1, length=50), y = seq(0, 1, length=50))
grid$class <- predict(rf, grid)

qplot(x, y, data=grid, fill=class, geom="tile") + geom_point(aes(colour=class), data=w) + fill + col

grid$prob <- predict(rf, grid, type="prob")[, 1]

qplot(x, y, data=grid, fill=prob, geom="tile") + scale_fill_gradient2(low="#E41A1C", mid="white", high="#377EB8", midpoint=0.5)

pdf("~/desktop/size-vs-colour.pdf", width=8, height=8)
qplot(x, y, data=grid, size=abs(1-2*prob)) + scale_area() + coord_equal()
qplot(x, y, data=grid, fill=abs(1-2*prob), geom="tile") + scale_fill_gradient(low="white", high="black") + coord_equal()
dev.off()
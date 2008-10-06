# source("~/documents/clusters/clusterfly/load.r")
library(clusterfly)
library(kohonen)

source("../1-data/wine.r")

# wgobi(wine)
vars <- c("dilution", "alcohol", "color", "proline", "flavanoids")
w5 <- as.matrix(winer[, vars])


# Visualising iterations -----------------------------------------------------

#source("~/ggobi/rggobi/r/longitudinal.r")
right <- som_iterate(w5, grid = somgrid(10, 3), radius=c(3, 0), stepsize=1, nsteps=100)
wgobi(right)

distances <- melt(sapply(right, function(x) x$distances))
names(distances) <- c("oid", "step", "value")

# Error: No layers in plot
#ggplot(distances, aes(step, value)) + scale_y_continuous("distance from node")
#ggsave(file="som-dist-individual.pdf", width=8, height=5) 

right_sum <- summary(right)[, -c(7,8)]

ggplot(right_sum, aes(step, dist_mean)) + geom_line(aes(y=value, group = oid), colour=alpha("grey10", 0.2), data=distances) + geom_line(colour="black") + geom_point(aes(colour = radius)) + scale_y_continuous("distance from node") + scale_colour_gradient(.breaks = 1:3)

ggsave(file="som-dist.pdf", width=10, height=4) 

ggplot(right_sum, aes(step, dist_mean))  + geom_line(colour="black") + geom_point(aes(colour = radius)) + scale_y_continuous("distance from node") + scale_colour_gradient(.breaks = 1:3) + ylim(0.03, 0.06)
ggsave(file="som-dist-mean.pdf", width=10, height=3) 

final <- right[[length(right)]]

# m-in-ds vs d-in-ms ---------------------------------------------------------

initial <- right[[1]]

pdf("wine-bad-model.pdf", width = 6, height = 3)
plot(initial)
dev.off()

wgobi(initial)

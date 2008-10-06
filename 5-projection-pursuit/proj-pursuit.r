library(plyr)
source("anneal.r")
source("../1-data/wine.r")
#source("~/Documents/plyr/plyr/load.r") 

# Perform simulated annealing ------------------------------------------------

if (F) {
  winem <- as.matrix(winer[, -1])
  trace <- replicate(20, scagnostics.anneal(winem, 3, max.steps = 40), simplify=FALSE)
  for(i in 1:length(trace)) trace[[i]]$trace <- i
  trace_df <- do.call("rbind", trace)
  best <- trace_df[order(trace_df$index, decreasing=TRUE)[1:4], ]
  save(trace, trace_df, best, file="trace.rda", compress=TRUE)
} else load("trace.rda")

# Plot paths -----------------------------------------------------------------

qplot(step, index, data=trace_df, geom="line", group=trace, ylim=c(0, NA)) + geom_point(data=best, colour="red")
ggsave(file = "pp-index-progression.pdf", width=8, height=4)

# Plot best projections ------------------------------------------------------

plot_proj <- function(i) {
  bproj <- as.data.frame(winem %*% best$proj[[i]])
  qplot(pp1, pp2, data=as.data.frame(bproj), xlab="", ylab="", xlim=c(-3, 3), ylim=c(-3,3), colour=winer$type) + col + opts(legend.position = "none")
}

l_ply(1:4, function(i) ggsave(plot_proj(i), ps("proj-", i, ".pdf"), width=3, height=3))

# Restart from local optima --------------------------------------------------

second <- best[2, ]
trace12 <- subset(trace_df, trace==second$trace)
restart <- replicate(5, scagnostics.anneal(winem, 3, max.steps = 10, start=best$proj[[2]], temp=0.4), simplify=FALSE)
save(restart, file="restart.rda", compress=TRUE)
for(i in 1:length(restart)) restart[[i]]$trace <- i
restart_df <- do.call("rbind", restart)
restart_df$step <- restart_df$step + second$step - 1

qplot(step, index, data=restart_df, geom="line", group=trace) + geom_line(data=trace12, colour="red")
ggsave(file = "pp-restart.pdf", width=8, height=4)

# Projection paths -----------------------------------------------------------

proj_to_df <- function(proj) {
  rownames(proj) <- names(winer[, -1])
  proj_df <- as.data.frame(proj)
  proj_df$id <- rownames(proj_df)
  projm <- melt(proj_df, id="id")
  names(projm) <- c("variable", "axis", "value")
  projm
}
projections <- ddply(trace_df, .(trace, step, try), function(df) cbind(proj_to_df(df$projs[[1]]), index=df$index))

projections$qual <- chop(projections$index)
projections$variable <- factor(projections$variable)

qplot(variable, value, data=projections, facets = axis ~ qual, geom="blank") + geom_line(aes(group=interaction(step, trace)), colour=alpha("black", 0.2))

qplot(reorder(variable, value), value, data=subset(projections, index > 0.3), facets = axis ~ qual, geom="blank") + geom_line(aes(group=interaction(step, trace)))

qplot(index, data=trace_df, geom="histogram", binwidth=0.01)
pd <- cast(projections, ... ~ variable + axis, force)

pdist <- dist(pd[, -(1:5)]) ^ 2
ord <- isoMDS(pdist, k=4)

d2 <- data.frame(ord$points, pd[,1:5])
qplot(X1, X2, data=d2, colour=index)
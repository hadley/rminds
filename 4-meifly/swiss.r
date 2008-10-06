library(ggplot2)
library(rggobi)
library(meifly)

y <- swiss$Fertility
x <- swiss[, -1]
mods <- fitall(y, x, lm)
ggobi(mods, swiss)

# Model level plots
# ------------------------------

#ggopt(grid.fill = "grey95")

model <- summary(mods)
model$df <- factor(model$df)
modelm <- melt(rescaler(model, "range"), id=c("model", "df"))
modelm$variable <- with(modelm, reorder_factor(variable, tapply(value, variable, mean)))

# Coefficient plots
# ------------------------------
mcoef <- coef(mods)

baseline <- geom_hline(colour="grey50")
pcp <- geom_path(aes(group=model), data=mcoef, colour="grey30")

qplot(variable, raw, data=subset(mcoef, raw != 0), xlab="", ylab="Coefficient") + baseline
ggsave(file="coef-raw.pdf", width=6, height=6)
qplot(variable, std, data=subset(mcoef, raw != 0), xlab="", ylab="Standardised coefficient") + baseline
ggsave(file="coef-std.pdf", width=6, height=6)

# Model brushing
# -----------------------------

upag <- subset(mcoef, variable == "Agriculture" & raw > 0)$model 

model <- transform(model, 
  top2 = factor(AIC >= AIC[order(AIC, decreasing=TRUE) [2]]),
  upag = factor(model %in% upag)
)
colour <- scale_colour_manual(values = c("black", "red"))
size <- scale_size_manual(values = c(2, 3))

qplot(df, R2, data = model, xlab="Degrees of freedom", colour=top2, size=top2, ylab=expression(R^2)) + colour + opts(legend.position="none") + size
ggsave(file="coef-models.pdf", width=6, height=6)

mcoef2 <- merge(mcoef, model[, c("model", "top2", "upag")])
qplot(variable, std, data=subset(mcoef2, std != 0), colour=top2,ylab=expression(R^2)) + geom_line(aes(group=model, size=top2), data=mcoef2) + colour + opts(legend.position="none") +  scale_size_manual(values = c(0.3, 1.4))
ggsave(file="coef-coefs.pdf", width=6, height=6)

qplot(df, R2, data = model, xlab="Degrees of freedom", colour=upag, size=upag) + colour + opts(legend.position="none") + size
ggsave(file="coef-models-upag.pdf", width=6, height=6)

qplot(variable, std, data=subset(mcoef2, std != 0), colour=upag) + geom_line(aes(group=model, size=upag), data=mcoef2) + colour + opts(legend.position="none") +  scale_size_manual(values = c(0.3, 1.4))
ggsave(file="coef-coefs-upag.pdf", width=6, height=6)

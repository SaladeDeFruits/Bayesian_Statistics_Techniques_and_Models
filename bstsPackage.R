library(bsts)
data(goog)
train <- as.vector(goog)[1:1372]
test <- as.vector(goog)[1372:1428]
ss <- AddLocalLevel(list(), train)
ss <- AddSeasonal(ss, train, nseasons = 7)
model <- bsts(train, ss, niter=1000)
# ???
pred <- predict(model, horizon=56, burn=100)
plot(pred, ylim=c(250,800), xlim=c(1,1428))
par(new=T)
plot(test, ylim=c(250,800), xlim=c(-1372,56), type='l', lwd=3, col='red', axes=F)
legend('topleft', legend=c('Predicted','Actual'), col=c('blue','red'), lty=1, lwd=3, cex=1.5)
standInd = function(x) {
  out = (x - x[1])/sd(x, na.rm=TRUE)
  return(out)
}

plotInd = function(x, Fmult, ...) {
  x = apply(x, 2, standInd)
  matplot(Fmult, x, type="l", lty=1, ...)
  abline(h=0, lty=3, col="gray")
}
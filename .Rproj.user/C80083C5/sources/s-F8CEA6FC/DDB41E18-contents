# t-distribution function with standard normal distribution
dev.off()
x <- seq(-3, 3, 0.001)
plot(x, dnorm(x), type="l", lwd=2, lty=1, ylab="f(x)", 
     main = "t-distribution with different df and standard normal distribution")
curve(dt(x, 3), lwd=2, lty=2, col="skyblue", add=T)
curve(dt(x, 10), lwd=2, lty=3, col="navy", add=T)
curve(dt(x, 30), lwd=2, lty=4, col="gray50", add=T)

legend(x = "topright", legend = c("Normal", "t(df=3)", "t(df=10)", "t(df=30)"), 
       lty = 1:4, lwd=2, col = c("black", "skyblue", "navy", "gray50"))
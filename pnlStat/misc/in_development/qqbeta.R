#This function produces a qqplot for the beta family
#
# Stephen J. Walsh - PNNL
# October 13, 2010
#
# We follow the simple procedure described by W. Clevelend (1993) in
#  "Visualizing Data". Confidence intervals are built based on the Kolmogorov-
#  Smirnov test, with critical values for a 95% CI taken David Sheskin (2007)
#  "Handbook of Parametric and Non-Parametric Statistical Procedures"


qqbeta <- function(X,theta)
{  
 #data length
 n <- length(X)
 # order data
 x.ord <- sort(X)
 #empirical quantiles
 f <- (1:n - 0.5)/n
 
 # get theortical quantiles
 q <- qbeta(f,shape1 = theta[1],shape2 = theta[2])

 plot(x = q,
      y = x.ord,
      xlab = "Theoretical Quantiles",
      ylab = "Sample Quantiles",
      main = "Beta QQ-plot",
 #     ylim = c(0,1),
 #     xlim = c(0,1)
      )
 abline(0,1,col="blue",lty="dashed")

 #need confidence bands (based on the Kolmogorov-Smirnov statistic)
 
 #first need theoretical CDF
 Fn <- pbeta(q,shape1 = theta[1],shape2 = theta[2])
 #compute KS critical value
 k <- ks.crit(n)
 low <- Fn - k
 low[low < 0]<- 0 
 up <- Fn + k
 up[up > 1] <- 1 
 
 lb.vec <- qbeta(low,shape1 = theta[1],shape2 = theta[2])
 ub.vec <- qbeta(up,shape1 = theta[1],shape2 = theta[2]) 
 
 #get bounds
 lines(x = q,y = lb.vec,col = "red",lty = "dotted")
 lines(x = q,y = ub.vec,col = "red",lty = "dotted")

 legend("bottomright",
        legend = c("Ideal Fit","95% Confidence Bands"),
        col = c("blue","red"),
        lty = c("dashed","dotted"),
        bty = "n")
}                   

#for lack of better way, hard code the KS critical values for a alpha = 0.05
# level test

ks.crit <- function(n)
{
 # n: sample size 
 if(n <= 40)
 {
  n.seq <- 1:40
  crit.vec <- c(0.975,0.842,0.708,0.624,0.563,0.519,0.483,0.454,0.430,0.409,
                0.391,0.375,0.361,0.349,0.338,0.327,0.318,0.309,0.301,0.294,
                0.287,0.281,0.275,0.269,0.264,0.259,0.254,0.250,0.246,0.242,
                0.238,0.234,0.231,0.227,0.224,0.221,0.218,0.215,0.213,0.210)
  k <- crit.vec[n.seq == n]
 }else if (n > 40){

   k <- 1.36/sqrt(n)
 
 }else{
  stop("You must supply a valid sample size!!")
 }
 k
}  

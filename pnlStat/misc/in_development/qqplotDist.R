qqplotDist <- function(X, dist="normal", fit.line=c(0.25,0.75), ...) {

   # Match the distribution request with available distributions:
   dist <- match.arg(tolower(dist), c("normal", "t", "gamma", "chisquare", "exponential", "weibull"))
   d.fun <- switch(dist, normal="qnorm", t="qt", gamma="qgamma", chisquare="qchisq",
                         exponential="qexp", weibull="qweibull")

   # Put the dots in the list
   dots <- as.list(substitute(list(...)))[-1]
   n.dots <- names(dots)

   # if no xlab was specified
   if (!is.element("xlab", n.dots))
     dots$xlab <- paste("Theoretical quantiles of the ",
                        toupper(substr(dist,1,1)),
                        tolower(substr(dist,2,nchar(dist))),
                        " distribution", sep="")
   # if no ylab was specified
   if (!is.element("ylab", n.dots))
     dots$ylab <- paste("Sample Quantiles of '", deparse(substitute(X)), "'", sep="")


   # Make sure parameter arguments that were supplied actually match
   # the args of the quantile function
   d.fun.formals <- formals(d.fun)
   valid.parm.args <- setdiff(names(d.fun.formals), c("p", "lower.tail", "log.p"))

   # If we have dots that match the 'valid.parm.args'
   if (length(parm.matches <- which(n.dots %in% valid.parm.args))) {
      parm.list <- dots[parm.matches]
      dots <- dots[-parm.matches]

      # Replace default parameter arguments with those supplied by the user
      for (parm in names(d.fun.formals)) {
        if (parm %in% names(parm.list))
          d.fun.formals[parm] <- parm.list[parm]
      }
    }

   # Set the graphics parameter
   op <- par(las=1, pty="s")

   # length of the data vector and sort
   n <- length(X)
   X <- sort(X)

   # Calculate the sample and theoretical quantiles used to
   # draw the line.
   y1 <- quantile(X, fit.line)

   # Get the theoretical 1st and 3rd quantiles
   d.fun.formals[["p"]] <- fit.line
   print(d.fun.formals)
   x1 <- do.call(d.fun, d.fun.formals)

   # Calculate line to fit through
   slope <- diff(y1)/diff(x1)
   int <- y1[1] - slope * x1[1]

   # Generate the theoretical quantiles for plotting
   d.fun.formals["p"] <- ppoints(n)
   print(d.fun.formals)
   plot.x <- do.call(d.fun, d.fun.formals)
   plot.y <- X

   pvar(length(plot.x))
   pvar(n)

   # Calculate the goodness of fit "gof"
   # This can sometimes come out negative, since this isnt' a Least-square fit
#   SSE <- sum((plot.y - (int+slope*plot.x))^2)
#   SST <- (length(plot.y)-1) * var(plot.y)
#   gof <- (SST - SSE) / SST

   # For long series, points are removed from the middle of
   # the series to avoid bogging down the plot
   if (n > 300) {
      subset <- c(1:100,floor(seq(101,n-100,len=100)),(n-100):n)
      plot.x <- plot.x[subset]
      plot.y <- plot.y[subset]
   }

   # Make the plot
   plot.formals <- c(list(x=plot.x, y=plot.y, col="Blue"), dots)
   do.call("plot.default", plot.formals)

   # Add in the line
   abline(int, slope)

   par(op)
   
} # end qqplotDist()


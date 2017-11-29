# plotting method for class cusum
plot.cusum <- function(c.obj, title=NULL, ooc.labels=NULL, include.data=FALSE,
                       constant.y.axis=FALSE, margin.title=NULL,
                       ylab.left="Cusum", ylab.right="Data", cex=0.7, ...) {

  if (class(c.obj) != "cusum")
   stop(deparse(substitute(c.obj)), " is not of class 'cusum'\n")

  # if no device is opened, then open the windows device
  if (dev.cur() == 1)
    X11(height=12,width=12)

  # Will return the graphing parameters to their original values
  op <- par()
  on.exit(options(warn=-1))
  on.exit(par(op))
  on.exit(options(warn=0))

  # current device is windows
  if (names(dev.cur()) == "windows")
    par(ask=TRUE)

  # Extract the objects from the list for easier access
  for (obj in names(c.obj))
    assign(obj, c.obj[[obj]])

  # Attempt to extract the dates if possible
  if (!is.null(names(cusum))) {
    if (class(
        xaxis.text <- try(formatDT(as.character(names(cusum)),
                                   date.outformat="yyyy-mm-dd")$date,
                          silent=TRUE)
        ) == "try-error")
      xaxis.text <- names(cusum)
  }
  else
    xaxis.text <- 1:length(cusum)

  # Let the orientation of the axis labels
#  if (max(nchar(xaxis.text)) > 4)
#    par(las=2)
#  else
    par(las=1)
  

  # Set up yaxis limits
  if (constant.y.axis) {
    ylim <- range(cusum, h)
    ylim.2 <- range(data)
  }

  # Define the basic plotting function
  plotS <- function(sub) {

     # Grab the subset of the data to be plotted
     y <- cusum[sub]
     resetCnt <- resetCounter[sub]
     x <- 1:length(sub)

     if (!constant.y.axis) 
       ylim <- range(y, h)
     

     # Plot the data
     plot(x, y, type="n", font.main=1, xlab="", ylab=ylab.left, ylim=ylim,
          main=paste(title, "  Obs = ", min(sub), ":", max(sub), sep=""),
          axes=FALSE, frame=TRUE, ...)

     # Plot the axes
     axis(2, at=round(seq(ylim[1], ylim[2], length=6), 1))
     xaxis.text.sub <- xaxis.text[sub]
     axis(1, at=1:length(sub), labels=xaxis.text.sub)

     # Add in the control limit
     abline(h=h, col="Blue")


     # If data are to be plotted along side...
     if (include.data) {

       y.data <- data[sub]

       if (!constant.y.axis)
         ylim.2 <- range(y.data)

       # Add in the actual data points

       # Function to map the raw data points into
       # the interval occupied by the plotted cusum
       cusum.space <- function(x, xrange, yrange) {
         # x = vector of data points to be plotted
         #     wrt the second (right) vertical axis
         # xrange = range of the data points plotted
         #     wrt to the second (right) vertical axis
         # yrange = range of the data points plotted
         #     wrt to the first (left) vertical axis
         if (length(yrange) != 2)
           stop("length(yrange) must be 2.\n")
         if (length(xrange) != 2)
           stop("length(xrange) must be 2.\n")
         m <- diff(yrange) / diff(xrange)
         b <- yrange[1] - xrange[1] * m
         return(m * x + b)
       } # cusum.space
         
       axis(4, at=round(seq(ylim[1], ylim[2], length=6), 1),
            labels=round(seq(ylim.2[1], ylim.2[2], length=6), 1))

       for (i in unique(resetCnt))
         points(x[resetCnt==i], cusum.space(y.data, ylim.2, ylim)[resetCnt==i],
                type="o", col="Gray", pch=17, cex=1.2*cex, ...)

       # Add in the label for the right vertical axis
       las.setting <- par()$las
       par(las=0)
       mtext(ylab.right, side=4, line=2.25, cex=cex)
       par(las=las.setting)
  
     } # if (include.data)
     

     # Plot each of the Cusum strings
     for (i in unique(resetCnt)) 
       points(x[resetCnt==i], y[resetCnt==i], pch=19, type="o", cex=cex, ...)

     # Plot ooc points in red
     ooc.ind <- y > h

     if (any(ooc.ind)) {

       # Use red points if no labels
       if (is.null(ooc.labels)) 
         points(x[ooc.ind], y[ooc.ind], col="Red", pch=19, cex=cex,  ...)

       # Add in labels of ooc points
       else {
         points(x[ooc.ind], y[ooc.ind], col="White", pch=19, cex=cex,  ...)
         ooc.lab <- ooc.labels[sub]
         text(x[ooc.ind], y[ooc.ind], ooc.lab[ooc.ind], col="Red",
              cex=cex, ...)
       }

     }

  } # end plotS()   


  # Establish the graphing parameters
  par(lab=c(3,5,7), mgp=c(2.75,0.4,0),
      tcl=-0.25, cex.axis=0.9,
      yaxs='r',font.main=1,
      cex.main=1.2)#, mfrow=c(4,1))

  if (!include.data)
    par(mar=c(1.75,3.75,1.5,0.75))
  else
    par(mar=c(1.75,3.75,1.5,3.75))

  if (!is.null(margin.title))
    par(omi=c(0.1,0.1,0.5,0.1))  

  # Determine the optimal graph subset size (gsize)
  # that best divides the data.  This value will minimize
  # the number of plots while making the last plot contain
  # the most possible data.  Each graph will contain between
  # 80 and 100 points apiece.
  n <- length(cusum)
  overlap <- 5
  gsizes <- seq(80,100,by=2)
  t1 <- floor(n/gsizes)
  t2 <- t1==min(t1)
  i <- 1
  while (t2[i]==FALSE)
    i <- i+1
  gsize <- gsizes[i]
  
  # Subset the series into sets of size "gsize" and produce a plot for each subset
  j <- 1
  while ((j * gsize) < (n + gsize)) {

        plotS(max(j,(j-1)*gsize-overlap):min(n,j*gsize))

        if (!is.null(margin.title) & (j%%4 == 1))
           mtext(margin.title,outer=TRUE)

        j <- j+1
  }  

} # plot.cusum


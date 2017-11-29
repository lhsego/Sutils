###  This function creates my histograms
###  Created by Brett Amidan, March 2004

my.pdf.plot <- function(data.vec,group.vec=NULL,group.labels=NULL, nclass=25,
  relative.freq=TRUE,na.rm=T,do.legend=T,leg.cex=.6,...)  {
	###  data.vec is the vector of data
	###  nclass is the number of class bins to use
	###  relative.freq is used if proportions are wanted on the y axis
	###  ... means you can add other plot commands to the pdf plot
  
  ## if no group.vec then create one with all 1's
  if (is.null(group.vec))  {
    group.vec <- rep(1,length(data.vec))
  }
  
  ### if na.rm=T, then remove all NAs
  if (na.rm)  {
    ind.na <- is.na(data.vec)
    data.vec <- data.vec[!ind.na]
    group.vec <- group.vec[!ind.na]
  }
  
  ### set the xlim if NULL
  xlimits <- c(min(data.vec),max(data.vec))
  
	### bin all the data / need the create.bins function
	temp.bins <- create.bins.group(data.vec=data.vec,group.vec=group.vec,
    number.bins=nclass)
  group.counts <- temp.bins[,3:ncol(temp.bins)]
  if (ncol(temp.bins)==3)  {
    ## make it a matrix again
    group.counts <- matrix(group.counts,ncol=1)
  }
  
  ## get midpts of each bin
  midpts <- (temp.bins[,"low.bin"]+temp.bins[,"up.bin"])/2
  ## calculate relative freq if desired
  if (relative.freq) {
    group.counts <- group.counts / matrix(colSums(group.counts),
      nrow=nrow(group.counts),ncol=ncol(group.counts),byrow=TRUE)
  }
  ## calculate the y limits
  ylimits <- c(min(group.counts),max(group.counts))
  
  ### set up the plot
  plot(0,0,type="n",xlim=xlimits,ylim=ylimits,...)
  
  lwd.vec <- seq(3,1,by=-.5)[1:ncol(group.counts)]
  for (i in 1:ncol(group.counts)) {
    ## add each line
    lines(midpts,group.counts[,i],col=(1+i),lwd=lwd.vec[i])
  }

  ## add the legend
  if (do.legend) {
    if (is.null(group.labels)) {
      group.labels <- unique(group.vec)
    }
    legend(xlimits[1],ylimits[2],group.labels,cex=leg.cex,
      lwd=lwd.vec,col=c(2:(1+ncol(group.counts))))
  }
		
	invisible()
}

## This file creates bins to be used in pdf plots

create.bins.group <- function(data.vec,group.vec=group.vec, 
  low.limit=min(data.vec), hi.limit=max(data.vec),number.bins=20)  {
	##  data.vec is a vector of the data to be binned
	##  low.limit is the lowest bin marker
	##  hi.limit is the highest bin marker
	##  number.bins is the number of bins desired

	## create the bins
	temp.bins <- seq(low.limit,hi.limit,length=(number.bins+1))
	low.bins <- temp.bins[1:(number.bins)]
	up.bins <- temp.bins[2:(number.bins+1)]

	## create the bin counts output
	unq.groups <- sort(unique(group.vec))
  bin.totals <- matrix(0,number.bins,length(unq.groups))
  dimnames(bin.totals) <- list(1:nrow(bin.totals),
    paste("group",unq.groups,sep=""))

	## loop thru the bins and add to the bin totals
	## values on the bin markers are added to bin in which it is on lower marker
	## however, on last bin, values on upper marker will be included
	for (i in 1:number.bins) {
		if (i==number.bins) {
			## last bin to include those = to upper marker
			indy <- data.vec >= low.bins[i] & data.vec <= up.bins[i]
			NULL
		}
		else {
			indy <- data.vec >= low.bins[i] & data.vec < up.bins[i]
			NULL
		}
		## bin totals to correct group
		for (j in 1:length(unq.groups))  {
      ind.grp <- group.vec == unq.groups[j]
      bin.totals[i,j] <- bin.totals[i,j]+ sum(indy[ind.grp])
    } #ends j loop
	} # ends i loop

	output <- cbind(low.bins,up.bins,bin.totals)
	dimnames(output) <- list(paste("bin",1:length(low.bins),sep=""),
    c("low.bin","up.bin",dimnames(bin.totals)[[2]]))
	output
}  ## end of function

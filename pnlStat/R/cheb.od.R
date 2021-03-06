# Use Chebyshev's inequality to calculate critical values for outlier detection
# Created by Brett Amidan, April 2001
# Modified by J. Hathaway  Nov 22,2006
# Added na.rm=T to lines 9 and 12.
# Modified by Landon Sego, 2007-01-08
# Streamlined some of the code, added the one-sided upper limit
# and info abou the Vysochanskij-Petunin inequality

# The unimodal Chebyshev inequality is known as the
# Vysochanskij-Petunin inequality.  See:
# http://en.wikipedia.org/wiki/Vysochanskij%E2%80%93Petunin_inequality
# At the web-page, note that lambda must be > sqrt(8/3) which
# implies that rej.val must be < 1/6



##' Calculates the Chebysehev limit for a vector of data
##'
##' Calculates the lower, upper, one-sided upper, and one-sided lower Chebyshev
##' limits for a vector of data.
##'
##' When \code{unimodal = TRUE}, the Vysochanskij-Petunin inequality is used.
##' Otherwise, the traditional Chebyshev inequality is used.
##'
##' @export cheb.od
##' 
##' @param the.data A numeric vector of data
##' 
##' @param unimodal \code{=TRUE} indicates the assumption that the data come
##' from a unimodal distribution
##' 
##' @param rej.val A vector of probability cutoff values
##' 
##' @return A matrix, one row per \code{rej.value}, with the following columns:
##' \item{probabilities}{The probabilities from \code{rej.value}}
##' \item{lower.cv}{The lower Chebyshev limit} \item{upper.cv}{The upper
##' Chebyshev limit} \item{one.sided.upper.cv}{The one-sided upper Chebyshev
##' limit} \item{one.sided.lower.cv}{The one-sided lower Chebyshev limit}
##' 
##' @author Brett Amidan with additions from John Hathaway and Landon Sego
##' 
##' @seealso \code{\link{cheb.2step.od}}
##' 
##' @references For the Chebyshev inequality, see:
##'
##' \url{http://en.wikipedia.org/wiki/Chebyshev\%27s_inequality}
##'
##' The unimodal Chebyshev inequality is known as the Vysochanskij-Petunin
##' inequality.  See:
##'
##' \url{http://en.wikipedia.org/wiki/Vysochanskij\%E2\%80\%93Petunin_inequality}
##'
##' Also see:
##'
##' Amidan BG, Ferryman TA, Cooley SK. 2005. Data Outlier Detection Using the Chebyshev
##' Theorem.  Aerospace Conferece, IEEE. IEEEAC paper #1198. 1-6.
##' \url{http://ieeexplore.ieee.org/iel5/10432/33126/01559688.pdf?arnumber=1559688}
##' 
##' @keywords misc
##' @examples
##'
##'
##' # Unimodal, normal example
##' cheb.od(rnorm(500), rej.val=10^(-2:-5))
##'
##' # Bimodal example
##' cheb.od(c(rnorm(300), rnorm(300, 4, 1)), unimodal=FALSE)
##'
##'
cheb.od <- function(the.data, unimodal=TRUE, rej.val=c(1/100,1/1000,1/10000))  {

        if (any(rej.val >= 1/6))
          warning("The Vysochanskii-Petunin inequality holds only for 'rej.val' < 1/6 = ",
                  round(1/6, 5))

	## calculates needed statistics
	stdev <- sqrt(var(the.data, na.rm=TRUE))  #  J added na.rm=T

	if (is.na(stdev)) {
          output <- NULL  # J added this to account for the chance that
                          # there is only one observation.
          warning("Std. Dev. of data is NA")
        }

        else {
		mean.data <- mean(the.data, na.rm=TRUE) #  J added na.rm=T
		ks <- sqrt(1/rej.val)

		## do if data is unimodal
		if (unimodal)  {
			## find mode / not easy with continuous data

			## use my bining function to find mode
			temp.data <- create.bins(data.vec=the.data,number.bins=21)
			ind <- temp.data[,"bin.counts"]==max(temp.data[,"bin.counts"])
			mode.data <- mean(temp.data[ind,c("low.bin","up.bin")])

			## calculate for both tails
			b.val <- sqrt(stdev^2+(mode.data-mean.data)^2)
			up.crit.vals <- mode.data + ks*b.val*2/3
			low.crit.vals <- mode.data - ks*b.val*2/3
                        # This is a guess...I haven't been able to find a version of the
                        # one-sided Vysochanskii-Petunin inequality --Landon
                        one.sided.upper <- mode.data + sqrt((1-rej.val)/rej.val)*b.val*2/3
                        one.sided.lower <- mode.data - sqrt((1-rej.val)/rej.val)*b.val*2/3

                }
		## if not unimodal, do this
		else {
			## calculate for both tails
			up.crit.vals <- mean.data + ks*stdev
			low.crit.vals <- mean.data - ks*stdev
                        # I know this one is right --Landon
                        one.sided.upper <- mean.data + stdev * sqrt((1-rej.val)/rej.val)
                        one.sided.lower <- mean.data - stdev * sqrt((1-rej.val)/rej.val)
		}
		## Creates output for rej.val's and crit.vals
		output <- cbind(rej.val, low.crit.vals, up.crit.vals, one.sided.upper, one.sided.lower)
		dimnames(output)[[2]] <- c("probabilities","lower.cv","upper.cv","one.sided.upper.cv","one.sided.lower.cv")
	} ## ends if

	output

} # cheb.od

## This file creates bins to be used in histograms

create.bins <- function(data.vec,
                        low.limit = min(data.vec),
                        hi.limit = max(data.vec),
                        number.bins = 20)  {
	##  data.vec is a vector of the data to be binned
	##  low.limit is the lowest bin marker
	##  hi.limit is the highest bin marker
	##  number.bins is the number of bins desired

	## create the bins
	temp.bins <- seq(low.limit,hi.limit,length=(number.bins+1))
	low.bins <- temp.bins[1:(number.bins)]
	up.bins <- temp.bins[2:(number.bins+1)]

	## create the bin counts output
	bin.totals <- rep(0,number.bins)

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
		bin.totals[i] <- bin.totals[i]+ sum(indy)
	}

	output <- cbind(low.bins,up.bins,bin.totals)
	dimnames(output) <- list(paste("bin",1:length(low.bins), sep=""),
                                 c("low.bin","up.bin","bin.counts"))
	output
}  # create.bins

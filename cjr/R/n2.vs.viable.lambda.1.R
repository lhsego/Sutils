# Visualize n2 versus viable lambda
# Demonstrate that looking for the viable lambda when n2=0 produces the largest possible lambda over 'all' n2's
# Also checks the assumption that all lambda > viable lambda are also viable (using minimum.viable.lambda)


##'Verify the algorithm for finding the viable pct.clean
##'
##'Emprically verifies that \code{n2 = 0} produces the highest viable value of
##'\code{lambda} for "all" other values of \code{n2}.  Using
##'\code{\link{minimum.viable.lambda}}, it also checks that \code{lambda}
##'values larger than the identified viable \code{lambda} are also viable.
##'
##'This function is nearly identical to \code{\link{n2.vs.viable.lambda}}
##'except that it does continue testing all \code{n2} in the sequence, even
##'when the \code{n2} vs. \code{lambda} profiles 'flatlines,' whereas
##'\code{\link{n2.vs.viable.lambda}} stops testing after 10 consecutive
##'\code{n2} values that result in the \code{viable.lambda} being equal to the
##'\code{target.lambda}.
##'
##'For this function, if the \code{viable.lambda} that is returned for a
##'particular \code{n2} is equal to the \code{target.lambda}, then
##'\code{\link{minimum.viable.lambda}} is not called for that \code{n2},
##'whereas \code{\link{minimum.viable.lambda}} is always called in
##'\code{\link{n2.vs.viable.lambda}}.
##'
##'This function examines the fundamental assumptions of the algorithm used by
##'\code{\link{find.viable.lambda}} and \code{\link{find.n2}} in order to
##'identify the viable \code{lambda}.  This emperically demonstrates that using
##'\code{\link{find.viable.lambda}} with \code{n2 = 0} results in the largest
##'possible viable \code{lambda} value which is also viable for "all" other
##'\code{n2} values.
##'
##'It examines a suitably long sequence of \code{n2} values with length
##'\code{seq.length}. The viable \code{lambda} is calculated for each of the
##'\code{n2} values, using \code{\link{find.viable.lambda}}. The results of
##'summarized across the \code{n2} sequence.  As increasinlyg larger values of
##'\code{n2} are examined, if the value of the resulting viable \code{lambda}
##'drops below the requested \code{pct.clean} or \code{0.80} (depending on the
##'value of \code{start.low.pct.clean}) more than 10 consecutive times, the
##'larger values of \code{n2} are not considered by breaking the loop over the
##'\code{n2} sequence.
##'
##'This function also calls \code{\link{minimum.viable.lambda}} for each
##'\code{n2} value, with results summarized across the \code{lambda} values
##'higher than the viable \code{lambda} and the \code{n2} sequence.
##'
##' @export
##' 
##'@param conf Desired (nominal) confidence level.
##'@param n1 Number of judgmental samples
##'@param r Ratio of prior expecation between judgmental and random samples
##'@param beta Second shape pararments of the beta prior of the judgmental
##'samples
##'@param pct.clean The nominal pct.clean for the given case.  This will be
##'used as the starting value for \code{lambda} if \code{start.low.pct.clean =
##'FALSE}.
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param decimal.precision The precision of the solution for \code{lambda}
##'(passed to \code{\link{find.viable.lambda}}).
##'@param start.low.pct.clean \code{=TRUE} using a starting value of
##'\code{lambda = 0.80} when searching for the viable \code{lambda}.
##'Otherwise, it uses \code{lambda = pct.clean}.
##'@param show.plot \code{=TRUE} displays a plot of the \code{n2} versus the
##'viable \code{lambda}
##'@param seq.length Integer specifying the length of the \code{n2} sequence
##'that will be tested
##'@param verbose \code{=TRUE} prints all the details of the validation.
##'@param print.deviations \code{=TRUE} prints the full output of
##'\code{\link{minimum.viable.lambda}} if any of the larger \code{lambda}
##'values are not viable.
##'@return A list with the following elements: \item{n2.0.viable}{The
##'\code{use.pct.clean} element returned by \code{\link{find.n2.viable}}.}
##'\item{first.high}{\code{=TRUE} if the viable \code{lambda} required when
##'\code{n2=0} results in the largest viable \code{lambda} over the sequence of
##'\code{n2} values.} \item{all.n2.work}{\code{=TRUE} if the viable
##'\code{lambda} required by \code{n2=0} is also viable for all other \code{n2}
##'in the sequence} \item{non.incr.seq}{\code{=TRUE} if the resulting viable
##'\code{lambda} sequence is a non-increasing function of the \code{n2}
##'sequence, i.e., as \code{n2} increases, the required viable \code{lambda} is
##'decreasing (or the same).} \item{max.vl.diff}{The largest difference in the
##'viable lambda sequence.  This will be 0 or negative for non-increasing
##'sequences.  But it will be positive for a sequence which has an increasing
##'element} \item{pct.not.decreasing}{The percentage of the \code{n2} sequence
##'that does not result in decreasing values of viable \code{lambdas}.}
##'\item{first.index.not.decreasing}{The first index in the \code{n2} sequence
##'where a non-decreasing viable \code{lambda} occurs}
##'\item{num.of.greater.viable.lambda.checked}{The number of \code{n2}'s that
##'were checked using \code{\link{minimum.viable.lambda}}}
##'\item{all.greater.lambda.viable}{This and the following elements in this
##'list contain summaries from calls to \code{minimum.viable.lambda} for each
##'\code{n2}.  This element will be \code{TRUE} if the value \code{all.viable}
##'returned by \code{\link{minimum.viable.lambda}} is \code{TRUE} for all
##'\code{n2}.} \item{pct.greater.lambda.viable}{The percentage of \code{n2}
##'values where the value \code{all.viable} returned by
##'\code{\link{minimum.viable.lambda}} were \code{TRUE}.}
##'\item{first.index.not.greater.lambda.viable}{The first index in the
##'\code{n2} sequence where not all the greater lambdas where viable}
##'\item{summary.pct.glv.min}{The minimum (across the \code{n2} sequence) of
##'the \code{pct.viable} value returned by \code{\link{minimum.viable.lambda}}}
##'\item{summary.pct.glv.q1}{The first quartile (across the \code{n2} sequence)
##'of the \code{pct.viable} value returned by
##'\code{\link{minimum.viable.lambda}}} \item{summary.pct.glv.med}{The median
##'(across the \code{n2} sequence) of the \code{pct.viable} value returned by
##'\code{\link{minimum.viable.lambda}}} \item{summary.pct.glv.mean}{The mean
##'(across the \code{n2} sequence) of the \code{pct.viable} value returned by
##'\code{\link{minimum.viable.lambda}}} \item{summary.pct.glv.q3}{The third
##'quartile (across the \code{n2} sequence) of the \code{pct.viable} value
##'returned by \code{\link{minimum.viable.lambda}}}
##'\item{summary.pct.glv.max}{The maximum (across the \code{n2} sequence) of
##'the \code{pct.viable} value returned by \code{\link{minimum.viable.lambda}}}
##'\item{results}{A data frame with a row for each \code{n2} value that gives
##'the complete (unsummarized) results}
##'@author Landon Sego
##'@seealso \code{\link{find.viable.lambda}}, \code{\link{is.viable.lambda}}
##'@keywords misc
##'@examples
##'
##'n2.vs.viable.lambda.1(0.95, 10, 2, 239, 0.977, 10^6, start.low.pct.clean=TRUE)
##'
n2.vs.viable.lambda.1 <- function(conf, n1, r, beta, pct.clean, maxN, decimal.precision=5,
                                  start.low.pct.clean=FALSE, show.plot=FALSE, seq.length=100, verbose=FALSE,
                                  print.deviations=FALSE) {

  # Pick the starting lambda 
  starting.lambda <- ifelse(start.low.pct.clean, min(0.80, pct.clean), pct.clean)

#  pvar(starting.lambda)
  

  # The largest sample size we expect
  fn2 <- find.n2.viable(conf, n1=n1, N=maxN + 1, r=r, beta=beta, decimal.precision=decimal.precision,
                        pct.clean=starting.lambda, maxN=maxN)
  n2.inf <- fn2$n2

  # Now make that n2 even larger, if n2.inf is small
  n2.max <- round(max(n2.inf, 5000))

  # A set of n2's over which to test the viability--make this linear in the log scale
  n2.seq <- unique(c(0, round(exp(seq(log(1), log(n2.max), length=seq.length-1)))))

  n2.seq.length <- length(n2.seq)

  # The viable lambda sequence
  vl.seq <- c(fn2$use.pct.clean, rep(NA, n2.seq.length - 1))

  # Create the viability vector
  v.vec <- c(TRUE, rep(NA, n2.seq.length - 1))

  # output for minimum.viable.lambda
  m.vec <- m1.vec <- as.logical(rep(NA, n2.seq.length))

  # Count the number of times minimum.viable.lambda is checked
  cnt <- 0

  # iterate over the other n2's
  for (i in 1:n2.seq.length) {

    if (verbose)
      pvar(i, n2.seq[i])

    if (i > 1) {
      
      # Find the viable lambda for that particular n2
      vl.seq[i] <- find.viable.lambda(starting.lambda, conf, n1, n2.seq[i], r, beta, maxN,
                                      decimal.precision = decimal.precision)
  
      # Is the original viable lambda (from n2=0) viable for this n2?
      v.vec[i] <- is.viable.lambda(vl.seq[1], n1, n2.seq[i], r, beta, maxN)$viable
      
    }

    # Also checks the assumption that all lambda > viable lambda are also viable (using minimum.viable.lambda)
    if (vl.seq[i] > starting.lambda) {
      
      mv <- minimum.viable.lambda(conf, n1, n2.seq[i], r, beta, vl.seq[i], maxN, verbose=verbose)
      m.vec[i] <- mv$all.viable
      m1.vec[i] <- mv$pct.viable

      cnt <- cnt + 1

      # Show results if they aren't viable
      if (print.deviations & !mv$all.viable) {
        pvar(n1, r, beta, maxN, i, n2.seq[i], vl.seq[i], v.vec[i])
        print(mv)
      }
      
    }

   
    if (verbose)
      print(mv)

  }  # for (i in 2:length(n2.seq))


  # If the the whole thing is flatline:
  if (all(vl.seq <= starting.lambda)) {

    outList <- list(n2.0.viable = fn2$use.pct.clean,
                    first.high = TRUE,
                    all.n2.work = TRUE,
                    non.incr.seq = TRUE,
                    max.vl.diff = 0,
                    pct.not.decreasing = 0,
                    first.index.not.decreasing = NA,
                    num.of.greater.viable.lambda.checked = cnt,
                    all.greater.lambda.viable = NA,
                    pct.greater.lambda.viable = NA,
                    first.index.not.greater.lambda.viable = NA,                    
                    summary.pct.glv.min = NA,
                    summary.pct.glv.q1 = NA,
                    summary.pct.glv.med = NA,
                    summary.pct.glv.mean = NA,
                    summary.pct.glv.q3 = NA,
                    summary.pct.glv.max = NA,
                    results = NULL)
                    
      if (show.plot) {

        plot(n2.seq, vl.seq, type="l",
             main=paste(pvar(conf, n1, r, beta, maxN, verbose=FALSE, digits=5)),
             xlab=expression(n[2]),
             ylab=ifelse(all(diff(vl.seq) == 0),
                         paste("Viable lambda =", vl.seq[1]),
                         "Viable lambda"))
      }


  }

  else {
        
  
    # Now do the key checks:
  
    # Is the first one (with n2=0) the highest lambda?  
    first.high <- vl.seq[1] == max(vl.seq) 
    
    # Does the first lambda (from n2=0) work for all the other n2's?
    all.n2.work <- all(v.vec)
  
    # Calculate the differences of the viable lambda sequence
    vl.seq.diff <- round(diff(vl.seq), 14) 
    
    # Is it a non-increasing sequence?
    non.incr.seq <- all(vl.seq.diff <= 0)
  
    # What's the largest difference? (to characterize the magnitude of 'increasingness'
    max.vl.diff <- max(vl.seq.diff)
  
    # Make a a data frame to combine the results together
    results <- data.frame(n2=n2.seq, viable.lambda=vl.seq, decreasing=c(TRUE, vl.seq.diff <= 0),
                          all.n2.work=v.vec, all.greater.lambda.viable=m.vec, pct.greater.lambda.viable = m1.vec)
  

    # Remove missing values from m.vec and m1.vec
    m.vec <- m.vec[!is.na(m.vec)]
    m1.vec <- m1.vec[!is.na(m1.vec)]
    
    summary.pct.glv <- summary(m1.vec)
    
    outList <- list(n2.0.viable = fn2$use.pct.clean,
                    first.high = first.high,
                    all.n2.work = all.n2.work,
                    non.incr.seq = non.incr.seq,
                    max.vl.diff = max.vl.diff,
                    pct.not.decreasing = sum(!results$decreasing) / length(results$decreasing),
                    first.index.not.decreasing = which(!results$decreasing)[1],
                    num.of.greater.viable.lambda.checked = cnt,                    
                    all.greater.lambda.viable = all(m.vec),
                    pct.greater.lambda.viable = sum(m.vec) / length(m.vec),
                    first.index.not.greater.lambda.viable = which(!m.vec)[1],
                    summary.pct.glv.min = summary.pct.glv[["Min."]],
                    summary.pct.glv.q1 = summary.pct.glv[["1st Qu."]],
                    summary.pct.glv.med = summary.pct.glv[["Median"]],
                    summary.pct.glv.mean = summary.pct.glv[["Mean"]],
                    summary.pct.glv.q3 = summary.pct.glv[["3rd Qu."]],
                    summary.pct.glv.max = summary.pct.glv[["Max."]],
                    results = results)
  
    if (show.plot) 
  
      plot(n2.seq, vl.seq, type="l",
           main=paste(pvar(conf, n1, r, beta, maxN, verbose=FALSE, digits=5), "\n",
                      pvar(first.high, all.n2.work, non.incr.seq, verbose=FALSE)),
           xlab=expression(n[2]),
           ylab=ifelse(all(diff(vl.seq) == 0),
                       paste("Viable lambda =", vl.seq[1]),
                       "Viable lambda"))
    

  }

  return(outList)

  
} # n2.vs.viable.lambda.1

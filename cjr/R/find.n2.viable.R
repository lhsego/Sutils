# Uses find.n2 to look for an n2 with a viable lambda



##'Finds the required random sample size, ensuring a viable pct.clean is used
##'
##'Finds the required random sample size and automatically ensures a viable
##'pct.clean is used.
##'
##'Calls \code{\link{find.n2}}, and if the requested \code{pct.clean} is
##'viable, then it returns the results.  If it isn't viable,
##'\code{\link{find.n2}} is called a second time using the recommended
##'\code{pct.clean} that was identified by the first call.
##'
##' @export
##' 
##'@param conf A number in (0,1) giving the desired Bayesian confidence
##'@param \dots Arguments to \code{\link{cjrParms}} (except do not pass in a
##'value for \code{arg.check})
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param decimal.precision The level of precision for the viable lambda,
##'passed to \code{\link{find.viable.lambda}}
##'@param round.up \code{=TRUE} rounds the random sample size up to the nearest
##'integer.
##'@param discrete \code{=TRUE} uses \code{\link{calcConf.discrete}} to
##'calculate the confidence when determining the sample size.
##'@return A list with the following components: \item{n2}{The required random
##'sample size} \item{conf.achieved}{The acheived level of confidence}
##'\item{prior.pct.clean}{The expected value of the percentage clean prior to
##'collecting data, which depends the choices for \code{N}, \code{n1},
##'\code{prior.prob} (or \code{beta}), and \code{r}.}
##'\item{requested.pct.clean}{The pct.clean that was requested in the call to
##'\code{find.n2.viable}} \item{use.pct.clean}{The pct.clean that was actually
##'used to calculate the required random sample size.  This will be greater
##'than or equal to \code{requested.pct.clean}.}
##'@author Landon Sego
##'@seealso \code{\link{find.n2}}
##'@keywords misc
##'@examples
##'
##'find.n2.viable(0.95, N=10000, n1=20, r=2, prior.prob=0.01, pct.clean=0.98)
##'
##'find.n2.viable(0.97, n1=13, r=1.5, pct.clean=0.99, beta=35, N=2500, round.up=TRUE)
##'
find.n2.viable <- function(conf, ..., maxN = 10^6, decimal.precision = 5, round.up = FALSE, discrete=FALSE) {

  # Make initial call to find.n2
  out <- find.n2(conf, ..., maxN = maxN, decimal.precision = decimal.precision, round.up = round.up, discrete = discrete)

  # Parsing out the arguments in  ... to a list
  args <- list(...)
  
  # If the requested lambda was not viable, call again
  if (!out$is.viable.request) {

    # Assign the objects in args to objects of their own name so they can be used in the call to find.n2:
    obj.txt <- c("n1", "N", "pct.clean", "prior.prob", "beta", "r")

    # Assign the objects in the list to objects of the same name in the scope of this function
    # If the object is not in the 'args' list, than an NA will be assigned
    for (o in obj.txt)
      assign(o, ifelse(!is.null(args[[o]]), args[[o]], NA))

    # Check assignments
    # pvar(n1, N, pct.clean, prior.prob, beta, r, alpha)

#    pvar(out[["recommended.pct.clean"]],digits=17)

    # Second call
    out1 <- find.n2(conf, n1=n1, N=N, pct.clean=out[["recommended.pct.clean"]], prior.prob=prior.prob,
                    beta = beta, r = r, maxN = maxN, decimal.precision = decimal.precision, round.up = round.up,
                    discrete = discrete)
    
    # Final sanity check
    if (!out1[["is.viable.request"]]) {
      cat("\nFailure in find.n2.viable:  pct.clean was not viable on the second attempt to find the sample size\n",
          "The first call:  find.n2.(",
          pvar(conf, maxN, n1, N, pct.clean, prior.prob, beta, r, round.up, verbose=FALSE),
          ")\n", "Results from first call:\n", sep="")
      print(out)
      cat("Results from second call (to find.n2):\n")
      print(out1)
      stop("Failure 1:  pct.clean was not viable\n")
    }

    out <- out1
    
  }

  return(list(n2 = out[["n2"]],
              conf.achieved = out[["conf.achieved"]],
              prior.pct.clean = out[["prior.pct.clean"]],
              requested.pct.clean = args[["pct.clean"]],
              use.pct.clean = out[["use.pct.clean"]]))
  

} # find.n2.viable

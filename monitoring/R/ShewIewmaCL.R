# Calculate a data frame that can be used to determine the pair of control limits
# for the combined IEWMA and Shewhart charts.

ShewIewmaCL <- function(Y, mu0.hat, lambda,
                        h.e.seq = seq(round(mu0.hat, 1) + 0.1, 25, by = 0.5),
                        h.s.seq = seq(round(mu0.hat, 1) + 0.1, 25, by = 0.5),
                        nrep = 10^5,
                        ncores = 6) {

  # The limits have to be at least as large as mu0.hat
  if (any(h.e.seq <= mu0.hat))
    stop("All values of 'h.e.seq' need to be greater than 'mu0.hat'")
  if (any(h.s.seq <= mu0.hat))
    stop("All values of 'h.s.seq' need to be greater than 'mu0.hat'")   
  
  # Build upper-triangular the data frame
  d.ut <- expand.grid(h.e = h.e.seq, h.s = h.s.seq)
  d.ut <- d.ut[d.ut$h.e < d.ut$h.s,]

  # Function for calculating the ARL
  cARL <- function(x) {
    
   h.s.tmp <- x[1,"h.s"]
   h.e.tmp <- x[1,"h.e"]
   
   out <- ShewIewmaSim(Y, mu0.hat, lambda = lambda, h.e = h.e.tmp, h.s = h.s.tmp,
                       nrep = nrep, ncores = 1)
   
   return(c(list(h.e = h.e.tmp, h.s = h.s.tmp), out))
   
  } # cARL

  # Now calculate the run lengths by parallelizing over the cases
  get.arls <- timeIt(dfplapply(d.ut, cARL, output.df = TRUE, packages = "monitoring",
                               needed.objects = c("Y", "mu0.hat", "lambda", "nrep"),
                               njobs = ncores, random.seed = round(1000 * runif(1)),
                               check.interval.sec = 5))

  return(get.arls)


} # ShewIewmaCL
  

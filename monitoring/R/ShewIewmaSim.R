# Simulate ARL of combined Shewhart IEWMA chart

# Simulates (via Bootstrapping) the ARL of a combined Shewhart and IEWMA
# as discussed by Shu, 2007, "A One-sided EWMA Control Chart for monitoring process means"
# Does not calculate the limit

# X[i] = (1-lambda) * X[i-1] + lambda * max(Y[i], mu_0)
# It initializes with mu_0

# Signal occurs when X[i] > h


ShewIewmaSim <- function(Y, mu0, lambda, h.e, h.s=NULL, nrep = 10^5, ncores = 1, ...) {

  # Y is the vector of historical data, in sequence
  # mu0 is the estimate of the in-control mean
  # h.e is the control limit of the IEWMA
  # h.s is the control limit of the Shewhart chart
  # nreps = is the number of monte carlo reps
  # ncores is the # of parallel jobs to run to speed up the simulation

  # Verify inputs
  if (!is.numeric(Y))
    stop(deparse(substitute(Y)), " must be a numeric vector.\n")
  if (!is.numeric(mu0))
    stop("'mu0' must be a real number")
  if (!is.numeric(lambda))
    stop("'lambda' must be a real number in (0, 1]")
  if ((lambda <= 0) | (lambda > 1))
    stop("'lambda must be in (0, 1]")
  if (!is.numeric(h.e))
    stop("'h.e' must be a real number")
  if (h.e < mu0)
    stop("The control limit 'h.e' must be larger than 'mu0'")
  if (!is.null(h.s)) {
    if (!is.numeric(h.s))
      stop("'h.s' must be a real number")
    if (h.s < mu0)
      stop("The control limit 'h.s' must be larger than 'mu0'")
  }

  
 # Function that calls the compiled C code for simulating the charts
 simFun <- function(nrep) {

   .C(ifelse(is.null(h.s), "IewmaSim", "ShewIewmaSim"),
      as.double(Y),
      as.integer(length(Y)),
      as.double(mu0),
      as.double(lambda),
      as.double(h.e),
      # Note, if the Shewhart limit is NULL, then no Shewhart limit will be applied
      as.double(ifelse(is.null(h.s), -99999, h.s)), 
      nrep = as.integer(nrep),
      srl = double(1),
      ssrl = double(1),
      nEwma = integer(1),
      PACKAGE = "monitoring")[c("nrep","srl","ssrl","nEwma")]
   
  } # simFun

  # Split the reps into sections based on the number of jobs
  nrep.list <- lapply(parseJob(nrep, ncores), length)

  # Now run the simulation in parallel and time the results
  simRes <- timeIt(plapply(nrep.list, simFun, packages = "monitoring",
                           needed.objects = c("Y", "mu0", "lambda", "h.e", "h.s"),
                           njobs = ncores, check.interval.sec = 0.1, ...),
                   units = "sec", return.time = TRUE, verbose = FALSE)
  
  # Now sum the results across the parallel jobs
  sr <- apply(list2df(simRes$out), 2, sum)
  
  # Calculate the summary stats
  return(list(nrep = sr[["nrep"]],
              arl = sr[["srl"]] / sr[["nrep"]],
              se.arl = sqrt((sr[["ssrl"]] - (sr[["srl"]] * sr[["srl"]]) / sr[["nrep"]]) /
                       (sr[["nrep"]] * (sr[["nrep"]] - 1))),
              pct.ewma = sr[["nEwma"]] / sr[["nrep"]],
              time.sec = simRes$elapsed))


} # ShewIewmaSim

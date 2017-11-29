
# Try bounding n so that it doesn't run over the limit...
## sample size
find.n <- function(N, C, lambda, jaech=TRUE) {

  # N = population size
  # C = Confidence
  # lambda = % Acceptable

  # N, C, lambda must all have same length or length 1

   # Current VSP implementation
  if (jaech) {
    u <- pmax(1, (1-lambda)*N)
    return(ceiling(0.5 * (1 - (1-C)^(1/u)) * (2*N - u + 1)))
  }

  # Using the Gamma function smoother
  else {
   
    n.length <- max(length(N), length(C), length(lambda))
    
    if (length(N) == 1)
      N <- rep(N, n.length)
    if (length(C) == 1)
      C <-  rep(C, n.length)
    if (length(lambda) == 1)
      lambda <- rep(lambda, n.length)
  
    n.vec <- rep(NA, n.length)
    
    for (i in 1:n.length) {
  
      new.conf.wrapper <- function(n)
        new.conf(N[i], n, lambda[i], jaech=FALSE) - C[i]
        
      res <- try(ceiling(uniroot(new.conf.wrapper, c(0, lambda[i]*N[i]), tol=1e-8)$root))
  
      if (class(res) == "try-error") {
        n.vec[i] <- NA
        pvar(i, lambda[i], C[i], N[i], new.conf.wrapper(0), new.conf.wrapper(lambda[i]*N[i]))
      }
      else
        n.vec[i] <- res
      
    }
  
    return(n.vec)
  }

} # nsize

# Calculate the confidence using the Jaech approximation.  If it's 100%, also identify
# the Achieved % Acceptable
calc.conf <- function(N, n, lambda) {

  lambda <- round(lambda, 5)
  
  if (n == N) 
    out <- list(conf=1, requested.lambda=lambda, achieved.lambda=1)

  else if (lambda == 1)
    out <- list(conf=round(n/N,5), requested.lambda=1, achieved.lambda=1)

  else {
  
    u <- max(1, (1-lambda)*N)
    
    target <- 5e-06
    
    # First find a discrete cut off for the u value to guide the binary search
    find.first.u <- 0
  
    do.binary.search <- TRUE
    
    # Search discrete values to find a starting value of u
    for (u.val in 1:N) {
      test <- (1 - (2*n)/(2*N - u.val + 1))^u.val
  #    pvar(u.val, test)
      if (test < target) {
        find.first.u <- u.val
        break
      }
      # We haven't found the u.val because the sample size is small
      if (u.val == N)
        do.binary.search <- FALSE
    }
  
    # Do a binary search to find the cutoff lambda
    if (do.binary.search) {

      conf.tmp.obj <- function(u) 
        (1 - (2*n)/(2*N - u + 1))^u - target

      lambda.prime <- round(1 - uniroot(conf.tmp.obj, c(1, find.first.u), tol=1e-10)$root / N, 5)
      
      # Binary search
##       u.low <- find.first.u
##       u.high <- 1
##       i <- 0
##       while((abs(u.low - u.high) > 1e-10) & (i < 1000)) {
##         i <- i + 1
##         test.u <- (u.low + u.high) / 2
##         conf.tmp <- (1 - (2*n)/(2*N - test.u + 1))^test.u
##         if (conf.tmp < target)
##           u.low <- test.u
##         else
##           u.high <- test.u
##       } # while
    
##       u.solve <- (u.low + u.high) / 2
    
##      lambda.prime <- round(1 - u.solve/N, 5)
    }
    # If no binary search, then there is no lambda such that the confidence will be 1, so we need to send this
    # to actually calculate the confidence  
    else
      lambda.prime <- 0
  
    if (n >= lambda*N)
      a.lambda <- round(n/N, 5)
    else
      a.lambda <- 0
  
#    pvar(n, lambda, lambda.prime, a.lambda, digits=5)
  
    if (lambda > lambda.prime) {
      if (n >= lambda*N)
        conf <- 1
      else
        conf <- 1 - (1 - (2*n)/(2*N - u + 1))^u
  
  #    pvar(conf, digits=15)
      out <- list(conf=round(conf,5), requested.lambda=round(lambda,5), achieved.lambda=round(max(a.lambda, lambda),5))
    }
    else {
      out <- list(conf=1, requested.lambda=round(lambda,5), achieved.lambda=round(max(a.lambda, lambda.prime),5))
    }
  
  #  print(unlist(out))

  } # else

  # Calculate using the C code that is virtually identical to that used in VSP
  out2 <- .C("CalcSchillingInv2",
             as.double(100 * (1 - lambda)),
             as.integer(N),
             as.integer(n),
             conf = double(1),
             achieved.lambda = double(1),
             useAchieved = integer(1),
             PACKAGE = "aoz")

  pvar(out2$useAchieved)
  
  out3 <- list(conf = out2$conf, requested.lambda = round(lambda,5), achieved.lambda = out2$achieved.lambda)

  # Compare the results before returning:
  mdiff <- max(abs(unlist(out) - unlist(out3)))
  if (mdiff > 1e-13) {
    cat("C results (on top) do not agree with R results\n")
    pvar(mdiff, digits=13)
    op <- options(digits=17)
    print(unlist(out3))
    print(unlist(out))
    options(op)
  }

  return(out)

} # calc.conf

# Using the hypergeometric mass function
true.conf <- function(N, n, lambda) {

  u <- floor((1-lambda)*N) + 1

  return(1 - dhyper(0, u, N-u, n))

} # true.conf

# Using the hypergeometric mass function
true.n <- function(N, C, lambda) {

  u <- floor((1-lambda)*N) + 1

  # A starting value
  n.upper <- ceiling(0.5*(1 - (1 - C)^(1/u))*(2*N - u + 1))

  n.length <- length(n.upper)
  
  if (length(N) == 1)
    N <- rep(N, n.length)
  if (length(C) == 1)
    C <-  rep(C, n.length)
  if (length(lambda) == 1)
    lambda <- rep(lambda, n.length)
  if (length(u) == 1)
    u <- rep(u, n.length)
  
  n.vec <- conf.vec <- rep(NA, n.length)

  for (i in 1:n.length) {
    n.tmp <- 0:n.upper[i]
    conf.vec <- 1 - dhyper(0, u[i], N[i]-u[i], n.tmp)
    n.vec[i] <- n.tmp[which(conf.vec >= C[i])[1]]
  }


  # Checks to verify the search
  true.conf.lower.n <- true.conf(N, n.vec, lambda)
  true.conf.upper.n <- true.conf(N, n.vec-1, lambda)
  
  if (!(all(true.conf.lower.n < C) & all(true.conf.upper.n >= C))) {

    for (i in 1:n.length) {
      if (!(true.conf.lower.n[i] < C[i]) & (true.conf.upper.n[i] >= C[i])) {
         pvar(i, N[i], C[i], lambda[i], n[i], true.conf.lower.n[i], true.conf.upper.n[i], digits=5)
        if (true.conf.lower.n[i] >= C[i])
          result <- "Oversampled"
        else if (true.conf.upper.n[i] < C[i])
          result <- "Undersampled"
        pvar(i, result)
      } # if
    } # for
  } # if 

  return(n.vec)
  
}  # true.n


# Solving for lambda---
find.lambda <- function(n, N, C, jaech=TRUE) {

  # n = sample size
  # N = population size
  # C = confidence

  if (n > N)
    stop("n must be no greater than N\n")
  
  # Lower bound for C.  This keeps us away from the portion of
  # the Confidence function that is not monotonic
  
  # Upper bound for C
  if (any(C > 0.99999)) {
    warning("Values of C > 0.99999 set to 0.9999\n")
    C[C > 0.99999] <- 0.99999
  }

  eConf <- n/N

  lambda <- 1
  
  if (C > eConf) {

    new.conf.wrapper <- function(lambda) {
      new.conf(N, n, lambda, jaech=jaech) - C
    } # new.conf.wrapper
  
    root.lambda <- try(uniroot(new.conf.wrapper, c(0, (N-1)/N), tol=1e-10)$root, silent=TRUE)
  
    # Binary search
    d.low <- 1 - (N-1)/N
    d.high <- 1
    i <- 0
    while (abs(d.low - d.high) > 1e-10) {
       i <- i + 1
       defective <- (d.low + d.high) / 2
       conf.tmp <- new.conf(N, n, 1 - defective, jaech=jaech)
       if (conf.tmp < C)
         d.low <- defective
       else
         d.high <- defective
    }
  
    lambda <- 1 - (d.low + d.high)/2
  
    # Check binary search against the uniroot solution, if the uniroot didn't fail
    if (class(root.lambda) != "try-error") {
      if (abs(root.lambda - lambda) > 1e-10)
        stop(pvar(abs(root.lambda - lambda)))
    }

  }

  else if (C < eConf) 
    pvar(eConf)

  return(lambda)

} # find.lambda


new.conf <- function(N, n, lambda, jaech=TRUE) {

  len <- max(length(N), length(n), length(lambda))
  
  if (length(N) == 1)
    N <- rep(N, len)
   if (length(lambda) == 1)
    lambda <- rep(lambda, len)
  if (length(n) == 1)
    n <- rep(n, len)  
             
  u <- pmax(1, (1-lambda)*N)
  
  # Identify values that need to have a confidence of 1
  conf.1 <- (n >= lambda*N) | (n == N)

  # Pick a dummy value so as not to hang the lgamma or the Jaech functions
  n[conf.1] <- round(N/2)
  
  if (jaech) 
    result <- 1 - (1 - (2*n)/(2*N - u + 1))^u    
  else
    result <- 1 - exp(lgamma(N-u+1) + lgamma(N-n+1) - lgamma(N-u-n+1) - lgamma(N+1))
    
  # Replace the values with 1 that needed it
  result[conf.1] <- 1

  return(result)
  
} # new.conf

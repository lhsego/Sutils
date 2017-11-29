##### This function is a little odd. It doesn't depend upon N directly and varies as a function
##### of rho and n. It is an increasing function of B w.r.t. rho and n.  


N <- rep(1000,3)
n <- c(15,200,5)
rho <- c(1,0.05,0.025)

obj.fun <- function(B, prob = 0.95){
  
  integrandText <- paste("function(x) { (1 - x)^(B - 1)/beta(1,B) * ",
                         paste(paste("(1 - rho[", 1:length(rho), "] *  x)^n[",
                                     1:length(n), "]", sep = ""),
                               collapse = " * "),
                         "}", sep = "")
  
  integrand <- eval(parse(text = integrandText))

  derivIntegrand <- function(x) {
  
    integrand(x) * (((1 - B) / (1 - x)) + sum((n * rho) / (rho * x - 1))) + 1e-04
  
  }

  # Create cutpoint
  # If B = 1, the derivative is 0 (derivIntegrand is a constant)
  # If B = 2, derivIntegrand is strictly negative and < -0.01 over c(0,1)

  if (derivIntegrand(1 - 1e-10) > -0.01 & B > 2) {
  
    # Find the cutpoint by equating the derivative to -0.01
    cutPoint <- uniroot(derivIntegrand, c(0, 1 - 2.2e-16))$root
  
    # This is the case where the slope never gets to -0.01 
  } else {
  
    cutPoint <- NULL
  
  }

  Integ <- function(fun, lower = 0, upper = 1) {
  
    integrate(fun, lower = lower, upper = upper, rel.tol = 1e-10, subdivisions = 500)[[1]]
  
  }    

  # Calculate the integral
  if (!is.null(cutPoint)) {
  
    val <- sum(Integ(integrand, 0, cutPoint),
                 Integ(integrand, cutPoint, 1))
  
  } else {
  
    val <- Integ(integrand)
  
  }
  
  return(val - prob)
  
}

uniroot(obj.fun, interval = c(1,1e04))$root

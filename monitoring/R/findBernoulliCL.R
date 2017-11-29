## Bernoulli CUSUM

## Estimates the value of h using the corrected diffusion approximation
approx.h <- function(p0,gamma,arl.b0=NULL,arl.g0=NULL,p1a=NULL){

    if (p0*gamma >= 1) stop("p1 >= 1")
  
    if (is.null(arl.b0) & is.null(arl.g0)) stop("\nNeed a value for arl.b0 or arl.g0.\n")
    if (!is.null(arl.b0) & !is.null(arl.g0)) stop("\nNeed either arl.b0 or arl.g0, not both.\n")
 
    if (is.null(arl.g0))
       if (arl.b0 < 1/p0)
          stop("\narl.b0 = ",arl.b0," < 1 * (1/p0), need larger arl.b0.\n")
    
    if (is.null(arl.b0)) arl.b0 <- arl.g0 / p0
  

    if (is.null(p1a)) {
       findpa <- Bernoulli.find.pa(p0,gamma)
       m <- findpa$m
       p1.a <- findpa$p1.a
    }
    else {
      m <- p1a$m
      p1.a <- p1a$p1.a
    }
    
    r1 <- -log((1-p1.a)/(1-p0))
    r2 <- log( (p1.a*(1-p0)) / (p0*(1-p1.a)) )

    f <- function(hstar) 
        (( exp(hstar*r2) - hstar*r2 - 1 ) / abs(r2*p0 - r1)) - arl.b0

    hstar <- uniroot(f,c(0.5,20))$root

#    cat("hstar =",hstar,"\n")

    ep <- function(p) {
      if (p < 0.01)
        return( (1/3)*(sqrt((1-p)/p) - sqrt(p/(1-p))) )
      else
        return( 0.410 - 0.0842*log(p) - 0.0391*(log(p))^3 - 0.00376*(log(p))^4 - 0.000008*(log(p))^7 )
    }
    
    h <- round((hstar - ep(p0)*sqrt(p0*(1-p0)))*m,0) / m

    ## Product of m and h should be an integer
    if ((m*h)%%1) 
       cat("\nWarning in approx.h:  m*h =",m*h,"should be an integer\n")
       
   
#    cat("p1.a =",p1.a,"  m =",m,"  estimated h =",h,"\n")
#    cat("h*m =",h*m,"\n")

    return(list(p1.a=p1.a,h=h,m=m))
}


# Using the results of approx.h, finds the best value of h for the requested ARL0
findBernoulliCL <- function(p0,gamma,arl.b0=NULL,arl.g0=NULL,head.start.state=NULL,verbose=FALSE,...) {

  if (p0*gamma >= 1) stop("p1 >= 1")
 
  if (is.null(arl.b0) & is.null(arl.g0)) stop("\nNeed a value for arl.b0 or arl.g0.\n")
  if (!is.null(arl.b0) & !is.null(arl.g0)) stop("\nNeed either arl.b0 or arl.g0, not both.\n")

  
  if (is.null(arl.g0))
    if (arl.b0 < 1/p0)
       stop("\narl.b0 = ",arl.b0," < 1 * (1/p0), need larger arl.b0.\n")
  
  if (is.null(arl.b0)) arl.b0 <- arl.g0 / p0
  
  h.start <- approx.h(p0, gamma, arl.b0=arl.b0, ...)

  if (!is.null(head.start.state))
   firstState <- max(1, head.start.state)
  else
   firstState <- 1
  
  find.arl.b0 <- Bernoulli.linear.ARL(h.start$m, h.start$h, p0)[firstState]

  if (verbose) cat("Initial: find.arl.b0 = ",find.arl.b0," ns =",
                   h.start$h*h.start$m," h =",h.start$h,"\n")

  count <- 0
  
  if (find.arl.b0 > arl.b0) {

      ns <- round(h.start$m * h.start$h , 0)

      # stops on first step that is strictly below arl.b0
      while ((find.arl.b0 >= arl.b0) & (ns > 1)) {
         ns <- ns - 1
         find.arl.b0 <- Bernoulli.linear.ARL(h.start$m, ns / h.start$m, p0)[firstState]
         if (verbose) cat("Overshoot: find.arl.b0 = ",find.arl.b0,
                          " ns =",ns," h =",ns/h.start$m,"\n")
         count <- count + 1
      }

      # Go up 1 step and recalculate
      ns <- ns + 1
      find.arl.b0 <- Bernoulli.linear.ARL(h.start$m, ns / h.start$m, p0)[firstState]

      if (count > 10) cat("Note: ", count, "calls to Bernoulli.linear.ARL() were required to find h.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = h.start$m,
                  h = ns / h.start$m,
                  p1.a = h.start$p1.a))
  }

  else if (find.arl.b0 < arl.b0) {

      ns <- round(h.start$m * h.start$h , 0)

      # stops when we are at or above arl.b0
      while (find.arl.b0 < arl.b0) {
         ns <- ns + 1
         find.arl.b0 <- Bernoulli.linear.ARL(h.start$m, ns / h.start$m, p0)[firstState]
         if (verbose) cat("Undershoot: find.arl.b0 = ",find.arl.b0,
                          " ns =",ns," h =",ns/h.start$m,"\n")
         count <- count + 1         
      }

      if (count > 10) cat("Note: ", count, "calls to Bernoulli.linear.ARL() were required to find h.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = h.start$m,
                  h = ns / h.start$m,
                  p1.a = h.start$p1.a))

  }

  # We hit ARL0 exactly with corrected diffusion
  else {

      if (verbose) cat("Exact hit.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = h.start$m,
                  h = h.start$h,
                  p1.a = h.start$p1.a))
  }

} # end find.h  
  

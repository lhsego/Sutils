# Using a given value of m and a target value of h, finds the actual h
# and the resulting arl's
findBernoulliCL.simple <- function(p0, m, h.start, arl.b0=NULL, arl.g0=NULL,
                                   head.start.state=NULL, verbose=FALSE) {

  if (is.null(arl.b0) & is.null(arl.g0)) stop("\nNeed a value for arl.b0 or arl.g0.\n")
  if (!is.null(arl.b0) & !is.null(arl.g0)) stop("\nNeed either arl.b0 or arl.g0, not both.\n")

  if (is.null(arl.g0))
    if (arl.b0 < 1/p0)
       stop("\narl.b0 = ",arl.b0," < 1 * (1/p0), need larger arl.b0.\n")
  
  if (is.null(arl.b0)) arl.b0 <- arl.g0 / p0

  if (!is.null(head.start.state))
   firstState <- max(1, head.start.state)
  else
   firstState <- 1

  find.arl.b0 <- Bernoulli.linear.ARL(m, round(m * h.start , 0) / m, p0)[firstState]

  if (verbose) cat("Initial: find.arl.b0 = ",find.arl.b0," ns =",
                   h.start*m," h =",h.start,"\n")

  count <- 0
  
  if (find.arl.b0 > arl.b0) {

      ns <- round(m * h.start , 0)

      # stops on first step that is strictly below arl.b0
      while ((find.arl.b0 >= arl.b0) & (ns > 1)) {
         ns <- ns - 1
         find.arl.b0 <- Bernoulli.linear.ARL(m, ns / m, p0)[firstState]
         if (verbose) cat("Overshoot: find.arl.b0 = ",find.arl.b0,
                          " ns =",ns," h =",ns/m,"\n")
         count <- count + 1
      }

      # Go up 1 step and recalculate
      ns <- ns + 1
      find.arl.b0 <- Bernoulli.linear.ARL(m, ns / m, p0)[firstState]

      if (verbose & (count > 20)) cat("Note: ", count,
                                      "calls to Bernoulli.linear.ARL() were required to find h.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = m,
                  h = ns / m,
                  ns = ns))
  }

  else if (find.arl.b0 < arl.b0) {

      ns <- round(m * h.start , 0)

      # stops when we are at or above arl.b0
      while (find.arl.b0 < arl.b0) {
         ns <- ns + 1
         find.arl.b0 <- Bernoulli.linear.ARL(m, ns / m, p0)[firstState]
         if (verbose) cat("Undershoot: find.arl.b0 = ",find.arl.b0,
                          " ns =",ns," h =",ns/m,"\n")
         count <- count + 1         
      }

      if (verbose & (count > 20)) cat("Note: ", count,
                                      "calls to Bernoulli.linear.ARL() were required to find h.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = m,
                  h = ns / m,
                  ns = ns))

  }

  # We hit ARL0 exactly 
  else {

      if (verbose) cat("Exact hit.\n")
      
      return(list(arl.b0 = find.arl.b0,
                  arl.g0 = find.arl.b0 * p0,
                  m = m,
                  h = h.start,
                  ns = round(m * h.start)))
  }

} # end find.h  
  

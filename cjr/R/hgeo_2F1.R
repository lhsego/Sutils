hgeo_2F1 <- function(a,b,d,z, method = c('recurrence', 'quadrature'), eps = 1e-10){
  
  method <- match.arg(method)
  
  if(method == 'recurrence'){
    # term j
    S_j <- 0
    C_j <- 0
    
    # term j-1
    S_j_1 <- 0
    C_j_1 <- 0
    
    # term j-2
    S_j_2 <- 0
    C_j_2 <- 0
    
    j <- 0
    flag <- TRUE
    
    while(flag){
      
      if(j == 0){
        
        C_j_1 <- 1
        
        S_j_1 <- 1
        
        C_j <- (a*b*z)/d
        
        S_j <- S_j_1 + C_j
        
      } else if (j == 1){
        
        C_j_2 <- 1
        
        S_j_2 <- 1
        
        C_j_1 <- (a*b*z)/d
        
        S_j_1 <- S_j_1 + C_j
        
        C_j <- C_j_1*((a+j)*(b+j)/(d+j))*(z/(j+1))
        
        S_j <- S_j_1 + C_j 
        
      } else {
        
        C_j_2 <- C_j_1
        
        S_j_2 <- S_j_1
        
        C_j_1 <- C_j
        
        S_j_1 <- S_j
        
        C_j <- C_j_1*((a+j)*(b+j)/(d+j))*(z/(j+1))
        
        S_j <- S_j_1 + C_j
        
      }
      
      if(j >= 1){
        
        f1 <- abs(C_j)/abs(S_j)
        
        f2 <- abs(C_j_1)/abs(S_j_1)
        
        f3 <- abs(C_j_2)/abs(S_j_2)
        
        if(f1 < eps && f2 < eps && f3 < eps){
          flag <- FALSE
        }
      }
      
      j <- j + 1
      
    }
    
    out <- S_j
    
  } else {
    
    integrand <- function(x){
      if(b <= 0 && a >= 1){
        # swap a and b (permissible by power series representation)
        1/beta(a, d-a)*(x^(a-1)*(1-x)^(d-a-1)*(1-z*x)^(-b))
      } else {
        1/beta(b, d-b)*(x^(b-1)*(1-x)^(d-b-1)*(1-z*x)^(-a))
      }
    }
    
    integral <- try(integrate(integrand, lower = 0, upper = 1, rel.tol = eps)[[1]])
    
    if(inherits(integral, 'try-error')){
      out <- NA
    } else {
      out <- integral
    }
    
  } 
  
  return(out)
  
}
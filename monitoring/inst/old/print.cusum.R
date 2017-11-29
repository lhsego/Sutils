# printing method for class cusum
print.cusum <- function(c.obj) {

  if (class(c.obj) != "cusum")
    stop(deparse(substitute(c.obj)), " is not of class 'cusum'\n")

  cat(" Reference Value (k):                            ", round(c.obj$k, 4), "\n",
      "Control Limit (h):                              ", round(c.obj$h, 4), "\n",
      "Achieved in-control ARL (arl.0.a):              ", round(c.obj$arl.0.a, 2), "\n",
      "In-control estimate of alpha (shape parameter): ", round(c.obj$alpha.0, 4), "\n",
      "In-control estimate of beta (scale parameter):  ", round(c.obj$beta.0, 4), "\n")
  
} # print.cusum



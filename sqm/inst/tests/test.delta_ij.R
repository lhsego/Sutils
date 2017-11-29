test.delta_ij <- function(truth, ncol = length(unique(truth))){
  # Essentially, need to create an indicator matrix such that if signature j is the true signature,
  # then test.delta_ij[j,] = c(0,j,0, ..., 0) = c(0,1,0, ..., 0)
  dmat <- list2df(lapply(truth, function(x = truth){dum <- rep(0, length = ncol);
                                    dum[x] <- 1;
                                    dum}))
  colnames(dmat) <- NULL
  attributes(dmat) <- attributes(dmat)[1]
  return(as.matrix(dmat))
  
}

# Test the function against its "true" counterpart
  data(exampleScoredata)
  esd <- exampleScoreData
  a <- test.delta_ij(esd$truthClass)
  b <- delta_ij(esd$truthClass)
  if(!identical(a,b))
    stop("The output produced by test.delta_ij does not identically match delta_ij")

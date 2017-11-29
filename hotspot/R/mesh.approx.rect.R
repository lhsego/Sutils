# Experimenting with grid calculations for squares (instead of analytic)
# Initial checks look good.  Would be good to see if it matches with
# a plot.

makeRectMesh <- function(rho, meshSize=200) {

  # meshSize is the number of points in the short direction of the rectangle
  # rho is the ratio of long side to short side (long / short)

  # Generates a massive matrix, where each of the 16 sample locations
  # is paired with each of the (meshSize * round(meshSize*rho)) test points.  Then the distance
  # between the sample locations and the test points is calculated.
  # Then a point label is assigned to each of the mesh points, indicating
  # the number of sample points that are within 'r' units of it (which
  # corresponds to the number of times that hotspot would be sampled if
  # it were centered at that particular mesh point.

  # Check rho
  if (rho < 1)
    stop("'rho' should be >= 1.\n")

  # meshSize must be a postive integer
  if ( (meshSize%%1 != 0) | (meshSize <= 0) )
    stop("'meshSize' must be a postive integer.\n")
  
  # Each mesh point is repeated 16 times, once for each sample point                        
  X <- expand.grid(x = seq(0,rho,length=round(meshSize*rho)),
                   y = seq(0,1,length=meshSize),
                   x.pt = (-1:2) * rho,
                   y.pt = -1:2)
  
  # Calculate the distance between each mesh point and each sample point
  X$dist <- sqrt((X$x - X$x.pt)^2 + (X$y - X$y.pt)^2)
  
  # A unique identifier for each mesh point
  X$pt.label <- as.factor(floor(round(meshSize*rho) * X$x) +
                      floor(meshSize * X$y) / 10^(floor(log10(meshSize)) + 1))

  # The number of mesh points
  nmesh <- meshSize * round(meshSize*rho)
  
  # A quick check
  if (nlevels(X$pt.label) != nmesh)
    stop("Number of point labels is not equal to number of mesh points.\n")

  invisible(list(X=X[,c("dist","pt.label")], rho=rho, meshSize=meshSize, nmesh=nmesh))
  
} # end makeRectMesh()


pdetectRect.a <- function(R, s, rho, fn, meshList=NULL, verbose=FALSE, ...) {
  

  # R = radius of hotspot
  # s = length of short side of rectangle (height)
  # rho * s = length of long side of rectangle (width)
  # fn = false negative rate, which must satisfy 0 <= fn <= 1, can be a vector
  # meshList is the output returned from makeRectMesh().  If it's null,
  #   makeRectMesh() will be called inside the function.
  # ... = additional arguments to makeRectMesh() (the meshSize argument).

  # Check rho
  if (rho < 1) stop("'rho' should be >= 1.\n")

  # Check fn
  if ((fn < 0) | (fn > 1))
    stop("'fn' must be in [0,1].\n")

  r <- R/s

  # Check r
  if (r < 0)
    stop("'R' and 's' must both be positive.\n")

  else if (r >= 2)
    stop("'R/s must be less than 2.\n")

  # Compute pdetect exactly
#  else if (r <= 1)
#    out <- pdetectRect(R, s, rho, fn)

  # Compute the pdetect using the mesh algorithm
  else {

    if (is.null(meshList)) 
      meshList <- makeRectMesh(rho, ...)

    # Check that rho from input matches rho in the mesh
    else {

      if (meshList$rho != rho) {
        cat("Warning:  meshList$rho =", meshList$rho,
            "is not equal to rho =", rho,
            "\nmeshList$rho will be used.\n")
        rho <- meshList$rho
      }
      

    } # else check that rho...

    # Is the evaluation point inside the radius of the sample point?
    hit <- (meshList$X$dist < r)

    # Tabulate results for each evaulation point
    sum.hits <- as.data.frame(table(tapply(hit, meshList$X$pt.label, FUN = sum),
                                    dnn="hits"))
    sum.hits$hits <- as.numeric(as.character(sum.hits$hits))
  
    # Some checks 
    if (!all((sum.hits$hits %% 1) == 0)) {
      cat("Not all hit counts are integers:\n")
      print(sum.hits)
      stop()
    }
  
    if ( sum(sum.hits$Freq) != meshList$nmesh ) {
      cat("Sum of counts is not mesh * round(mesh*rho).\n")
      print(sum.hits)
      stop()
    }

    if (verbose)
      print(sum.hits)

    out <- sum(sum.hits$Freq * (1 - fn^sum.hits$hits)) / meshList$nmesh
    
  } # else compute the pdetect using the mesh
  
  return(out)

} # end pdetectRect.a()

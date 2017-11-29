// Calculates a IEWMA as discussed by Shu, 2007, "A One-sided EWMA Control Chart for monitoring process means"
// Does not calculate the limit

// X[i] = (1-lambda) * X[i-1] + lambda * max(Y[i], mu_0)
// It initializes with mu_0

// Signal occurs when X[i] > h

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void calcIEWMA(double *Y,     // Data vector
               int *nY,       // Length of Y
               double *mu_0,  // Estimate of the in-control mean
               double *lambda,// Weight of the current observation in the EWMA
               double *h,     // Control limit
               int *reset,    // Boolean--reset after signal?
               double *X)     // Output of IEWMA

{

  int i;

  // Initialize the EWMA
  double  Xprev = *mu_0, maxY;

  for (i=0; i < *nY; i++) {

    // If reset
    if (*reset) {
      if (Xprev > *h)
        Xprev = *mu_0;
    }

    // Calculate the maxY
    if (Y[i] < *mu_0)
      maxY = *mu_0;
    else
      maxY = Y[i];

    // Calculate the EWMA
    X[i] = (1 - *lambda) * Xprev + *lambda * maxY;

    // Reset Xprev
    Xprev = X[i];

  
  } // for

} // void calcIEWMA

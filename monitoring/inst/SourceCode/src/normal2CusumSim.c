#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulates the initial state ARL of the Standardized Normal 2-sided CUSUM

void normal2CusumSim(int *reps,
                     double *k,
                     double *h,
                     double *mu,
                     double *sigma,
                     int *arl)
     
{

  int i, rl, signal;
  double X, cusumU, cusumL;

  GetRNGstate();

  for (i=0; i < *reps; i++) {

    // Initialize variables
    rl = 0;
    signal = 0;
    cusumU = 0;
    cusumL = 0;

    while (!signal) {

      X = rnorm(*mu, *sigma);
 
      // Calculate the upper CUSUM
      cusumU += X - *k;
      cusumL += X + *k;
  
      // Bound upper CUSUM below at 0
      if (cusumU < 0)
        cusumU = 0;

      // Bound lower CUSUM above at 0
      if (cusumL > 0)
        cusumL = 0;

      rl++;
  
      if (rl >= 2147483647) {
        Rprintf("Warning in C method 'normal2CusumSim'.  Run length of ");
        Rprintf("%i has exceeded 2^31 - 1.\nIteration %i truncated.\n", rl, i+1);
        break;
      }

      // Check for signal
      if ((cusumU > *h) | (cusumL < (-1 * *h)))
        signal = 1;

    } // while

    arl[i] = rl;

  } //  for (i=0; i < *reps ; i++) 

  PutRNGstate();

} // void normal2CusumSim

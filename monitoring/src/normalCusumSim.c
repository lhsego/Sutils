#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulates the initial state ARL of the Standardized Normal 2-sided CUSUM

void normalCusumSim(int *reps,
                    double *k,
                    double *h,
                    double *mu,
                    double *sigma,
                    int *arl)
     
{

  int i, rl;
  double cusum, X;

  GetRNGstate();

  for (i=0; i < *reps; i++) {

    // Initialize variables
    rl = 0;
    cusum = 0;

    while (cusum <= *h) {

      X = rnorm(*mu, *sigma);
 
      // Calculate the upper CUSUM
      cusum += X - *k;
  
      // Bound below at 0
      if (cusum < 0)
        cusum = 0;

      rl++;
  
      if (rl >= 2147483647) {
        Rprintf("Warning in C method 'normalCusumSim'.  Run length of ");
        Rprintf("%i has exceeded 2^31 - 1.\nIteration %i truncated.\n", rl, i+1);
        break;
      }

    } // while

    arl[i] = rl;

  } //  for (i=0; i < *reps ; i++) 

  PutRNGstate();

} // void normalCusumSim

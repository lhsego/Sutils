#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulates the initial state ARL of the GAMMA cusum

void gammaCusumSim(int *reps,
                   double *alpha,
                   double *beta,
                   double *k,
                   double *h,
                   double *initial,
                   int *arl)
     
{

  int i, rl;
  double cusum;

  GetRNGstate();

  for (i=0; i < *reps; i++) {

    rl = 0;
    cusum = *initial;

    while (cusum <= *h) {

      double X = rgamma(*alpha, *beta);
 
      // Calculate the CUSUM
      cusum = cusum + X - *k;
  
      // Bound below at 0
      if (cusum < 0)
        cusum = 0;

      rl++;
  
      if (rl >= 2000000000) {
        Rprintf("Warning in C method 'gammaCusumSim'.  Run length of ");
        Rprintf("%i has exceeded 2*10^9.\nIteration %i truncated.\n", rl, i+1);
        break;
      }

    } // while

    arl[i] = rl;

  } //  for (i=0; i < *reps ; i++) 

  PutRNGstate();

} // void ssCusum2

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulates the states, not the CUSUM statistic, 
// Simulates Initial state ARL using a Geometric

void BernoulliInitialCusumSim(int *reps,
                              double *p,
                              int *m,
                              int *ns,
                              int *start,
                              int *verbose,
                              int *arl,
                              int *success)

{
  int i, state, n, G1;

  GetRNGstate();

  //  Transient Markov states are labeled 1, 2, ..., *ns-1, *ns

  for (i=0; i < *reps; i++) {

    state = *start;
    n = 0;

    // Simulate out of control geometrics
    while (state <= *ns) {

      G1 = rgeom(*p);

      if ((G1 >= 2000000000) | (G1 < 0)) {
         Rprintf("Error in C method 'BernoulliInitialCusumSim'.  Geometric RV G1 = ");
         Rprintf("%i has exceeded 2*10^9. Simulation terminated.\n",G1);
         Rprintf("Itn: %i \tG1: %i \tRun Length: %i \tState: %i\n",
                 i+1,G1,n,state);
         *success = 0;
         break;
      }
      
      state = state - G1;
      if (state < 1) state = 1;
      state = state + *m - 1;
      n = n + G1 + 1;

      if ((n >= 2000000000) | (n <= 0)) {
         Rprintf("Error in C method 'BernoulliInitialCusumSim'.  Run length ");
         Rprintf("%i has exceeded 2*10^9. Simulation terminated.\n",n);
         Rprintf("Itn: %i \tG1: %i \tRun Length: %i \tState: %i\n",
                 i+1,G1,n,state);
         *success = 0;
         break;
      }
        
      if (*verbose) 
         Rprintf("Itn: %i \tG1: %i \tRun Length: %i \tState: %i\n",
                 i+1, G1, n, state);

     } // while (state <= *ns)

     // break the for loop
     if (!*success) break;

     arl[i] = n;

  } //  for (i=0; i < *reps ; i++) 

  PutRNGstate();

} // void  BernoulliInitialCusumSim

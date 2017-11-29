#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Simulates the states, not the CUSUM statistic,
// Simulates SS ARL using a Geometric

void BernoulliSSCusumSim(int *reps,
                         double *p0,
                         double *p1,
                         int *m,
                         int *ns,
                         int *shiftTime,
                         int *verbose,
                         int *ssrl,
                         int *nFalse,
                         int *success)

{
  int falseAlarm;
  int i, state, n, G0, G1, stateOld, nfalse=0;

  GetRNGstate();

  for (i=0; i < *reps; i++) {

    falseAlarm = 1;

    while (falseAlarm) {

      falseAlarm = 0;
      state = 1;
      n = 0;

      while (n <= *shiftTime) {

        stateOld = state;

        G0 = rgeom(*p0);

        if ((G0 >= 2000000000) | (G0 < 0)) {
           Rprintf("Error in C method 'BernoulliSSCusumSim'.  Geometric RV G0 = ");
           Rprintf("%i has exceeded 2*10^9. Simulation terminated.\n",G0);
           Rprintf("Itn: %i \tG0: %i \tRun Length: %i \tState: %i\n",
                   i+1,G0,n,state);
           *success = 0;
           break;
        }

        state = state - G0;
        if (state < 1) state = 1;
        state = state + *m - 1;
        n = n + G0 + 1;

        if ((n >= 2000000000) | (n <= 0)) {
           Rprintf("Error in C method 'BernoulliSSCusumSim'.  Run length ");
           Rprintf("%i has exceeded 2*10^9. Simulation terminated.\n",n);
           Rprintf("Itn: %i \tG0: %i \tRun Length: %i \tState: %i\n",
                   i+1,G0,n,state);
           *success = 0;
           break;
        }

        if (*verbose)
           Rprintf("Itn: %i \tG0: %i \tRun Length: %i \tState: %i\n",
                   i+1, G0, n, state);

        // If false alarm
        if ((state > *ns) & (n <= *shiftTime)) {
           falseAlarm = 1;
           nfalse++;
           if (*verbose) Rprintf("Geometric false alarm.\n");
           break;
	}

      } // while (n <= *shiftTime)

      // If we have a failure then break the falseAlarm loop
      if (!*success) break;

    } // if (falseAlarm)

    // If we have a failure then break the for loop
    if (!*success) break;

    // Subtract off the geometric R.V. that exceeded the shift time
    n = n - G0 - 1;

    // Get into the state just before the shift takes place
    state = stateOld + n - *shiftTime;
    if (state < 1) state = 1;

    // Check the state and n, if bad, break the for loop
    if ((n > *shiftTime) | (state < 1) | (state > *ns)) {
       Rprintf("After geometric obs, n = %i or state = %i is incorrect.\n", n, state);
       *success = 0;
       break;
    }

    // reset the arl counter
    n = 0;

    // print where we're at
    if (*verbose) Rprintf("State just prior to shift:  n = %i, state = %i \nShift introduced here.\n", n, state);

    // Simulate out of control geometrics
    while (state <= *ns) {

      G1 = rgeom(*p1);

      if ((G1 >= 2000000000) | (G1 < 0)) {
         Rprintf("Error in C method 'BernoulliSSCusumSim'.  Geometric RV G1 = ");
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
         Rprintf("Error in C method 'BernoulliSSCusumSim'.  Run length ");
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

     ssrl[i] = n;

  } //  for (i=0; i < *reps ; i++)

  PutRNGstate();

  *nFalse = nfalse;

} // void BernoulliSSCusumSim

// Simulate ARL of combined Shewhart IEWMA chart

// It simulates the ARL by randomly selecting a starting index in the historical, in-control sequence and then running the chart
// until a signal occurs.  If the chart runs past the end of the series, it 'wraps' to the beginning.  After the signal, another
// starting index is randomly chosen (with replacement) from the in-control sequence.  This bootstrapping approach bears some 
// to block resampling and the stationary boostrap, especially since the distribution of the ARL is geometric.  See 
// Chernik, M.R. 1999. Bootstrap Methods:  A Practitioner's Guide. Wiley & Sons.  pp 95-96.

// After each index is selected, the ARL that results from that particular index is recorded in an array and
// that ARL is extracted from the array if that starting index is randomly selected again (to avoid re-simulating the same
// outcome).  This speeds up simulation considerably.

// Calculates a IEWMA as discussed by Shu, 2007, "A One-sided EWMA Control Chart for monitoring process means"
// Does not calculate the limit

// X[i] = (1-lambda) * X[i-1] + lambda * max(Y[i], mu_0)
// It initializes with mu_0

// For the combined Shewhart and IEWMA chart, the signal occurs when X[i] > h.e or Y[i] > h.s

// Landon Sego, 2011-08-06

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// D_ for double, I_ for integer
// big D, I are pointers
// little d, i are actually defined variables (not pointers)

// A joint Shewhart and IEWMA control chart

void ShewIewmaSim(double *D_Y,         // Vector of in-control distribution of training data
                  int    *I_nY,        // Length of Y
                  double *D_mu_0,      // Estimate of the in-control mean
                  double *D_lambda,    // Weight of the current observation in the EWMA
                  double *D_h_e,       // Control limit for the EWMA
                  double *D_h_s,       // Control limit for the Shewhart chart
                  int    *I_nRep,      // Number of simulations
                  double *D_srl,       // Sum of the run lengths
                  double *D_ssrl,      // Sum of squares of the run lengths
                  int    *I_nEwma)     // Count of the number of signals from the EWMA   
                   

{

  int i,                   // Index for Control Chart iterations
      i_orig,              // Original index chosen for EWMA looping
      j,                   // Index for MC reps
      i_rl,                // Run length counter 
      i_signal,            // Indicator of a signal
      i_nEwma = 0,         // A counter for the number of times the EWMA signals (versus the Shewhart)
      i_drawn[*I_nY],      // An array indicating whether that particular index was drawn or not
      i_esig[*I_nY];       // An array indicating whether that particular index resulted in an EWMA signal

  double d_rl,             // Double run length
         d_srl = 0,        // Cummulative sum of run lengths
         d_ssrl = 0,       // Cummulative sum of squares of run lengths
         d_maxY,           // Max of current Y value and mu_0
         d_Xprev,          // Previous value of EWMA statistic
         d_Xcurrent,       // Current value of EWMA statistic
         d_Ycurrent,       // Current value of the training data
         d_runL[*I_nY];    // Array of run lenghts resulting from starting in that particular index

  // Initialize the integer arrays 
  for (i = 0; i < *I_nY; i++) {
    i_drawn[i] = 0;
    i_esig[i] = 0;
  }

  // Check that arrays have initialized correctly
  // for (i = 0; i < *I_nY; i++)
  //   Rprintf("i=%i, i_drawn[i]=%i, d_runL[i]=%f \n", i, i_drawn[i], d_runL[i]);

  // Initialize the random number generator       
  GetRNGstate();  

  // Loop for simulation iteration
  for (j = 0; j < *I_nRep; j++) {

    // Select the starting index of the test set to draw from.  Since i is an integer,
    // the double returned by runif() should be rounded down to the nearest integer
    i_orig = runif(0, *I_nY);

    // Check to see whether this i has been drawn before.  If so, look up the ARL and
    // bypass the simulation
    if (i_drawn[i_orig]) {
      d_rl = d_runL[i_orig];
      i_nEwma += i_esig[i_orig];
      // Rprintf("i_orig = %i, i_drawn[i_orig] = %i, d_runL[i_orig] = %f\n", i_orig, i_drawn[i_orig], d_runL[i_orig]);
    }

    // Otherwise, calculate the run length for this particular i_orig
    else {

      // Set starting point for EWMA
      i = i_orig;

      // Initialize run length
      i_rl = 0;
  
      // Initialize EWMA statistics
      d_Xprev = *D_mu_0;
      i_signal = 0;

      // Loop for selecting a particular sequence of training data to pass 
      // through the ShewIEWMA chart
      while (!i_signal) {
  
        d_Ycurrent = D_Y[i];
  
        // Calculate the maxY
        if (d_Ycurrent < *D_mu_0)
          d_maxY = *D_mu_0;
        else
          d_maxY = d_Ycurrent;
  
        // Calculate the EWMA
        d_Xcurrent = (1 - *D_lambda) * d_Xprev + *D_lambda * d_maxY;
  
        // Increment the runlength counter
        i_rl++;
  
        // Check out the details of the simulation--Looks good
        // Rprintf("ShewIewmaSim: j = %i, i = %i, Y = %f, maxY = %f, Xcurrent = %f, Xprev = %f, rl = %i\n",
        //        j, i, d_Ycurrent, d_maxY, d_Xcurrent, d_Xprev, i_rl);
  
        // Check for signals on the EWMA and the Shewhart, and or to long a run at 10^8
        if ((d_Xcurrent > *D_h_e) | (d_Ycurrent > *D_h_s) | (i_rl >= 100000000)) {
  
          // Count the number of EWMA signals
          if (d_Xcurrent > *D_h_e) {
            i_esig[i_orig] = 1;
            i_nEwma++;
	  }
  
          // If it has run too long, return an error
          if (i_rl >= 100000000) 
            error("Control limit is too high. Run length for iteration %i has reached 10^8\n", j);
  
          // Flag the signal, which will break the while loop
          i_signal = 1;
  
        } // if signal...
  
        // Reset Xprev
        d_Xprev = d_Xcurrent;
  
        // Incement the index for the next iteration
        i++;
  
        // If the index has gone beyond the end of the series, send it back to the beginning
        if (i >= *I_nY)
  	  i = 0;
  
      } // EWMA while loop

      // Set the run length variables
      d_rl = i_rl;
      d_runL[i_orig] = d_rl;

      //Rprintf("i_orig = %i, i_drawn[i_orig] = %i, d_runL[i_orig] = %f, i_rl = %i\n", 
      //         i_orig,      i_drawn[i_orig],      d_runL[i_orig],      i_rl);

      // Set the 'drawn' flag to 1
      i_drawn[i_orig] = 1;

    }  // end else
   

    // Increment the cumulative sum (and sums of squares) of runlengths
    d_srl += d_rl;
    d_ssrl += d_rl * d_rl;

  } // End simulation, close j loop

  // Close the random number generator
  PutRNGstate();

  // 'Return' the sum and the sum of squares of the run lengths
  *D_srl = d_srl;
  *D_ssrl = d_ssrl;
  *I_nEwma = i_nEwma;

} // void ShewIewmaSim



// An IEWMA control chart only

void IewmaSim(double *D_Y,         // Vector of in-control distribution of training data
              int    *I_nY,        // Length of Y
              double *D_mu_0,      // Estimate of the in-control mean
              double *D_lambda,    // Weight of the current observation in the EWMA
              double *D_h_e,       // Control limit for the EWMA
              double *D_h_s,       // Control limit for the Shewhart chart
              int    *I_nRep,      // Number of simulations
              double *D_srl,       // Sum of the run lengths
              double *D_ssrl,      // Sum of squares of the run lengths
              int    *I_nEwma)     // Count of the number of signals from the EWMA   

{

  int i,                   // Index for Control Chart iterations 
      i_orig,              // Original index chosen for EWMA looping
      j,                   // Index for MC reps
      i_rl,                // Run length counter 
      i_signal,            // Indicator of a signal
      i_drawn[*I_nY];      // An array indicating whether that particular index was drawn or not
  
  double d_rl,             // Double run length
         d_srl = 0,        // Cummulative sum of run lengths
         d_ssrl = 0,       // Cummulative sum of squares of run lengths
         d_maxY,           // Max of current Y value and mu_0
         d_Xprev,          // Previous value of EWMA statistic
         d_Xcurrent,       // Current value of EWMA statistic
         d_Ycurrent,       // Current value of the training data
         d_runL[*I_nY];    // Array of run lenghts resulting from starting in that particular index

  // Initialize the integer array
  for (i = 0; i < *I_nY; i++) {
    i_drawn[i] = 0;
  }

  // Initialize the random number generator       
  GetRNGstate();  

  // Loop for simulation iteration
  for (j = 0; j < *I_nRep; j++) {

    // Select the starting index of the test set to draw from.  Since i is an integer,
    // the double returned by runif() should be rounded down to the nearest integer
    i_orig = runif(0, *I_nY);

    // Check to see whether this i has been drawn before.  If so, look up the ARL and
    // bypass the simulation
    if (i_drawn[i_orig]) {
      d_rl = d_runL[i_orig];
      //      Rprintf("i_orig = %i, i_drawn[i_orig] = %i, d_runL[i_orig] = %f\n",
      //  	       i_orig,      i_drawn[i_orig],      d_runL[i_orig]);
    }

    // If the index has not been drawn before
    else {

      // Set starting point for EWMA
      i = i_orig;

      // Initialize run length
      i_rl = 0;
  
      // Initialize EWMA statistics
      d_Xprev = *D_mu_0;
      i_signal = 0;

      // Loop for selecting a particular sequence of training data to pass 
      // through the IEWMA chart
      while (!i_signal) {
  
        d_Ycurrent = D_Y[i];
  
        // Calculate the maxY
        if (d_Ycurrent < *D_mu_0)
          d_maxY = *D_mu_0;
        else
          d_maxY = d_Ycurrent;
  
        // Calculate the EWMA
        d_Xcurrent = (1 - *D_lambda) * d_Xprev + *D_lambda * d_maxY;
  
        // Increment the runlength counter
        i_rl++;
  
        // Check out the details of the simulation--Looks good
        // Rprintf("IewmaSim: j = %i, i = %i, Y = %f, maxY = %f, Xcurrent = %f, Xprev = %f, rl = %i\n",
        //        j, i, d_Ycurrent, d_maxY, d_Xcurrent, d_Xprev, i_rl);
  
        // Check for signals on the EWMA and too long a run (10^8)
        if ((d_Xcurrent > *D_h_e) | (i_rl >= 100000000)) {
  
          // If it has run too long...
          if (i_rl >= 100000000) 
            error("Control limit is too high. Run length for iteration %i has reached 10^8\n", j);   
  
          // Flag the signal, which will break the while loop
          i_signal = 1;
  
        } // if signal...
  
        // Reset Xprev
        d_Xprev = d_Xcurrent;
  
        // Incement the index for the next iteration
        i++;
  
        // If the index has gone beyond the end of the series, send it back to the beginning
        if (i >= *I_nY)
  	  i = 0;
  
      } // EWMA while loop

      // Set the run length variables
      d_rl = i_rl;
      d_runL[i_orig] = d_rl;

      //Rprintf("i_orig = %i, i_drawn[i_orig] = %i, d_runL[i_orig] = %f, i_rl = %i\n", 
      //         i_orig,      i_drawn[i_orig],      d_runL[i_orig],      i_rl);

      // Set the 'drawn' flag to 1
      i_drawn[i_orig] = 1;

    } // else

    // Increment the cumulative sum (and sums of squares) of runlengths
    d_srl += d_rl;
    d_ssrl += d_rl * d_rl;

  } // End simulation, close j loop

  // Close the random number generator
  PutRNGstate();

  // 'Return' the sum and the sum of squares of the run lengths
  *D_srl = d_srl;
  *D_ssrl = d_ssrl;
  *I_nEwma = *I_nRep;

} // void IewmaSim


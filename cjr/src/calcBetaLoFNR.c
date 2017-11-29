#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Calculate the confidence accounting for false negatives
void calcBetaLoFNR(int *i_n_hi,
                   int *i_n_lo,
                   double *d_beta_hi,
                   double *d_phi_hi,
                   double *d_phi_lo,
                   double *d_rho,
                   int *i_verbose,
                   // These are the results we're after
                   double *d_beta_lo,
                   double *d_x_hi_summands_3,
                   double *d_x_lo_summands_1,
                   double *d_conf_denom) {

  int i_x_hi, i_x_lo; 
  
  double d_x_hi,
		     d_x_lo,
         d_n_hi = *i_n_hi,
         d_n_lo = *i_n_lo,
         d_beta_lo_num = 0, 
         d_beta_lo_denom = 0,
 		     d_conf_lo_denom = 0;

  // Calculate the posterior mean of theta_h given \hat{x_hi} = 0, and compute some extra
  // quantities along the way that will be needed later

  // Loop over the i_x_hi
  for (i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++) {

    // Define i_x_hi as double
    d_x_hi = i_x_hi;
 
    // Compute an array of summands that will be used in the confidence function
    d_x_hi_summands_3[i_x_hi] = lchoose(d_n_hi, d_x_hi) + d_x_hi * log(*d_phi_hi);
                             
    // Aggregate the numerator of beta_lo
    d_beta_lo_num += expm1(d_x_hi_summands_3[i_x_hi] + lbeta(d_x_hi + 2, d_n_hi - d_x_hi + *d_beta_hi)) + 1;

    // Aggregate the denominator of beta_lo and the 'hi' portion of the denominator of the confidence
    d_beta_lo_denom += expm1(d_x_hi_summands_3[i_x_hi] + lbeta(d_x_hi + 1, d_n_hi - d_x_hi + *d_beta_hi)) + 1;

  } // Loop over x_hi

  // Printing stuff
  if (*i_verbose) 
    Rprintf("d_beta_lo_num = %g, d_beta_lo_denom = %g\n", 
            d_beta_lo_num, d_beta_lo_denom);

  // R_FINITE(x) returns FALSE if x is NA, NaN, Inf, or -Inf
  if (!R_FINITE(d_beta_lo_num)) 
    error("Extreme parameter values led to loss of precision in 'd_beta_lo_num'");
  if (!R_FINITE(d_beta_lo_denom)) 
    error("Extreme parameter values led to loss of precision in 'd_beta_lo_denom'");

  // Calculate beta_lo (and yes, in this case, beta_lo_denom / beta_lo_num is correct :)
  *d_beta_lo = *d_rho * d_beta_lo_denom / d_beta_lo_num - 1;

  // Now compute values for the denominator of the overall confidence function that depend on beta_lo
  for (i_x_lo = 0; i_x_lo <= *i_n_lo; i_x_lo ++) {

		 // Convert to double 
     d_x_lo = i_x_lo;

     // Compute an array of terms that will be needed later in the confidence function
     d_x_lo_summands_1[i_x_lo] = lchoose(d_n_lo, d_x_lo) + d_x_lo * log(*d_phi_lo);

     // Aggregating the 'lo' portion of the denominator of the confidence function
     d_conf_lo_denom += expm1(d_x_lo_summands_1[i_x_lo] + lbeta(d_x_lo + 1, d_n_lo - d_x_lo + *d_beta_lo)) + 1;

  } // Loop over x_lo

  // And calculate the denominator of the confidence function
  *d_conf_denom = d_beta_lo_denom * d_conf_lo_denom;

  // Print the denominator
  if (*i_verbose) 
    Rprintf("d_conf_denom = %g\n", *d_conf_denom);

} // calcBetaLoFNR


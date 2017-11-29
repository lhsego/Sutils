#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Calculate the confidence accounting for false negatives

void calcConfFNR(int *i_N,
                 int *i_n_hi,
                 int *i_n_lo,
                 double *d_beta_hi,
                 double *d_phi_hi,
                 double *d_phi_lo,
                 double *d_rho,
                 int *i_thresh,
                 int *i_verbose,
                 double *d_conf) {
      
  // Double variables needed in the calculation
  double 

    // To calculate beta_lo
    summands, 
    d_beta_lo_num = 0, 
    d_beta_lo_denom = 0, 
    d_beta_lo,

    // Double versions of variables that were originally int
    d_x_hi,
    d_x_lo, 
    d_n_hi = *i_n_hi,
    d_n_lo = *i_n_lo,
    d_N = *i_N,
    d_k,
    d_j,
    d_i,

		// Intermediate variables for calculating the confidence
		d_01, d_02, d_03, d_04, d_05, d_06,

    // Variables for aggregating the numerotor and denominator of the confidence
    d_conf_num = 0,
    d_conf_denom = 0;

  // Various integer variables we'll need for loops
  int i_x_hi, i_x_lo, i_i, i_j, i_k; 

  ////////////////////////////////////////////////////////////////////////////////
  // Calculate the posterior mean of theta_h given \hat{x_hi} = 0, to get beta_lo
  // (See page 76 of Landon's notebook)
  ////////////////////////////////////////////////////////////////////////////////

  // Loop over the i_x_hi
  for (i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++) {

    // Define i_x_hi as double
    d_x_hi = i_x_hi;
 
    // Compute the summands of the denominator 
    // (Note:  'exp1(x)' returns 'exp(x) - 1', hence adding 1)
    summands = lchoose(d_n_hi, d_x_hi) + d_x_hi * log(*d_phi_hi);

    // Compute the numerator
    d_beta_lo_num += expm1(summands + lbeta(d_x_hi + 2, d_n_hi - d_x_hi + *d_beta_hi)) + 1;

    // Compute the denominator
    d_beta_lo_denom += expm1(summands + lbeta(d_x_hi + 1, d_n_hi - d_x_hi + *d_beta_hi)) + 1;
  
  } // beta_lo loop over x_hi

  // R_FINITE(x) returns FALSE if x is NA, NaN, Inf, or -Inf
  if (!R_FINITE(d_beta_lo_num)) 
    error("Extreme parameter values led to loss of precision in 'd_beta_lo_num'");
  if (!R_FINITE(d_beta_lo_denom)) 
    error("Extreme parameter values led to loss of precision in 'd_beta_lo_denom'");

  // And the resulting value of \beta_lo
  d_beta_lo = *d_rho * d_beta_lo_denom / d_beta_lo_num - 1;

  if (*i_verbose) 
    Rprintf("d_beta_lo = %g\n", d_beta_lo);

  ////////////////////////////////////////////////////////////////////////////////
  // Now calculate components of the confidence function, looping over x_hi, x_lo, 
  // i, j, and k
  ////////////////////////////////////////////////////////////////////////////////
  
  // Looping over x_hi
  for (i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++) {

    // Convert x_hi from int to double
    d_x_hi = i_x_hi;

    // Terms that depend on x_hi
    d_01 = lchoose(d_n_hi, d_x_hi) + d_x_hi * log(*d_phi_hi);
    d_02 = lbeta(d_x_hi + 1, d_n_hi - d_x_hi + *d_beta_hi);

		// Looping over x_lo
		for (i_x_lo = 0; i_x_lo <= *i_n_lo; i_x_lo++) {

      // Convert x_lo from int to double
      d_x_lo = i_x_lo;

      // Terms that depend on x_lo
      d_03 = lchoose(d_n_lo, d_x_lo) + d_x_lo * log(*d_phi_lo);
      d_04 = lbeta(d_x_lo + 1, d_n_lo - d_x_lo + d_beta_lo);

			// Add terms to the denominator
			d_conf_denom += expm1(d_01 + d_02 + d_03 + d_04) + 1;

      // Looping over k (hypothetical values of x_hi)
      for (i_k = 0; i_k <= imin2(*i_n_hi, *i_thresh); i_k++) {

        // Convert k from int to double
        d_k = i_k;

				// Terms that depend on k, x_lo, and x_hi;
        d_05 = lchoose(d_n_hi, d_k) +
               lbeta(d_x_hi + d_k + 1, 2 * d_n_hi - d_x_hi + *d_beta_hi - d_k) + 
      				 d_01 + d_03;

        // Looping over j (hypothetical values of x_lo)
        for (i_j = 0; i_j <= imin2(*i_n_lo, *i_thresh - i_k); i_j++) {

          // Convert j from int to double
          d_j = i_j;
  
  				// Terms that depend on j, k, x_lo, and x_hi
          d_06 = lchoose(d_n_lo, d_j) + d_05;

          // Looping over i (hypothetical values of y_lo)
          for (i_i = 0; i_i <= imin2(*i_N - *i_n_hi - *i_n_lo, *i_thresh - i_k - i_j); i_i++) {

            // Convert i from int to double
            d_i = i_i;
  
            // Compute the summand of the numerator, add in terms that depend on i
            d_conf_num += expm1(d_06 + lchoose(d_N - d_n_hi - d_n_lo, d_i) + 
                                lbeta(d_x_lo + d_i + d_j + 1, 
                                      d_N - d_n_hi + d_n_lo - d_x_lo + d_beta_lo - d_i - d_j)) + 1;

					} // Loop over i

				} // Loop over j

			} // Loop over k

		} // Loop over x_lo

	} // Loop over x_hi


  ////////////////////////////////////////////////////////////////////////////////
  // Check intermediate outputs and calculate the confidence
  ////////////////////////////////////////////////////////////////////////////////

  // Check results
  if (!R_FINITE(d_conf_denom)) 
    error("Extreme parameter values led to loss of precision in 'd_conf_denom'");

  if (!R_FINITE(d_conf_num)) 
    error("Extreme parameter values led to loss of precision in 'd_conf_num'");

  // Look at results to check
  if (*i_verbose) 
    Rprintf("d_conf_num = %g, d_conf_denom = %g\n",
            d_conf_num, d_conf_denom);


  // Now calculate the confidence
  *d_conf = d_conf_num / d_conf_denom;

} // calcConfFNR

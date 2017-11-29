#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

// Calculate the confidence accounting for false negatives
void calcConfFNR_parallel(int *i_N,
                          int *i_n_hi,
                          int *i_n_lo,
                          int *i_index_start,
                          int *i_index_end,
                          double *d_beta_hi,
                          double *d_beta_lo,
                          double *d_phi_hi,
                          double *d_phi_lo,
                          double *d_rho,
                          double *d_x_hi_summands_3,
                          double *d_x_lo_summands_1,
                          int *i_thresh,
                          int *i_verbose,
                          double *d_num_partial) {
               
  // Double variables needed in the calculation
  double 

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
		d_1, d_2, d_3,

    // Variables for aggregating the numerotor and denominator of the confidence
    d_conf_num = 0;

  // Various integer variables we'll need for loops
  int i_x_hi_lo, i_x_hi, i_x_lo, i_i, i_j, i_k;

  ////////////////////////////////////////////////////////////////////////////////
  // Now calculate components of the confidence function, looping over the joined
  // indexes of x_hi and x_lo and then i, j, and k
  ////////////////////////////////////////////////////////////////////////////////
  
	// Looping over a portion of x_lo and x_hi product of indexes -- this is the level of parallelization
	for (i_x_hi_lo = *i_index_start; i_x_hi_lo <= *i_index_end; i_x_hi_lo++) {

    // Extract the x_hi and x_lo from the array of indexes
    i_x_lo = i_x_hi_lo % (*i_n_lo + 1);
    i_x_hi = i_x_hi_lo / (*i_n_lo + 1);

    // Convert from int to double
    d_x_lo = i_x_lo;
    d_x_hi = i_x_hi;

    // Terms that depend on x_lo and x_hi that were calculated previously
    d_1 = d_x_lo_summands_1[i_x_lo] + d_x_hi_summands_3[i_x_hi];;

    // Looping over k (hypothetical values of x_hi)
    for (i_k = 0; i_k <= imin2(*i_n_hi, *i_thresh); i_k++) {

      // Convert k from int to double
      d_k = i_k;

			// Terms that depend on k, x_lo, and x_hi;
      d_2 = lchoose(d_n_hi, d_k) +
            lbeta(d_x_hi + d_k + 1, 2 * d_n_hi - d_x_hi + *d_beta_hi - d_k) + 
    				d_1;

      // Looping over j (hypothetical values of x_lo)
      for (i_j = 0; i_j <= imin2(*i_n_lo, *i_thresh - i_k); i_j++) {

        // Convert j from int to double
        d_j = i_j;
  
				// Terms that depend on j, k, x_lo, and x_hi
        d_3 = lchoose(d_n_lo, d_j) + d_2;

        // Looping over i (hypothetical values of y_lo)
        for (i_i = 0; i_i <= imin2(*i_N - *i_n_hi - *i_n_lo, *i_thresh - i_k - i_j); i_i++) {

          // Convert i from int to double
          d_i = i_i;
  
          // Compute the summand of the numerator, add in terms that depend on i
          d_conf_num += expm1(d_3 + lchoose(d_N - d_n_hi - d_n_lo, d_i) + 
                              lbeta(d_x_lo + d_i + d_j + 1, 
                                    d_N - d_n_hi + d_n_lo - d_x_lo + *d_beta_lo - d_i - d_j)) + 1;

				} // Loop over i

			} // Loop over j

		} // Loop over k

	} // Loop over x_hi_lo

  ////////////////////////////////////////////////////////////////////////////////
  // Check intermediate outputs and calculate the confidence
  ////////////////////////////////////////////////////////////////////////////////

  // R_FINITE(x) returns FALSE if x is NA, NaN, Inf, or -Inf
  if (!R_FINITE(d_conf_num)) 
    error("Extreme parameter values led to loss of precision in 'd_conf_num'");

  // Look at results to check
  if (*i_verbose) 
    Rprintf("d_conf_num = %g\n", d_conf_num);

  // Assign the partial sum of the confidence to the pointer
  *d_num_partial = d_conf_num;

} // calcConfFNR_parallel

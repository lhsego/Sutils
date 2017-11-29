#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void contFNR(int *i_n_hi, // number of judgment samples
			 int *i_n_lo, // number of random samples
			 int *i_thresh, // threshold value t
			 double *d_N, // Total number of items in population
			 double *d_integral, // The simpson's rule estimates
			 double *d_beta_hi, // "prior" over theta_hi
			 double *d_beta_lo, // "prior" over theta_lo
			 double *d_rho, // likelihood multiplier 
			 double *d_fnr, // false negative probability
			 double *d_conf) { // Return the confidence 
      
  int i_x_hi, i_x_lo, i, j;

  double d_n_hi = *i_n_hi, 
		 d_n_lo = *i_n_lo,
         d_x_hi,
		 d_x_lo,
		 d_i,
		 d_j,
		 d_01,
		 sum_xh = 0,
		 sum_num = 0,
		 sum_xl = 0;
    
   // Calculate the denominator of the posterior
   
   for(i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++){

	d_x_hi = i_x_hi;
	
	sum_xh += expm1(d_x_hi*log(*d_fnr) + lchoose(d_n_hi, d_x_hi) + lbeta(d_x_hi+1, d_n_hi - d_x_hi + *d_beta_hi)) + 1;
   
   }
  
  for(i_x_lo = 0; i_x_lo <= *i_n_lo; i_x_lo++){
  
	d_x_lo = i_x_lo;
	
	sum_xl += expm1(d_x_lo*log(*d_fnr) + lchoose(d_n_lo, d_x_lo) + lbeta(d_x_lo + 1, d_n_lo - d_x_lo + *d_beta_lo)) + 1;
		
  }
  
  // The denominator of the posterior is the product of sum_xh and sum_xl
  double post_denom = sum_xh * sum_xl;
  
  for(i = 0; i <= imin2(*i_thresh, *i_n_hi); i++){
  
	d_i = i;
	
	d_01 = lchoose(d_n_hi, d_i) + log(d_integral[i]);
	
	for(j = 0; j <= *i_n_hi; j++){ 
	
		d_j = j;
		// numerator of confidence 
		sum_num += expm1(d_01 + d_j*log(*d_fnr) + lchoose(d_n_hi, d_j) + lbeta(d_i + d_j + 1, 2*d_n_hi + *d_beta_hi - d_i - d_j)) + 1;
		
	}	
	
  }
	
  // The confidence
  *d_conf = sum_num/post_denom;
  
} // contFNR

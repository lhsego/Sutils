#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void optimCCFNR(int *i_N,
                int *i_n_hi,
				int *i_n_lo,
				int *i_nl_array,
				int *i_nh_array,
				int *i_nLength,
				double *d_beta_hi,
				double *d_rho,
				double *d_phi_hi,
				double *d_phi_lo,
				int *i_thresh,
				double *d_conf) {
      

  int i_x_hi, i_x_lo, i, j, k, m;

  double d_N = *i_N, 
		 d_n_hi = *i_n_hi, 
		 d_n_lo = *i_n_lo,
		 d_beta_lo,
         d_x_hi,
		 d_x_lo,
		 d_i,
		 d_j,
		 d_k,
		 d_constPos,
		 d_constNeg,
		 sNum = 0,
		 sDen = 0,
         sum_xh = 0,
		 sum_num = 0,
		 d_pos_terms,
		 d_neg_terms,
		 sum_xl = 0;
    
	
   // Calculate d_beta_lo
   for(i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++){

	d_x_hi = i_x_hi;
	
	sNum = sNum + expm1(log(d_x_hi) + d_x_hi*log(*d_phi_hi) + lgammafn(d_n_hi - d_x_hi + *d_beta_hi) - lgammafn(d_n_hi - d_x_hi + 1)) + 1;
	sDen = sDen + expm1(d_x_hi*log(*d_phi_hi) + lgammafn(d_n_hi - d_x_hi + *d_beta_hi) - lgammafn(d_n_hi - d_x_hi + 1)) + 1;
   
   }
  
   d_beta_lo = *d_rho/(1/(d_n_hi + *d_beta_hi + 1) * (1 + sNum/sDen)) - 1;
   
  // Calculate the denominator of the posterior
  for(i_x_hi = 0; i_x_hi <= *i_n_hi; i_x_hi++){
  
	d_x_hi = i_x_hi;
	
	sum_xh = sum_xh + expm1(lgammafn(d_n_hi+1) - lgammafn(d_n_hi-d_x_hi+1) + d_x_hi*log(*d_phi_hi)
							+ lgammafn(d_n_hi-d_x_hi+*d_beta_hi) - lgammafn(d_n_hi+*d_beta_hi+1)) + 1;
		
  }
  
  for(i_x_lo = 0; i_x_lo <= *i_n_hi; i_x_lo++){
  
	d_x_lo = i_x_lo;
	
	sum_xl = sum_xl + expm1(lgammafn(d_n_lo+1) - lgammafn(d_n_lo-d_x_lo+1) + d_x_lo*log(*d_phi_lo)
							+ lgammafn(d_n_lo-d_x_lo+d_beta_lo) - lgammafn(d_n_lo+d_beta_lo+1)) + 1;
		
  }
  
  // The denominator of the posterior is the product of sum_xh and sum_xl
  double post_denom = sum_xh * sum_xl;
  
  d_constPos = lgammafn(d_n_hi+1)+
			   lgammafn(d_n_lo+1)+ 
			   lgammafn(d_N-d_n_hi-d_n_lo+1)+
			   lgammafn(d_n_lo+1)+
			   lgammafn(d_n_hi+1);
				
   d_constNeg = lgammafn(d_N-d_n_hi+d_n_lo+d_beta_lo+1)+
				lgammafn(2*d_n_hi+*d_beta_hi+1);
  
	for(m = 0; m < *i_nLength; m++){
		d_x_lo = i_nl_array[m];
		d_x_hi = i_nh_array[m];
		for(k = 0; k <= imin2(*i_thresh, *i_n_hi); k++){ 
			d_k = k;
			for(j = 0; j <= imin2(*i_thresh - k, *i_n_lo); j++){ 
			d_j = j;
				for(i = 0; i <= imin2(*i_thresh - k - j, *i_N - *i_n_hi - *i_n_lo); i++){ 
				d_i = i;
					
				d_pos_terms = d_constPos+
							  d_x_lo*log(*d_phi_lo)+
							  d_x_hi*log(*d_phi_hi)+
							  lgammafn(d_x_lo+d_i+d_j+1)+
							  lgammafn(d_N-d_n_hi+d_n_lo-d_x_lo+d_beta_lo-d_i-d_j)+ 
							  lgammafn(d_x_hi+d_k+1)+ 
							  lgammafn(2*d_n_hi-d_x_hi-d_k+*d_beta_hi);
									  
				d_neg_terms = d_constNeg+
							  lgammafn(d_x_hi+1)+ 
							  lgammafn(d_n_hi-d_x_hi+1)+ 
							  lgammafn(d_x_lo+1)+ 
							  lgammafn(d_n_lo-d_x_lo+1)+
							  lgammafn(d_i+1)+
							  lgammafn(d_N-d_n_hi-d_n_lo-d_i+1)+
							  lgammafn(d_j+1)+ 
							  lgammafn(d_n_lo-d_j+1)+ 
							  lgammafn(d_k+1)+ 
							  lgammafn(d_n_hi-d_k+1);
					
				sum_num = sum_num + expm1(d_pos_terms - d_neg_terms) + 1;

				}
			}
		}					
	}

  // Bring it together by taking numerator/denominator
  *d_conf = sum_num/post_denom;
  
} // optimCCFNR

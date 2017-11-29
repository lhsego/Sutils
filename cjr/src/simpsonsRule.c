#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void simpsonsRule(double *d_N, // Total number of items in population
				  double *d_n_lo, // number of random samples
				  double *d_n_hi, // number of judgment samples
				  double *d_thresh, // threshold value t 
				  double *d_fnr, // false negative probability
				  double *d_theta_seq, // sequence {0, ..., 1} Needs to be densely sampled (100k+ points)
				  double *d_beta_lo, // "prior" over theta_lo
				  double *d_a, // lower limit of integration (always 0)
				  double *d_b, // upper limit of integration (always 1)
				  int *i_length_n, // min(*d_n_hi, *d_thresh) 
				  int *i_length_theta, // length of *d_theta_seq
				  double *out){
						
			double f_y, d_i, d_wts[*i_length_theta],
			innerEstimate, d_lt = *i_length_theta;
			
			int i, j;
			
			// Compute the weights for simpson's rule 
			for(i = 0; i < *i_length_theta; i++){
				
				if(i == 0 || i == (*i_length_theta - 1)){
				  d_wts[i] = 1;
				} else if(i % 2 == 0){
				  d_wts[i] = 2;
				} else {
				  d_wts[i] = 4;
				}
				
			}
			
			// I apply the integration directly to the posterior (the thing we have to integrate).
			// You may well be better off using your own version of simpson's rule to estimate 
			// f_y.  
			for(i = 0; i < *i_length_n; i++){
				
				d_i = i;
				
				innerEstimate = 0;
				// compute simpson's rule estimate at index i 
				for(j = 0; j < *i_length_theta; j++){
					
					f_y = expm1(log(pbinom(*d_thresh - d_i, *d_N - *d_n_hi, d_theta_seq[j], 1, 0))
								+ (*d_beta_lo-1)*log(1-d_theta_seq[j]) + (*d_n_lo)*log((1-(1-*d_fnr)*d_theta_seq[j]))) + 1;
								
					innerEstimate += f_y * d_wts[j]; 
					
				}
				// Multiply by scaling factor to get correct simpson's estimate 
				// Return out. It is required to run contFNR. 
				out[i] = (*d_b - *d_a)/(3*(d_lt-1))*innerEstimate;

			}
		
		}
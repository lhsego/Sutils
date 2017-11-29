// Calculates the discrete loss

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void discreteLoss(int *i_nClass,
                  int *i_trueClassLoss,
                  int *i_predClassLoss,
                  double *d_lossValue,
                  int *i_nData,
                  int *i_truthData,
                  int *i_predData,
                  double *d_loss)
{
  int i, j;

  // Loop over data
  for (i = 0; i < *i_nData; i++) {

    // Initialize the loss
    d_loss[i] = NA_REAL;

    //    Rprintf("i = %i, i_truthData[i] = %i, i_predData[i] = %i\n",
    //        i, i_truthData[i], i_predData[i]);

    // Check for missing values
    if ((i_truthData[i] == NA_INTEGER) | (i_predData[i] == NA_INTEGER)) {
      //      Rprintf("A value was NA at row %i\n",i+1);
      continue;
    }

    // Loop over loss matrix vectors
    for (j = 0; j < *i_nClass; j++) {

      // If the predicted and true classes in the data
      // match the predicted and true class from the loss matrix...
      if ((i_truthData[i] == i_trueClassLoss[j]) &
          (i_predData[i] == i_predClassLoss[j])) {

        // Assign the coresponding loss
        d_loss[i] = d_lossValue[j];

        // Break out of this loop
        break;

      }

    }

    // Make sure a loss value was actually assigned
    if (ISNA(d_loss[i]))
      error("A loss value was not assigned to row %i of 'signatures'\n", i+1);

  }

}


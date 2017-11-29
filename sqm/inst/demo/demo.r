# This statement loads the 'SQM' R package.
library('SQM')

# The input file provides the typical structure of the signature data that will be inputted into SQM.
# Each row of the input file corresponds to a classified observation
# for the specified signature; the specified signature is given in the first column of each row.
input_file <- "signatures.csv"

# Now, suppose that we were only given the input file, and we wish to compute the accuracy metrics for
# each signature and output them to a specified file.
output_file <- "accuracies.csv"

# Here, we read in the input file, compute the accuracy metrics for each signature, and write
# the computed accuracy metrics to the output file. The first column of each row in the output file contains
# the signature identifier, and the other columns correspond to each of the computed accuracy metrics.
afAccuracy(input_file, output_file)
## SQM: An R Package for Signature Quality Metrics

Contains methods for performing multi-attribute utility assessments, estimating error of 
multi-attribute utility scores via bootstapping, as well as the typical classifier 
performance metrics associated with confusion matrices.

Methods in the SQM package were developed as part of the Signature Quality Metrics
project under the Signature Discovery Initiative at the Pacific Northwest 
National Laboratory (PNNL).

#### To cite:

Sego LH, Ramey JA, White AM. 2016. SQM: An R Package for Signature Quality Metrics.
Pacific Northwest National Laboratory. http://pnnl.github.io/SQM.

#### To install:

Before you get started, the source code of the `Smisc` package, which is imported by `SQM`, 
contains C code that requires compilation:
  
- Mac: you'll need [Xcode](https://developer.apple.com/xcode/)
- Windows: you'll need to install [R tools](http://cran.r-project.org/bin/windows/Rtools/)
- Linux/Unix: compilation should take place automatically

    # Install 'devtools' if needed
    install.packages("devtools")

    devtools::install_github("pnnl/Smisc")
    devtools::install_github("pnnl/SQM")


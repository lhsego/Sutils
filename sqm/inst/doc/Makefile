# Makefile to use knitr for package vignettes
# Original source from Yihui Xie:
# http://yihui.github.com/knitr/demo/vignette/

# For more about Makefiles and R, see:
# http://www.stat.auckland.ac.nz/~stat782/downloads/make-tutorial.pdf

# Put all PDF targets here, separated by spaces.
PDFS= vignette-accuracy.pdf vignette-utility.pdf vignette-cost.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *.csv

%.pdf: %.Rnw
	R -e "library(knitr); knit2pdf('$*.Rnw')"

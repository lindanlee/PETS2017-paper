circumvention-experiment.pdf: circumvention-experiment.tex circumvention-experiment.bib

PDFLATEX = pdflatex -halt-on-error -file-line-error
BIBTEX = bibtex
%.pdf: %.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*

clean:
	rm -f circumvention-experiment.pdf

.PHONY: clean

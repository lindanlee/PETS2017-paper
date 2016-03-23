pets2017-paper.pdf: pets2017-paper.tex pets2017-paper.bib

PDFLATEX = pdflatex -halt-on-error -file-line-error
BIBTEX = bibtex
%.pdf: %.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*

clean:
	rm -f pets2017-paper.pdf

.PHONY: clean

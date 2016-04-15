FIGS = *.png time_to_success.pdf time_to_success_clamped.pdf all-participant-edges.pdf

pets2017-paper.pdf: pets2017-paper.tex pets2017-paper.bib $(FIGS)

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

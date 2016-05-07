FIGS = *.png time_to_success_clamped.pdf all-participant-edges-scales.pdf

pets2017-paper.pdf: pets2017-paper.tex pets2017-paper.bib $(FIGS)

PDFLATEX = pdflatex -halt-on-error -file-line-error
BIBTEX = bibtex
%.pdf: %.tex
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*

clean:
	rm -f pets2017-paper.pdf

.PHONY: clean

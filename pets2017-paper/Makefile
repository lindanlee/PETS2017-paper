FIGS = *.png time_to_success_clamped.pdf time_to_success_active_clamped.pdf all-participant-edges-scales.pdf
TABLES = participant-summary.tex attempts-bridge-proxy.tex time_to_success_ecdf.tex all-participant-times.tex

pets2017-paper.pdf: pets2017-paper.tex pets2017-paper.bib $(FIGS) $(TABLES)

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

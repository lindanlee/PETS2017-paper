FIGS = *.png time_to_success_clamped.pdf time_to_success_active_clamped.pdf time_to_success_ecdf.pdf all-participant-edges.pdf torconfig.pdf topology.pdf
TABLES = participant-summary.tex attempts-bridge-proxy.tex time_to_success_ecdf.tex all-participant-times.tex median_time.tex

pets2017-paper.pdf: pets2017-paper.tex pets2017-paper.bib $(FIGS) $(TABLES)

PDFLATEX = pdflatex -halt-on-error -file-line-error
BIBTEX = bibtex
%.pdf: %.tex
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*

# Disable the %.pdf rule for this figure, because
# time_to_success_ecdf.tex also exists but is not the source for the
# figure (it's a table instead).
time_to_success_ecdf.pdf:
	:

torconfig.pdf: ../torconfig.dot
	dot -Tpdf "$^" > "$@"

clean:
	rm -f pets2017-paper.pdf

.PHONY: clean

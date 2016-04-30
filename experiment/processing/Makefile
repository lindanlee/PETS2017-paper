GRAPHS = time_to_success.pdf time_to_success_clamped.pdf time_to_success_ecdf.pdf all-participant-edges.pdf \
	time_per_screen.pdf

all: $(GRAPHS)

$(GRAPHS): graphs.R common.R *.csv
	Rscript "$<"

edges.csv: all_edges logs/*.log
	"./$<" logs/*.log > "$@"

attempts.csv: attempts edges.csv
	"./$<" edges.csv > "$@"

.PHONY: all

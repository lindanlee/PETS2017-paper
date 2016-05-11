GRAPHS = time_to_success_clamped.pdf time_to_success_active_clamped.pdf time_to_success_ecdf.pdf all-participant-edges.pdf \
	time_per_screen.pdf percent_per_screen.pdf
TABLES = attempts-bridge-proxy.tex participant-summary.tex time_to_success_ecdf.tex

all: $(GRAPHS) $(TABLES)

$(GRAPHS): graphs.R common.R *.csv
	Rscript "$<"

edges.csv: all_edges logs/*.log
	"./$<" logs/*.log > "$@"

attempts.csv: attempts edges.csv
	"./$<" edges.csv > "$@"

.PHONY: all

GRAPHS = time_to_success.pdf time_to_success_clamped.pdf time_to_success_ecdf.pdf

all: $(GRAPHS)

$(GRAPHS): graphs.R common.R
	Rscript "$<"

.PHONY: all

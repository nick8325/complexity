BEAM_FILES = example_gb_sets.beam example_queues.beam example_sorting.beam example_trees.beam fit.beam measure.beam timing.beam

.PHONY: all clean
all: $(BEAM_FILES) Fit

%.beam: %.erl
	erlc $<

Fit: Fit.hs
	ghc --make -O Fit

clean:
	rm -f Fit Fit.o Fit.hi $(BEAM_FILES) gnuplot data

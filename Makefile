ERL_FILES = example_gb_sets.erl example_queues.erl example_sorting.erl example_trees.erl fit.erl measure.erl timing.erl
BEAM_FILES = $(ERL_FILES:.erl=.beam) examples.beam

.PHONY: all clean
all: $(BEAM_FILES) Fit

%.beam: %.erl
	erlc $<

Fit: Fit.hs
	ghc --make -O Fit

examples.erl: $(ERL_FILES)
	echo '-module(examples).' > $@
	echo '-compile(export_all).' >> $@
	grep 'measure_.*()' $^ | sed 's/^\(.*\)\.erl:measure_\(.*\)().*$$/\2() -> \1:measure_\2()./' >> $@

clean:
	rm -f Fit Fit.o Fit.hi $(BEAM_FILES) examples.erl gnuplot data

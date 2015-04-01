ERL_FILES = example_gb_sets.erl example_queues.erl example_deques.erl example_sorting.erl example_trees.erl example_avl_trees.erl queues.erl priority_queue.erl example_jsx_gedell.erl example_http.erl
BEAM_FILES = $(ERL_FILES:.erl=.beam) examples.beam

.PHONY: all clean
all: $(BEAM_FILES)

%.beam: %.erl
	erlc -I ../../include $<
	cp $@ ../../ebin

examples.erl: $(ERL_FILES)
	echo '-module(examples).' > $@
	echo '-compile(export_all).' >> $@
	grep '^measure_.*()' $^ | sed 's/^\(.*\)\.erl:measure_\(.*\)().*$$/\2() -> \1:measure_\2()./' >> $@

clean:
	rm -f $(BEAM_FILES) examples.erl

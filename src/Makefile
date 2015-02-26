
ERL_FILES = fit.erl timing.erl measure.erl measure_java.erl json_gen.erl
BEAM_FILES = $(ERL_FILES:.erl=.beam)

.PHONY: all clean
all: $(BEAM_FILES)

%.beam: %.erl
	erlc -I ../include $<
	cp $@ ../ebin

clean:
	rm -f $(BEAM_FILES)

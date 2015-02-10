
ERL_FILES = fit.erl timing.erl measure.erl measure_java.erl
BEAM_FILES = $(ERL_FILES:.erl=.beam)

.PHONY: all clean
all: $(BEAM_FILES)

%.beam: %.erl
	erlc -I ../include $<

clean:
	rm -f $(BEAM_FILES)

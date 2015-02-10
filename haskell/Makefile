
.PHONY: all clean
all: Fit
	cp Fit ../examples/erlang
	cp Fit ../examples/java

Fit: Fit.hs
	ghc --make -O Fit

clean:
	rm -f Fit Fit.o Fit.hi ../examples/erlang/Fit ../examples/java/Fit

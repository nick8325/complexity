
.PHONY: all clean
all: Fit

Fit: Fit.hs
	ghc --make -O Fit
	cp Fit ../ebin

clean:
	rm -f Fit.o Fit.hi Fit

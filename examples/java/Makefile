
all: MyClass.class example_MyClass.beam


%.beam: %.erl
	erlc -I ../../include $< 

%.class: %.java
	javac -classpath ../../java $<

clean:
	rm -f *.class *.beam data gnuplot plot.pdf

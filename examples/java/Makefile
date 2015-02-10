
all: MyClass.class example_MyClass.beam


%.beam: %.erl
	erlc -I ../../include $< 

%.class: %.java
	javac -classpath ../../java $<

#	javac -classpath javassist.jar:. Complexity.java

clean:
	rm -f *.class *.beam data gnuplot plot.pdf

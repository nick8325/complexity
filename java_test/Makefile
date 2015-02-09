



all: java_test.beam MyClass.class Complexity.class


java_test.beam: java_test.erl
	erl -make

%.class: %.java
	javac $<

Complexity.class: Complexity.java
	javac -classpath /usr/lib/erlang/lib/jinterface-1.5.11/priv/OtpErlang.jar:javassist.jar:. Complexity.java

clean:
	rm -f *.class *.beam plot.pdf data



all: Complexity.class
	cp *.class javassist/javassist.jar ../ebin


Complexity.class: Complexity.java
	javac -classpath javassist/javassist.jar:. Complexity.java

clean:
	rm -f *.class


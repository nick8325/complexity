
all: MyClass.class ArrayListWrapper.class example_MyClass.beam example_containers.beam
	cp erl.sh java ../../ebin


%.beam: %.erl
	erlc -I ../../include $< 
	cp $@ ../../ebin

%.class: %.java
	javac -classpath ../../java $<
	cp $@ ../../ebin

clean:
	rm -f *.class *.beam

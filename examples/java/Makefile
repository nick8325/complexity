
all: MyClass.class ArrayListWrapper.class example_MyClass.beam example_containers.beam example_json_simple.beam
	cp erl.sh java ../../ebin


%.beam: %.erl
	erlc -I ../../include $< 
	cp $@ ../../ebin

%.class: %.java
	javac -classpath ../../java:../../external/json-simple/json-simple-1.1.1.jar $<
	cp $@ ../../ebin

clean:
	rm -f *.class *.beam

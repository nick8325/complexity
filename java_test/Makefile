



all: java_test.beam MyClass.class MyTest.class TestCase.class Command.class


java_test.beam: java_test.erl
	erl -make

%.class: %.java
	javac $<

MyTest.class: MyTest.java
	javac -classpath /usr/lib/erlang/lib/jinterface-1.5.11/priv/OtpErlang.jar:javassist.jar:. MyTest.java


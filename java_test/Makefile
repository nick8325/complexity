



all: java_test.beam MyClass.class MyTest.class


java_test.beam: java_test.erl
	erl -make

MyClass.class: MyClass.java
	javac MyClass.java

MyTest.class: MyTest.java
	javac -classpath /usr/lib/erlang/lib/jinterface-1.5.11/priv/OtpErlang.jar:. MyTest.java


# Makefsle for FuncLang
#

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	scalac $*.scala

all: testfli

FLInterp.class: FuncLang.class
TestFLI.class: FLInterp.class 

fl:	FuncLang.class
fli: FLInterp.class
testfli: TestFLI.class

clean:	
	rm *.class

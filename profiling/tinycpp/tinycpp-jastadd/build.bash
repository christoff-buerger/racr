#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

clear_all(){
	rm -rf src-gen
	rm -rf java-bin
	mkdir src-gen
	mkdir java-bin
}

make_syntax(){
	java -jar tools/jflex.jar --quiet -d src-gen/tinycpp specifications/Lexer.jflex
	java -jar tools/beaver.jar -c -d src-gen specifications/Parser.beaver
}

make_binary(){
	javac -deprecation -encoding utf-8 -classpath tools/beaver-rt.jar \
		-d java-bin -sourcepath src-gen src/**/*.java src-gen/**/*.java
	binaries=`find java-bin/*`
	jar cfm $1 manifest.txt -C java-bin ${binaries#*/}
}

# Delete jars:
rm *.jar

# Build declarative version:
clear_all
make_syntax
java -jar tools/jastadd2.jar --rewrite --package="tinycpp.ast" --o=src-gen \
	specifications/*.ast `ls specifications/*.jrag | grep -v NormalizationIterative.jrag`
make_binary tinycpp-declarative.jar

# Build iterative version:
clear_all
make_syntax
java -jar tools/jastadd2.jar --package="tinycpp.ast" --o=src-gen \
	specifications/*.ast `ls specifications/*.jrag | grep -v NormalizationDeclarative.jrag`
make_binary tinycpp-iterative.jar

# Build iterative, cached, incremental version:
clear_all
make_syntax
java -jar tools/jastadd2.jar --rewrite --cache all --incremental full --package="tinycpp.ast" --o=src-gen \
	specifications/*.ast `ls specifications/*.jrag | grep -v NormalizationDeclarative.jrag`
make_binary tinycpp-iterative-cached.jar

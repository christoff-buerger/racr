#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

clear
clear
clear

echo "=========================================>>> PDF Latex 1"
pdflatex -interaction=nonstopmode racr-net-manual.tex | grep !

echo "=========================================>>> Make Bibliography"
biber racr-net-manual

echo "=========================================>>> Make Index"
makeindex racr-net-manual.idx

echo "=========================================>>> PDF Latex 2"
pdflatex -interaction=nonstopmode racr-net-manual.tex | grep !

echo "=========================================>>> PDF Latex 3"
pdflatex -interaction=nonstopmode racr-net-manual.tex

echo "=========================================>>> Delete Intermediate Results"
# Filter directories, the bibliography, the TeX sources, the generated pdf and this shell script; Remove the rest. 
for f in `ls -p | grep -v / | grep -v bibliography.bib | grep -v .tex | grep -v racr-net-manual.pdf | grep -v .bash`
do
	echo $f
	rm $f
done

open racr-net-manual.pdf

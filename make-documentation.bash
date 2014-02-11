#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

############################# Initialise Variables ############################

# The absolute path of the current directory:
basedir=`pwd`

# Array of Wiki pages to process:
declare -a wiki_pages=(
	Introduction
	Architecture
	ASTs
	Attributes
	Rewrites
	Annotations
	SupportAPI)

######################### Parse Command Line Arguments ########################

if [ $# -lt 2 ]
then
	echo "Wrong number of arguments - at least two arguments expected:"
	echo "	(1) Racket distribution directory"
	echo "	(2) RACR Wiki Git repository"
	exit 1
else
	cd  $1
	scheme_distro=`pwd`
	shift
	cd $basedir
	cd $1
	wiki_sources=`pwd`
	shift
fi
while [ $# -gt 0 ]
do
	case $1 in
		--pullwiki)
			pullwiki=on;;
		--clearall)
			clearall=on;;
		*)
			echo "Unknown command line option [$1]."
			exit 1;;
	esac
	shift
done

######################### Generate RACR Documentation #########################

#echo "=========================================>>> Delete Old Manual"
#cd ${basedir}/documentation
#echo "${basedir}/documentation/racr-manual.pdf"
#rm racr-manual.pdf

if [ $pullwiki ]
then
	echo "=========================================>>> Pull Wiki"
	cd $wiki_sources
	git pull
fi

echo "=========================================>>> Copy Sources"
for f in `ls ${wiki_sources}/documentation/print/*`
do
	echo "$f"
	cp $f ${basedir}/documentation
done
for f in ${wiki_pages[@]}
do
	echo "${wiki_sources}/${f}.wiki"
	cp ${wiki_sources}/${f}.wiki ${basedir}/documentation
done

echo "=========================================>>> Wiki -> Tex"
cd ${basedir}/documentation
${scheme_distro}/bin/plt-r6rs wiki-to-latex.scm ${wiki_pages[@]}

echo "=========================================>>> PDF Latex 1"
pdflatex -interaction=nonstopmode racr-manual.tex | grep !

echo "=========================================>>> Make Bibliography"
bibtex racr-manual

echo "=========================================>>> Make Index"
makeindex racr-manual.idx

echo "=========================================>>> PDF Latex 2"
pdflatex -interaction=nonstopmode racr-manual.tex | grep !

echo "=========================================>>> PDF Latex 3"
pdflatex -interaction=nonstopmode racr-manual.tex | grep !

if [ $clearall ]
then
	echo "=========================================>>> Delete Intermediate Results"
	for f in `ls | grep -v racr-manual.pdf`
	do
		echo "$f"
		rm $f
	done
fi

######################### Open Generated Documentation ########################

open racr-manual.pdf
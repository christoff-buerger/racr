#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# Internal script adapting (racr core) to use (racr ironscheme-hashtable-adapter),
# such that the ``empty list is null and thus invalid key for hashtable'' bug of
# IronScheme is fixed.

sed	-e 's/(import/(import (racr ironscheme-hashtable-adapter)/g' \
	-e 's/(hashtable-ref/(hashtable-ref*/g' \
	-e 's/(hashtable-set!/(hashtable-set!*/g' \
	-e 's/(hashtable-delete!/(hashtable-delete!*/g' \
	-e 's/(hashtable-contains?/(hashtable-contains?*/g' \
	-e 's/(hashtable-entries/(hashtable-entries*/g' \
	-e 's/(export/(export node-dot-net-instance node-dot-net-instance-set!/g' \
	-e 's/(mutable annotations)/(mutable annotations) (mutable dot-net-instance)/g' \
	-e '1,/(list))))))/s//(list) #f)))))/' \
	"$1/core.scm" > "$1/core.sls"

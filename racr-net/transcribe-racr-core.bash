#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

# Internal script adding the mutable node-dot-net-instance field to RACR's node record,
# such that Scheme nodes know their respective wrapping C# node. A bidirectional mapping
# between C# and Scheme nodes is required by RACR-NET for proper integration with RACR.

sed	-e 's/(export/(export node-dot-net-instance node-dot-net-instance-set!/g' \
	-e 's/(mutable annotations)/(mutable annotations) (mutable dot-net-instance)/g' \
	-e '1,/(list))))))/s//(list) #f)))))/' \
	"$1/core.scm" > "$1/core.sls"

#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

rm -rf racket-bin
plt-r6rs ++path ../../racr/racket-bin --install --collections racket-bin user-interface.scm
plt-r6rs ++path ../../racr/racket-bin --install --collections racket-bin language.scm

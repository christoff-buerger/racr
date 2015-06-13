# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

# Ironscheme interpreter alias
IS := mono ~/IronScheme/IronScheme.Console32-v2.exe

all: racr.core.dll hashtable-iron-scheme-adapter.dll Racr.dll

TMP := $(shell mktemp)

racr.core.dll hashtable-iron-scheme-adapter.dll:  ../racr/core.scm transcribe.py Makefile
	@mkdir -p racr
	@./transcribe.py
	@echo '(import (racr core))' > $(TMP)
	@echo '(compile "$(TMP)")' | $(IS)
	@echo '(cadr (library-path))' | $(IS) | awk 'NR == 2 {\
		system("mv " substr($$0,3) "/racr.core.dll .");\
		system("mv " substr($$0,3) "/hashtable-iron-scheme-adapter.dll .");\
	}'
	@rm -r racr


Racr.dll: racr.core.dll hashtable-iron-scheme-adapter.dll
	xbuild
	cp bin/Debug/Racr.dll .


clean:
	rm -f racr.core.dll hashtable-iron-scheme-adapter.dll
	rm -rf bin obj Racr.dll

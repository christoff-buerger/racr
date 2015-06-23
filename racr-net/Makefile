# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

# Find IronScheme interpreter:
ISCONSOLE := IronScheme.Console32-v2.exe
ifndef IS
$(error Path to $(ISCONSOLE) not set; use IS=path flag to set it)
else
ifeq (,$(wildcard $(IS)/$(ISCONSOLE)))
$(error $(ISCONSOLE) does not exist at $(IS))
endif
ifeq (,$(shell which mono))
$(error Missing .NET runtime environment)
endif
override IS := mono $(IS)/$(ISCONSOLE)
endif

TMP := $(shell mktemp compile-script.XXXX)

all: racr.core.dll hashtable-iron-scheme-adapter.dll Racr.dll

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

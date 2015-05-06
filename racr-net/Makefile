all: racr/racr.core.dll racr/racr.hash.dll

# Ironscheme alias
IS := is


TMP := $(shell mktemp)

racr/racr.core.dll racr/racr.hash.dll: racr/core.sls Makefile
	@echo '(import (racr core))' > $(TMP)
	@echo '(compile "$(TMP)")' | $(IS)
	@echo '(cadr (library-path))' | $(IS) | awk 'NR == 2 {\
		system("mv " substr($$0,3) "/racr.core.dll racr/racr.core.dll");\
		system("mv " substr($$0,3) "/racr.hash.dll racr/racr.hash.dll");\
	}'


racr/core.sls: ../racr/core.scm transcribe.py Makefile
	./transcribe.py


clean:
	rm -f racr/core.sls racr/racr.core.dll racr/racr.hash.dll

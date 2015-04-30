
all: core.sls testing.sls

core.sls: ../../racr/core.scm transcribe.py
	./transcribe.py

testing.sls: ../../racr/testing.scm transcribe.py
	./transcribe.py

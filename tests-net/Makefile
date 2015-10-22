# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

Test.dll: Test.cs Makefile
	mono-csc Test.cs -r:../racr-net/IronScheme.dll -r:../racr-net/Racr.dll -target:library -pkg:nunit -out:$@
	cp ../racr-net/*dll .

clean:
	rm -f *.dll

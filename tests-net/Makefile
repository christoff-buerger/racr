# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

Test.dll: Test.cs
	mono-csc $< -r:../racr-net/IronScheme.dll -target:library -pkg:nunit -out:$@

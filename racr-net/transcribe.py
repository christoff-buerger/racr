#!/usr/bin/python
# -*- coding: utf-8 -*-

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: D. Langner, C. Bürger

import re

b = False

for f in ["core", "testing"]:

	in_path = "../racr/" + f + ".scm"
	out_path = "racr/" + f + ".sls"

	in_file = file(in_path)
	out_file = file(out_path, "w")

	while 1:
		l = in_file.readline()
		if not l: break

		l = re.sub(r"(hashtable-(ref|set!|delete!|contains\?|entries))(\s)", r"\1*\3", l)
		l = re.sub(r"(\(import .*)(\))", r"\1 (hashtable-iron-scheme-adapter)\2", l)

		if f == "core":
			l = re.sub(r"(\s*)\(export\w*", r"\g<0>\n\1 node-dot-net-instance\n\1 node-dot-net-instance-set!\n", l)
			l = re.sub(r"(\s*)\(mutable annotations\)", r"\g<0>\n\1(mutable dot-net-instance)", l)


			if not b and re.search(r"\(list\)\)\)\)\)\)", l):
				b = True
				l = re.sub(r"(\s*)\(list\)", r"\g<0>\n\1#f", l)

		out_file.write(l)

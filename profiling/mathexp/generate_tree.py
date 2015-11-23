#!/usr/bin/python

"""
creates 'GeneratedX.cs' and 'scheme/generatedX.sls'
"""

import random, string, sys

cnt = int(sys.argv[-1])



def new_node():
	return [random.choice("+*"), None, None]


def add_node(n):
	i = random.randint(1, 2)
	if n[i]: add_node(n[i])
	else: n[i] = new_node()

def set_children(n):
	for i in 1, 2:
		if n[i] != None:
			set_children(n[i])
			continue
		if random.random() > 0.5:
			n[i] = random.randint(1, 9)
		else:
			n[i] = random.choice(string.lowercase)



n = new_node()
for i in range(cnt - 1): add_node(n)
set_children(n)

#import pprint
#pp = pprint.PrettyPrinter(indent=4)
#pp.pprint(n)
#d = {}
#def index(n):
#	if type(n) == str:
#		d[n] = d.get(n, 0) + 1
#	elif type(n) == list:
#		for i in 1, 2: index(n[i])
#index(n)
#pp.pprint(d)



def format_tree(n, depth=0):
	if type(n) == int:
		return "\t"* depth +"""spec.CreateAst("Number", %.1f)""" % n
	if type(n) == str:
		return "\t"* depth +"""spec.CreateAst("Const", "%s")""" % n

	t = { "+": "AddExp", "*": "MulExp" }[n[0]]
	return "\t"* depth +"""spec.CreateAst("%s",\n""" % t +",\n".join(format_tree(n[i], depth + 1) for i in (1, 2)) +")"


def scheme_format_tree(n, depth=0):
	if type(n) == int:
		return " "* depth +"""(create-ast 'Number (list %.1f))""" % n
	if type(n) == str:
		return " "* depth +"""(create-ast 'Const (list "%s"))""" % n

	t = { "+": "AddExp", "*": "MulExp" }[n[0]]
	return " "* depth +"""(create-ast '%s (list\n""" % t +"\n".join(scheme_format_tree(n[i], depth + 1) for i in (1, 2)) +"))"



open("Generated%d.cs" % cnt, "w").write("""
static class Generated {
	public static Racr.AstNode Tree(Racr.Specification spec) {
		return %s;
	}
}
""" % format_tree(n, 2)[2:])


open("scheme/generated%d.sls" % cnt, "w").write("""
(library
 (generated)
 (export tree)
 (import (rnrs) (racr core))
 (define tree
  (lambda (spec)
   (with-specification spec
%s))))
""" % scheme_format_tree(n, 4))


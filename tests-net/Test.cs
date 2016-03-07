/*
	This program and the accompanying materials are made available under the
	terms of the MIT license (X11 license) which accompanies this distribution.
	
	Author: C. Bürger, D. Langner
*/

/*
	NUnit tests to validate the proper execution of RACR's test cases in IronScheme and the
	graph rewriting adaptation of RACR in RACR-NET. The coverage of all other RACR
	functionalities in RACR-NET is validated by the C# implementation of the questionnaires
	example (cf. examples-net/questionnaires).
	
	To run the tests from command line type `nunit-console Test.dll`.
*/

using System;
using System.IO;

using NUnit.Framework;

using IronScheme;
using IronScheme.Runtime;

[TestFixture] public class RacrTests {
	public RacrTests() {
		foreach (var libpath in new String[] {
			"racr",
			"racr-meta",
			"examples/atomic-petrinets",
			"examples/composed-petrinets",
			"examples/ttc-2015-fuml-activity-diagrams",
			"examples/siple",
		}) { ("(library-path (cons \"../../" + libpath + "/ironscheme-bin\" (library-path)))").Eval(); }
	}

	[Test] public void SchemeAvailable() {
		Assert.AreEqual(3, "(+ 1 2)".Eval<int>());
	}

	[Test] public void Tests() {
		foreach (var test in new String[] {
			"ast-construction",
			"attribute-evaluation-basics",
			"continuations-in-equations",
			"patterns",
			"rewrite-basics",
			"rewrite-buds",
			"rewrite-lists",
			"rewrite-refine-abstract",
			"rewrite-strategies",
		}) { File.ReadAllText("../../tests/" + test + ".scm").Eval(); }
	}

	[Test] public void BinaryNumbers() {
		File.ReadAllText("../../examples/binary-numbers/binary-numbers.scm").Eval();
	}

	[Test] public void StateMachines() {
		File.ReadAllText("../../examples/state-machines/state-machines.scm").Eval();
	}

	[Test] public void AtomicPetrinets() {
		foreach (var test in new String[] {
			"cookie-automaton",
			"syntax-tests",
		}) { File.ReadAllText("../../examples/atomic-petrinets/examples/" + test + ".scm").Eval(); }
	}

	[Test] public void ComposedPetrinets() {
		foreach (var test in new String[] {
			"purchase-processing",
			"runtime-structure-example-slide",
			"syntax-tests",
		}) { File.ReadAllText("../../examples/composed-petrinets/examples/" + test + ".scm").Eval(); }
	}

	[Test] public void ActivityDiagrams() {
		const String path = "../../examples/ttc-2015-fuml-activity-diagrams/examples/contest-tests/";
		"(import (rnrs) (ttc-2015-fuml-activity-diagrams user-interface))".Eval();
		Callable interpretWithoutInput = "(lambda (d) (run-activity-diagram d #f 6 #f))".Eval<Callable>();
		Callable interpretWithInput = "(lambda (d i) (run-activity-diagram d i 6 #f))".Eval<Callable>();
		foreach (var diagram in new string[] {
			"test1.ad",
			"test2.ad",
			"test3.ad",
			"test4.ad",
			"testperformance_variant1.ad",
			"testperformance_variant2.ad",
			"testperformance_variant3_1.ad",
		}) { interpretWithoutInput.Call(path + diagram); }
		foreach (var diagram in new string[] {
			"test6_false.ad",
			"test6_true.ad",
			"testperformance_variant3_2.ad",
		}) { interpretWithInput.Call(path + diagram, path + diagram + "input"); }
	}

	[Test] public void Siple() {
		const String path = "../../examples/siple/examples/";
		"(import (rnrs) (racr testing) (siple main) (siple exception-api))".Eval();
		Callable interpretCorrect = "siple-interpret".Eval<Callable>();
		Callable interpretIncorrect = "(lambda (p) (assert-exception siple-exception? (siple-interpret p)))".Eval<Callable>();
		foreach (var test in new string[] {
			"abnormal_termination.siple",
			"assertions.siple",
			"boolean_arithmetics.siple",
			"closures.siple",
			"control_flow.siple",
			"integer_and_real_arithmetics.siple",
			"nested_procedures.siple",
			"pointers.siple",
			"procedure_basics.siple",
			"relational_arithmetics.siple",
			"scopes.siple",
			"type_coercions.siple",
		}) { interpretCorrect.Call(path + "correct/" + test); }
		foreach (var test in new string[] {
			"abnormal_termination.siple",
			"assertions.siple",
			"boolean_arithmetics.siple",
			"control_flow.siple",
			"integer_and_real_arithmetics.siple",
			"procedure_basics.siple",
			"relational_arithmetics.siple",
			"scopes.siple",
		}) { interpretIncorrect.Call(path + "incorrect/" + test); }
	}
}

[TestFixture] public class RewritesTests {
	private Racr.Specification spec = null;

	public RewritesTests() {
		spec = new Racr.Specification();
		spec.AstRule("S->A");
		spec.AstRule("A->a-B<B1");
		spec.AstRule("Aa:A->B<B2-c");
		spec.AstRule("B->");
		spec.CompileAstSpecifications("S");
		spec.CompileAgSpecifications();
	}

	[Test] public void TestRewriteRefine() {
		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "A", 1, new Racr.AstNode(spec, "B")));
		var A = ast.Child("A");
		var B1 = A.Child("B1");

		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 2);
		Assert.AreEqual(A.NodeType(), "A");
		Assert.AreEqual(A.Child<int>(1), 1);
		Assert.AreEqual(A.Child(2), B1);

		A.RewriteRefine("Aa", new Racr.AstNode(spec, "B"), 4);

		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 4);
		Assert.AreEqual(A.NodeType(), "Aa");
		Assert.AreEqual(A.Child<int>(1), 1);
		Assert.AreEqual(A.Child(2), B1);
		Assert.AreEqual(A.Child(3).NodeType(), "B");
		Assert.AreEqual(A.Child<int>(4), 4);
	}

	[Test] public void TestRewriteAbstract() {
		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "Aa", 1, new Racr.AstNode(spec, "B"), new Racr.AstNode(spec, "B"), 4));
		var A = ast.Child("A");
		var B1 = A.Child("B1");
		var B2 = A.Child("B2");

		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 4);
		Assert.AreEqual(A.NodeType(), "Aa");
		Assert.AreEqual(A.Child<int>(1), 1);
		Assert.AreEqual(A.Child(2), B1);
		Assert.AreEqual(A.Child(3), B2);
		Assert.AreEqual(A.Child<int>(4), 4);

		var c = A.RewriteAbstract("A");
		Assert.AreEqual(c.Length, 2);
		Assert.AreEqual(c[0], B2);
		Assert.AreEqual(c[1], 4);

		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 2);
		Assert.AreEqual(A.NodeType(), "A");
		Assert.AreEqual(A.Child<int>(1), 1);
		Assert.AreEqual(A.Child(2), B1);
	}

	[Test] public void TestRewriteSubtree() {
		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "A", 42, new Racr.AstNode(spec, "B")));
		var A = ast.Child("A");
		var B1 = A.Child("B1");

		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 2);
		Assert.AreEqual(A.NodeType(), "A");
		Assert.AreEqual(A.Child<int>(1), 42);
		Assert.AreEqual(A.Child(2), B1);

		A.RewriteSubtree(new Racr.AstNode(spec, "Aa", 1, new Racr.AstNode(spec, "B"), new Racr.AstNode(spec, "B"), 42));
		Assert.IsFalse(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 2);
		Assert.AreEqual(A.NodeType(), "A");
		Assert.AreEqual(A.Child<int>(1), 42);
		Assert.AreEqual(A.Child(2), B1);

		A = ast.Child("A");
		Assert.IsTrue(A.HasParent());
		Assert.AreEqual(A.NumChildren(), 4);
		Assert.AreEqual(A.NodeType(), "Aa");
		Assert.AreEqual(A.Child<int>(1), 1);
		Assert.AreNotEqual(A.Child(2), B1);
		Assert.AreEqual(A.Child(2).NodeType(), "B");
		Assert.AreEqual(A.Child(3).NodeType(), "B");
		Assert.AreEqual(A.Child<int>(4), 42);
	}
}

 /*
  This program and the accompanying materials are made available under the
  terms of the MIT license (X11 license) which accompanies this distribution.

  Author: D. Langner, C. Bürger
*/

using System;
using System.IO;
using IronScheme;
using IronScheme.Runtime;
using NUnit.Framework;



[TestFixture]
class RacrTests {

	const string racrPath = "../";

	static void EvalScript(string path) { File.ReadAllText(racrPath + path).Eval(); }

	Callable interpretCorrect;
	Callable interpretIncorrect;

	[SetUp]
	public void Init() {
		"(import (racr core) (racr testing))".Eval();

		// extend library path
		"(library-path (cons {0} (library-path)))".Eval(racrPath + "examples");

		// siple
		"(import (siple main) (siple exception-api))".Eval();
		interpretCorrect = "siple-interpret".Eval<Callable>();
		interpretIncorrect = @"
		(lambda (x)
		  (assert-exception
		    siple-exception?
			(siple-interpret x)))
		".Eval<Callable>();
	}

	[Test] public void SchemeAdd() 							{ Assert.AreEqual(3, "(+ 1 2)".Eval<int>()); }

	[Test] public void RacrTestAstConstruction()			{ EvalScript("tests/ast-construction.scm"); }
	[Test] public void RacrTestAttributeEvaluationBasics()	{ EvalScript("tests/attribute-evaluation-basics.scm"); }
	[Test] public void RacrTestContinuationsInEquations()	{ EvalScript("tests/continuations-in-equations.scm"); }
	[Test] public void RacrTestPatterns()					{ EvalScript("tests/patterns.scm"); }
	[Test] public void RacrTestRewriteBasics()				{ EvalScript("tests/rewrite-basics.scm"); }
	[Test] public void RacrTestRewriteBuds()				{ EvalScript("tests/rewrite-buds.scm"); }
	[Test] public void RacrTestRewriteLists()				{ EvalScript("tests/rewrite-lists.scm"); }
	[Test] public void RacrTestRewriteRefineAbstract()		{ EvalScript("tests/rewrite-refine-abstract.scm"); }
	[Test] public void RacrTestRewriteStrategies()			{ EvalScript("tests/rewrite-strategies.scm"); }


	[Test]
	public void RacrExamples() {
		foreach (var test in new string[] {
			"binary-numbers/binary-numbers.scm",
			"petrinets/examples/cookie-automaton.scm",
			"petrinets/examples/purchase-processing.scm",
			"petrinets/examples/runtime-structure-example-slide.scm",
			"state-machines/state-machines.scm",
		}) {
			var path = "examples/" + test;
			Console.WriteLine("loading {0} ...", path);
			EvalScript(path);
		}
	}

	[Test]
	public void SipleCorrectExamples() {
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
		}) {
			var path = "examples/siple/examples/correct/" + test;
			Console.WriteLine("interpreting {0} ...", path);
			interpretCorrect.Call(path);
		}
	}

	[Test]
	public void SipleIncorrectExamples() {
		foreach (var test in new string[] {
			"abnormal_termination.siple",
			"assertions.siple",
			"boolean_arithmetics.siple",
			"control_flow.siple",
			"integer_and_real_arithmetics.siple",
			"procedure_basics.siple",
			"relational_arithmetics.siple",
			"scopes.siple",
		}) {
			var path = "examples/siple/examples/incorrect/" + test;
			Console.WriteLine("interpreting {0} ...", path);
			interpretIncorrect.Call(path);
		}
	}
}




[TestFixture]
class App {



	[Test]
	public static void TestRewriteRefine() {
		var spec = new Racr.Specification();


		spec.AstRule("S->A");
		spec.AstRule("A->a");
		spec.AstRule("Aa:A->b-c");
		spec.CompileAstSpecifications("S");
		spec.CompileAgSpecifications();

		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "A", 1));
		var A = ast.Child("A");

		Console.WriteLine("{0}", A.NumChildren() == 1);
		Console.WriteLine("{0}", A.NodeType() == "A");

		A.RewriteRefine("Aa", 2, 3);

		Console.WriteLine("{0}", A.NumChildren() == 3);
		Console.WriteLine("{0}", A.NodeType() == "Aa");

		foreach (var c in A.Children()) Console.WriteLine(c);

	}

	[Test]
	public static void TestRewriteAbstract() {
		var spec = new Racr.Specification();

		spec.AstRule("S->A");
		spec.AstRule("A->a");
		spec.AstRule("Aa:A->b-c");
		spec.CompileAstSpecifications("S");
		spec.CompileAgSpecifications();

		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "Aa", 1, 2, 3));
		var A = ast.Child("A");

		Console.WriteLine(A.NumChildren() == 3);
		Console.WriteLine(A.NodeType() == "Aa");

		var c = A.RewriteAbstract("A");
		foreach (var x in c) Console.WriteLine(x);

		Console.WriteLine(A.NumChildren() == 1);
		Console.WriteLine(A.NodeType() == "A");

	}

	[Test]
	public static void TestRewriteSubtree() {
		var spec = new Racr.Specification();

		spec.AstRule("S->A");
		spec.AstRule("A->a");
		spec.AstRule("Aa:A->b-c");
		spec.CompileAstSpecifications("S");
		spec.CompileAgSpecifications();

		var ast = new Racr.AstNode(spec, "S", new Racr.AstNode(spec, "A", 42));
		var A = ast.Child("A");

		Console.WriteLine(A.HasParent());
		Console.WriteLine(ast.Child("A").NodeType());

		A.RewriteSubtree(new Racr.AstNode(spec, "Aa", 1, 2, 3));

		Console.WriteLine(A.HasParent());
		Console.WriteLine(ast.Child("A").NodeType());
	}
}



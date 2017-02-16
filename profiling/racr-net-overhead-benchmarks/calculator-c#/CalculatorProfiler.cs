/*
	This program and the accompanying materials are made available under the
	terms of the MIT license (X11 license) which accompanies this distribution.

	Author: D. Langner, C. Bürger
*/

using System;
using System.Diagnostics;

using NUnit.Framework;

using IronScheme;
using IronScheme.Runtime;
using IronScheme.Scripting;

public static class CalculatorLanguage {
	public static readonly Racr.Specification CL;

	static CalculatorLanguage() {
		CL = new Racr.Specification();

		// abstract syntax tree
		CL.AstRule("Calculator->Definition*-Expression");
		CL.AstRule("Definition->name-value");
		CL.AstRule("Expression->");
		CL.AstRule("BinaryExpression:Expression->Expression<Op1-Expression<Op2");
		CL.AstRule("Addition:BinaryExpression->");
		CL.AstRule("Multiplication:BinaryExpression->");
		CL.AstRule("Number:Expression->value");
		CL.AstRule("Constant:Expression->name");
		CL.CompileAstSpecifications("Calculator");

		CL.RegisterAgRules(typeof(CalculatorLanguage));
		CL.CompileAgSpecifications();
	}

	// accessors
	public static double GetValue(this Racr.AstNode n) {
		return n.Child<double>("value");
	}

	public static string GetName(this Racr.AstNode n) {
		return n.Child<string>("name");
	}

	public static Racr.AstNode GetExpression(this Racr.AstNode n) {
		return n.Child("Expression");
	}

	public static Racr.AstNode GetOp1(this Racr.AstNode n) {
		return n.Child("Op1");
	}

	public static Racr.AstNode GetOp2(this Racr.AstNode n) {
		return n.Child("Op2");
	}

	public static Racr.AstNode GetDefinitions(this Racr.AstNode n) {
		return n.Child("Definition*");
	}

	public static double Eval(this Racr.AstNode n) {
		return n.AttValue<double>("Eval");
	}

	public static Racr.AstNode Lookup(this Racr.AstNode n, string name) {
		return n.AttValue<Racr.AstNode>("Lookup", name);
	}

	// attributes
	[Racr.AgRule("Lookup", "Calculator", Cached = true, Context = "*")]
	private static Racr.AstNode EvalConst(Racr.AstNode node, string name) {
		return (Racr.AstNode)node.GetDefinitions().FindChild((i, d) => ((Racr.AstNode)d).GetName() == name);
	}

	[Racr.AgRule("Eval", "Constant", Cached = true, Context = "*")]
	private static double EvalConst(Racr.AstNode node) {
		return node.Lookup(node.GetName()).GetValue();
	}

	[Racr.AgRule("Eval", "Number", Cached = true, Context = "*")]
	private static double EvalNumber(Racr.AstNode node) {
		return node.GetValue();
	}
 
	[Racr.AgRule("Eval", "Addition", Cached = true, Context = "*")]
	private static double EvalAddExp(Racr.AstNode node) {
		return node.GetOp1().Eval() + node.GetOp2().Eval();
	}

	[Racr.AgRule("Eval", "Multiplication", Cached = true, Context = "*")]
	private static double EvalMulExp(Racr.AstNode node) {
		return node.GetOp1().Eval() * node.GetOp2().Eval();
	}
	
	[Racr.AgRule("Eval", "Calculator", Cached = true, Context = "*")]
	private static double EvalRoot(Racr.AstNode node) {
		return node.GetExpression().Eval();
	}
}

public static class CalculatorProfiler {
	private static readonly Racr.Specification CL = CalculatorLanguage.CL;

	static CalculatorProfiler() {
		"(import (calculator-scheme main))".Eval();
	}

	public static void Main(string[] args) {
		var nodes = args.Length > 0 ? int.Parse(args[0]) : 1000;
		var constants = args.Length > 1 ? int.Parse(args[1]) : 26;
		var rewrites = args.Length > 2 ? int.Parse(args[2]) : 1000;
		var UseRacrNet = args.Length > 3 ? bool.Parse(args[3]) : true;

		InitialiseRandom();
		if (UseRacrNet) ProfileRacrNet(nodes, constants, rewrites, true);
		else ProfileRacrScheme(nodes, constants, rewrites, true);
	}

	public static Racr.AstNode ProfileRacrNet(int nodes, int constants, int rewrites, bool printTime) {
		var ast = MakeProfilingAst(nodes, constants);
		var watch = new Stopwatch();
		watch.Start();
		for (var i = 0; i < rewrites; i++) {
			ast.Eval();
			var def = ast.Lookup("d" + i % constants);
			def.RewriteTerminal("value", (def.Child<double>("value") + 0.1) % 3.0);
		}
		watch.Stop();
		if (printTime) Console.WriteLine("{0}", watch.ElapsedMilliseconds * 0.001);
		return ast;
	}

	public static Object ProfileRacrScheme(int nodes, int constants, int rewrites, bool printTime) {
		"(define profiling-ast (make-profiling-ast {0} {1}))".Eval(nodes, constants);
		var watch = new Stopwatch();
		watch.Start();
		@"(do ((i 0 (+ i 1))) ((= i {0}))
			(=eval profiling-ast)
			(let ((def (=lookup profiling-ast (number->const (mod i {1})))))
				(rewrite-terminal 'value def
					(mod (+ (->value def) 0.1) 3.0))))"
			.Eval(rewrites, constants);
		watch.Stop();
		if (printTime) Console.WriteLine("{0}", watch.ElapsedMilliseconds * 0.001);
		return "profiling-ast".Eval();
	}

	public static void InitialiseRandom(int seed = 19380110) {
		"(random {0})".Eval(seed);
	}

	private static bool FlipCoin() {
		return (bool)"(< (random) (/ 1 2))".Eval();
	}

	private static int RandomInteger(int lb, int ub) {
		return (int)"(random-integer {0} {1})".Eval(lb, ub);
	}

	private static Racr.AstList MakeDefinitions(int constants) {
		Racr.AstNode[] defs = new Racr.AstNode[constants];
		for (var i = 0; i < constants; i++) defs[i] = CL.CreateAst("Definition", "d" + i, i / 10.0);
		return CL.CreateAstList(defs);
	}

	private static Racr.AstNode NewNode() {
		return CL.CreateAst(FlipCoin() ? "Addition" : "Multiplication", new Racr.AstBud(), new Racr.AstBud());
	}

	private static void AddNode(Racr.AstNode n) {
		Racr.AstNode c = FlipCoin() ? n.GetOp1() : n.GetOp2();
		if (c.IsBudNode()) c.RewriteSubtree(NewNode());
		else AddNode(c);
	}

	private static void InitialiseLeafes(Racr.AstNode n, int constants) {
		foreach (var c in new Racr.AstNode[] {n.GetOp1(), n.GetOp2()}) {
			if (!c.IsBudNode()) InitialiseLeafes(c, constants);
			else if (FlipCoin()) c.RewriteSubtree(CL.CreateAst("Number", (double)RandomInteger(1, 10)));
			else c.RewriteSubtree(CL.CreateAst("Constant", "d" + RandomInteger(0, constants)));
		}
	}

	private static Racr.AstNode MakeProfilingAst(int nodes, int constants) {
		var expr = NewNode();
		for (var i = 1; i < nodes; i++) AddNode(expr);
		InitialiseLeafes(expr, constants);
		return CL.CreateAst("Calculator", MakeDefinitions(constants), expr);
	}
}

[TestFixture] public class CalculatorTests {
	private void test(int nodes, int constants, int rewrites) {
		CalculatorProfiler.InitialiseRandom();
		Racr.AstNode c1 = CalculatorProfiler.ProfileRacrNet(nodes, constants, rewrites, false);
		CalculatorProfiler.InitialiseRandom();
		Object c2 = CalculatorProfiler.ProfileRacrScheme(nodes, constants, rewrites, false);
		Assert.AreEqual(
			c1.PrintAst(),
			@"(call-with-string-output-port
				(lambda (port)
					(print-ast {0} (list) port)))".Eval(c2));
		Assert.AreEqual(c1.Eval(), (double)"(=eval {0})".Eval(c2));
	}

	[Test] public void n1c1r0()		{ test(1,	1,	0);	}
	[Test] public void n1c1r10()		{ test(1,	1,	10);	}
	[Test] public void n10c1r0()		{ test(10,	1,	0);	}
	[Test] public void n10c1r10()		{ test(10,	1,	10);	}
	[Test] public void n10c10r10()		{ test(10,	10,	10);	}
	[Test] public void n99c26r0()		{ test(99,	26,	0);	}
	[Test] public void n99c26r100()		{ test(99,	26,	100);	}
	[Test] public void n99c26r1000()	{ test(99,	26,	1000);	}
	[Test] public void n256c26r1024()	{ test(256,	26,	1024);	}
}

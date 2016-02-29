/*
	This program and the accompanying materials are made available under the
	terms of the MIT license (X11 license) which accompanies this distribution.

	Author: D. Langner, C. Bürger
*/

using System;
using System.Diagnostics;

using IronScheme;
using IronScheme.Runtime;
using IronScheme.Scripting;

public static class Accessors {
	public static double GetValue(this Racr.AstNode n) {
		return n.Child<double>("value");
	}
	public static string GetName(this Racr.AstNode n) {
		return n.Child<string>("name");
	}
	public static Racr.AstNode GetExp(this Racr.AstNode n) {
		return n.Child("Exp");
	}
	public static Racr.AstNode GetA(this Racr.AstNode n) {
		return n.Child("A");
	}
	public static Racr.AstNode GetB(this Racr.AstNode n) {
		return n.Child("B");
	}
	public static Racr.AstNode GetDefs(this Racr.AstNode n) {
		return n.Child("Defs");
	}
	public static double Eval(this Racr.AstNode n) {
		return n.AttValue<double>("Eval");
	}
	public static Racr.AstNode Lookup(this Racr.AstNode n, string name) {
		return n.AttValue<Racr.AstNode>("Lookup", name);
	}
}

public static class App {
	private static Racr.Specification CL;

	static App() {
		CL = new Racr.Specification();
		CL.AstRule("Root->Def*<Defs-Exp");
		CL.AstRule("Def->name-value");
		CL.AstRule("Exp->");
		CL.AstRule("BinExp:Exp->Exp<A-Exp<B");
		CL.AstRule("AddExp:BinExp->");
		CL.AstRule("MulExp:BinExp->");
		CL.AstRule("Number:Exp->value");
		CL.AstRule("Const:Exp->name");
		CL.CompileAstSpecifications("Root");
		CL.RegisterAgRules(typeof(App));
		CL.CompileAgSpecifications();
		
		"(import (calculator-scheme main))".Eval();
	}

	private static bool FlipCoin() {
		return (bool)"(< (random) (/ 1 2))".Eval();
	}

	private static int RandomInteger(int lb, int ub) {
		return (int)"(random-integer {0} {1})".Eval(lb, ub);
	}

	private static Racr.AstList MakeDefinitions(int constants) {
		Racr.AstNode[] defs = new Racr.AstNode[constants];
		for (var i = 0; i < constants; i++) defs[i] = CL.CreateAst("Def", "d" + i, i / 10.0);
		return CL.CreateAstList(defs);
	}

	private static Racr.AstNode NewNode() {
		return CL.CreateAst(FlipCoin() ? "AddExp" : "MulExp", new Racr.AstBud(), new Racr.AstBud());
	}

	private static void AddNode(Racr.AstNode n) {
		Racr.AstNode c = FlipCoin() ? n.GetA() : n.GetB();
		if (c.IsBudNode()) c.RewriteSubtree(NewNode());
		else AddNode(c);
	}

	private static void InitialiseLeafes(Racr.AstNode n, int constants) {
		foreach (var c in new Racr.AstNode[] {n.GetA(), n.GetB()}) {
			if (!c.IsBudNode()) InitialiseLeafes(c, constants);
			else if (FlipCoin()) c.RewriteSubtree(CL.CreateAst("Number", (double)RandomInteger(1, 10)));
			else c.RewriteSubtree(CL.CreateAst("Const", "d" + RandomInteger(0, constants)));
		}
	}

	private static Racr.AstNode MakeProfilingAst(int nodes, int constants) {
		var expr = NewNode();
		for (var i = 1; i < nodes; i++) AddNode(expr);
		InitialiseLeafes(expr, constants);
		return CL.CreateAst("Root", MakeDefinitions(constants), expr);
	}

	public static void Main(string[] args) {
		var nodes = args.Length > 0 ? int.Parse(args[0]) : 1000;
		var constants = args.Length > 1 ? int.Parse(args[1]) : 26;
		var rewrites = args.Length > 2 ? int.Parse(args[2]) : 1000;
		var UseRacrNet = args.Length > 3 ? bool.Parse(args[3]) : true;
		
		if (UseRacrNet) {
			var ast = MakeProfilingAst(nodes, constants);
			var watch = new Stopwatch();
			watch.Start();
			for (var i = 0; i < rewrites; i++) {
				ast.Eval();
				var def = ast.Lookup("d" + i % constants);
				def.RewriteTerminal("value", (def.Child<double>("value") + 0.1) % 3.0);
			}
			watch.Stop();
			Console.WriteLine("{0}", watch.ElapsedMilliseconds * 0.001);
		} else {
			"(define profiling-ast (make-profiling-ast {0} {1}))".Eval(nodes, constants);
			var watch = new Stopwatch();
			watch.Start();
			@"(do ((i 0 (+ i 1))) ((= i {0}))
				(att-value 'Eval profiling-ast)
				(let ((def (att-value 'Lookup profiling-ast (number->const (mod i {1})))))
					(rewrite-terminal 'value def
						(mod (+ (ast-child 'value def) 0.1) 3.0))))"
				.Eval(rewrites, constants);
			watch.Stop();
			Console.WriteLine("{0}", watch.ElapsedMilliseconds * 0.001);
		}
	}

	[Racr.AgRule("Lookup", "Root", Cached = true, Context = "*")]
	private static Racr.AstNode EvalConst(Racr.AstNode node, string name) {
		return (Racr.AstNode)node.GetDefs().FindChild((i, d) => ((Racr.AstNode)d).GetName() == name);
	}

	[Racr.AgRule("Eval", "Const", Cached = true, Context = "*")]
	private static double EvalConst(Racr.AstNode node) {
		return node.Lookup(node.GetName()).GetValue();
	}

	[Racr.AgRule("Eval", "Number", Cached = true, Context = "*")]
	private static double EvalNumber(Racr.AstNode node) {
		return node.GetValue();
	}
 
	[Racr.AgRule("Eval", "AddExp", Cached = true, Context = "*")]
	private static double EvalAddExp(Racr.AstNode node) {
		return node.GetA().Eval() + node.GetB().Eval();
	}

	[Racr.AgRule("Eval", "MulExp", Cached = true, Context = "*")]
	private static double EvalMulExp(Racr.AstNode node) {
		return node.GetA().Eval() * node.GetB().Eval();
	}
	
	[Racr.AgRule("Eval", "Root", Cached = true, Context = "*")]
	private static double EvalRoot(Racr.AstNode node) {
		return node.GetExp().Eval();
	}
}

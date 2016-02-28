using System;
using System.Diagnostics;


static class Accessors {
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

class App {
	public static void Main() {


		var spec = new Racr.Specification();

		spec.AstRule("Root->Def*<Defs-Exp");
		spec.AstRule("Def->name-value");
		spec.AstRule("Exp->");
		spec.AstRule("BinExp:Exp->Exp<A-Exp<B");
		spec.AstRule("AddExp:BinExp->");
		spec.AstRule("MulExp:BinExp->");
		spec.AstRule("Number:Exp->value");
		spec.AstRule("Const:Exp->name");
		spec.CompileAstSpecifications("Root");
		
		spec.RegisterAgRules(typeof(App));
		spec.CompileAgSpecifications();

		var defs = spec.CreateAstList(
				spec.CreateAst("Def", "a", 0.0),
				spec.CreateAst("Def", "b", 0.1),
				spec.CreateAst("Def", "c", 0.2),
				spec.CreateAst("Def", "d", 0.3),
				spec.CreateAst("Def", "e", 0.4),
				spec.CreateAst("Def", "f", 0.5),
				spec.CreateAst("Def", "g", 0.6),
				spec.CreateAst("Def", "h", 0.7),
				spec.CreateAst("Def", "i", 0.8),
				spec.CreateAst("Def", "j", 0.9),
				spec.CreateAst("Def", "k", 1.0),
				spec.CreateAst("Def", "l", 1.1),
				spec.CreateAst("Def", "m", 1.2),
				spec.CreateAst("Def", "n", 1.3),
				spec.CreateAst("Def", "o", 1.4),
				spec.CreateAst("Def", "p", 1.5),
				spec.CreateAst("Def", "q", 1.6),
				spec.CreateAst("Def", "r", 1.7),
				spec.CreateAst("Def", "s", 1.8),
				spec.CreateAst("Def", "t", 1.9),
				spec.CreateAst("Def", "u", 2.0),
				spec.CreateAst("Def", "v", 2.1),
				spec.CreateAst("Def", "w", 2.2),
				spec.CreateAst("Def", "x", 2.3),
				spec.CreateAst("Def", "y", 2.4),
				spec.CreateAst("Def", "z", 2.5));

		var exp = Generated.Tree(spec);

		var root = spec.CreateAst("Root", defs, exp);

		Console.WriteLine("Start");
		var watch = new Stopwatch();
		watch.Start();

		for (int i = 0; i < 1000; i++) {
			var def = defs.Child(i % 26 + 1);
			Console.WriteLine("{0}: {1}", i, root.Eval());
			def.RewriteTerminal("value", (def.Child<double>("value") + 0.1) % 3.0);
		}

		watch.Stop();
		Console.WriteLine("Time: {0}", watch.ElapsedMilliseconds * 0.001);
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


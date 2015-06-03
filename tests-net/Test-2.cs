/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.Diagnostics;


class B : Racr.AstNode {
	public B(Racr.Specification spec, string term) : base(spec, "B", term) {}
}

static class Extensions {
	public static Racr.AstNode GetList(this Racr.AstNode n) { return n.Child("List"); }
	public static string GetT(this Racr.AstNode n) { return n.Child<string>("t"); }

	public static int	M(this Racr.AstNode n) { return (int)	n.AttValue("M"); }
	public static bool	N(this Racr.AstNode n) { return (bool)	n.AttValue("N"); }
}


class MySpec : Racr.Specification {


	[Racr.AgRule("M", "A")]
	static int AM(Racr.AstNode n) { return n.NumChildren() * 2; }
	[Racr.AgRule("N", "A")]
	static bool AN(Racr.AstNode n) { return !n.HasParent(); }

	[Racr.AgRule("N", "B")]
	static bool BN(Racr.AstNode n) { return !n.HasParent(); }


	public MySpec() {

		AstRule("A->B*<List-C-w");
		AstRule("B->t");
		AstRule("C->");

		CompileAstSpecifications("A");
/*
		SpecifyAttribute("kids", "A", "*", false, (Racr.AstNode n) => {
			return n.GetList().NumChildren();
		});


		SpecifyAttribute("foo", "B", "*", false, (Racr.AstNode n, int x) => {
			return n.NumChildren() * x;
		});


		SpecifyAttribute("bar", "B", "*", false, (Racr.AstNode n, int x) => {
			return x * x;
		});
*/

		RegisterAgRules();
		CompileAgSpecifications();
	}

}


class App {
	

	static void TestFoo() {

		var spec = new MySpec();

		var root = new Racr.AstNode(spec, "A",
		                            new Racr.AstList(
			new B(spec, "abc"),
			new B(spec, "123"),
			new B(spec, "xyz")),
		                            new Racr.AstNode(spec, "C"),
		                            "hiya");


		root.ForEachChild((i, o) => {
			var node = o as Racr.AstNode;
			if (node != null)
				Console.WriteLine("{0}: {1}", i, node.NodeType());
			else
				Console.WriteLine("{0}: {1}", i, o);
		}, new Racr.Range(2));


		Console.WriteLine("---");


		Console.WriteLine("M: {0}", root.M());
		Console.WriteLine("N: {0}", root.N());

		Console.WriteLine("a: {0}", root.AttValue("a"));
		Console.WriteLine("b: {0}", root.AttValue("b", 3));

		Console.WriteLine("kids: {0}", root.AttValue("kids"));
		var index = root.GetList().Child(1).AttValue<int>("bar", 3);
		Console.WriteLine("index: {0}", index);


		Console.WriteLine("---");


		var c = root.FindChild((i, o) => {
			return i == 2;
		}) as Racr.AstNode;

		Console.WriteLine("{0}", c.NodeType());


		Console.WriteLine("---");

		Console.WriteLine(root);
		Console.WriteLine("NodeType: {0}", root.NodeType());
		Console.WriteLine("IsNode: {0}", root.IsNode());
		Console.WriteLine("HasParent: {0}", root.HasParent());
		Console.WriteLine("NumChildren: {0}", root.NumChildren());
		Console.WriteLine("HasChild 'B: {0}", root.HasChild("B"));
		Console.WriteLine("HasChild 'List: {0}", root.HasChild("List"));

		Console.WriteLine("");

		var child = root.GetList();

		Console.WriteLine(child);
		Console.WriteLine("Child 1 -> t: {0}", child.Child(1).GetT());
		Console.WriteLine("IsNode: {0}", child.IsNode());
		Console.WriteLine("HasParent: {0}", child.HasParent());
		Console.WriteLine("ChildIndex: {0}", child.ChildIndex());
		Console.WriteLine("NumChildren: {0}", child.NumChildren());

	}



	static void TestRewriteRefine() {
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

	static void TestRewriteAbstract() {
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

	static void TestRewriteSubtree() {
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


	public static void Main() {

		TestRewriteRefine();
		TestRewriteAbstract();

//		TestRewriteSubtree();

	}

}


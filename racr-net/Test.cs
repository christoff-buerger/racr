using System;

class App {


	public static void Main() {


		var spec = new Racr.Specification();
		spec.AstRule("A->B*<List-C-w");
		spec.AstRule("B->t");
		spec.AstRule("C->");


		spec.CompileAstSpecifications("A");


		spec.SpecifyAttribute("foo", "B", "*", false, (Racr.AstNode n, int x) => {
			return n.NumChildren() * x;
		});

		spec.SpecifyAttribute("bar", "B", "*", false, (Racr.AstNode n, int x) => {
			return x * x;
		});

		spec.SpecifyAttribute<MyBNode,int,int>("FooAttribute", "B", "*", false, MyBNode.FooAttribute, false);


		spec.CompileAgSpecifications();


		var root = new Racr.AstNode(spec, "A",
			new Racr.AstList(
				new MyBNode(spec, "abc"),
				new MyBNode(spec, "123"),
				new MyBNode(spec, "xyz")),
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

		var index = (int) root.GetList().Child(1).AttValue("bar", 3);
		Console.WriteLine(index);

		Console.WriteLine(root.GetList().Child(1).AttValue("FooAttribute", 3));

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
		Console.WriteLine("HasChild 'Foo: {0}", root.HasChild("Foo"));

		Console.WriteLine("");

		var child = root.GetList();

		Console.WriteLine(child);
		Console.WriteLine("Child 1 -> t: {0}", child.Child(1).Child<string>("t"));
		Console.WriteLine("IsNode: {0}", child.IsNode());
		Console.WriteLine("HasParent: {0}", child.HasParent());
		Console.WriteLine("ChildIndex: {0}", child.ChildIndex());
		Console.WriteLine("NumChildren: {0}", child.NumChildren());

	}

}


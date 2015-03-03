using System;
using IronScheme;
using IronScheme.Runtime;
using IronScheme.Scripting;


static class Racr {

	private static Callable createSpecification;
	private static Callable astRule;
	private static Callable compileAstSpecifications;
	private static Callable compileAgSpecifications;
	private static Callable createAst;
	private static Callable createAstList;
	private static Callable createAstBud;
	private static Callable astParent;
	private static Callable astChild;
	private static Callable astSibling;
	private static Callable astNodeQ;
	private static Callable astHasParentQ;
	private static Callable astChildIndex;
	private static Callable astHasChildQ;
	private static Callable astNumChildren;
	private static Callable astHasSiblingQ;
	private static Callable astNodeType;
	private static Callable astNodeRule;
	private static Callable astListNodeQ;
	private static Callable astBudNodeQ;
	private static Callable astSubtypeQ;


	private static Callable astAnnotationSet;
	private static Callable astWeaveAnnotations;
	private static Callable astAnnotationRemove;
	private static Callable astAnnotationQ;
	private static Callable astAnnotation;


	static Racr() {
		//"(library-path (cons {0} (library-path)))".Eval("../racr-repo/examples");

		"(import (racr core) (racr testing))".Eval();

		// ast
		createSpecification			= "create-specification".Eval<Callable>();
		astRule						= "ast-rule".Eval<Callable>();
		compileAstSpecifications	= "compile-ast-specifications".Eval<Callable>();
		compileAgSpecifications		= "compile-ag-specifications".Eval<Callable>();
		createAst					= "create-ast".Eval<Callable>();
		createAstList				= "create-ast-list".Eval<Callable>();
		createAstBud				= "create-ast-bud".Eval<Callable>();
		astParent					= "ast-parent".Eval<Callable>();
		astChild					= "ast-child".Eval<Callable>();
		astSibling					= "ast-sibling".Eval<Callable>();
		astNodeQ					= "ast-node?".Eval<Callable>();
		astHasParentQ				= "ast-has-parent?".Eval<Callable>();
		astChildIndex				= "ast-child-index".Eval<Callable>();
		astHasChildQ				= "ast-has-child?".Eval<Callable>();
		astNumChildren				= "ast-num-children".Eval<Callable>();
		astHasSiblingQ				= "ast-has-sibling?".Eval<Callable>();
		astNodeType					= "ast-node-type".Eval<Callable>();
		astNodeRule					= "ast-node-rule".Eval<Callable>();
		astListNodeQ				= "ast-list-node?".Eval<Callable>();
		astBudNodeQ					= "ast-bud-node?".Eval<Callable>();
		astSubtypeQ					= "ast-subtype?".Eval<Callable>();

		// ast annotations
		astAnnotationSet			= "ast-annotation-set!".Eval<Callable>();
		astWeaveAnnotations			= "ast-weave-annotations".Eval<Callable>();
		astAnnotationRemove			= "ast-annotation-remove!".Eval<Callable>();
		astAnnotationQ				= "ast-annotation?".Eval<Callable>();
		astAnnotation				= "ast-annotation".Eval<Callable>();


	}


	public class Specification {
		private object spec;

		public Specification() {
			spec = createSpecification.Call();
		}

		public void AstRule(string rule) {
			astRule.Call(spec, SymbolTable.StringToObject(rule));
		}

		public void CompileAstSpecifications(string startSymbol) {
			compileAstSpecifications.Call(spec, SymbolTable.StringToObject(startSymbol));
		}

		public void CompileAgSpecifications() {
			compileAgSpecifications.Call(spec);
		}

		public Node CreateAst(string nonTerminal, params Node[] children) {
			Cons list = null;
			for (int i = children.Length - 1; i >= 0; i--) {
				list = new Cons(children[0].ast, list);
			}
			return new Node(createAst.Call(spec, SymbolTable.StringToObject(nonTerminal), list));
		}

		public Node CreateAstList(params Node[] children) {
			Cons list = null;
			for (int i = children.Length - 1; i >= 0; i--) {
				list = new Cons(children[0].ast, list);
			}
			return new Node(createAstList.Call(list));
		}

		public Node CreateAstBud() {
			return new Node(createAstBud.Call());
		}
	}


	public class Node {
		internal object ast;

		internal static Node GetNode(object ast) {
			return (Node) astAnnotation.Call(ast, SymbolTable.StringToObject("this"));
		}

		internal Node(object ast) {
			this.ast = ast;
			SetAnnotation("this", this);
		}
		public Node Parent() {
			return GetNode(astParent.Call(ast));
		}
		public Node Child(int index) {
			return GetNode(astChild.Call(index, ast));
		}
		public Node Child(string name) {
			return GetNode(astChild.Call(SymbolTable.StringToObject(name), ast));
		}
		public Node Sibling(int index) {
			return GetNode(astSibling.Call(index, ast));
		}
		public Node Sibling(string name) {
			return GetNode(astSibling.Call(SymbolTable.StringToObject(name), ast));
		}
		public bool IsNode() {
			return (bool) astNodeQ.Call(ast);
		}
		public bool HasParent() {
			object ret = astHasParentQ.Call(ast);
			if (ret is bool) return (bool) ret;
			return true;
		}
		public int ChildIndex() {
			return (int) astChildIndex.Call(ast);
		}
		public bool HasChild(string name) {
			return (bool) astHasChildQ.Call(SymbolTable.StringToObject(name), ast);
		}
		public int NumChildren() {
			return (int) astNumChildren.Call(ast);
		}


		// ast annotations
		public void SetAnnotation(string name, object v) {
			astAnnotationSet.Call(ast, SymbolTable.StringToObject(name), v);
		}
		public object GetAnnotation(string name) {
			return astAnnotation.Call(ast, SymbolTable.StringToObject(name));
		}
		public void RemoveAnnotation(string name) {
			astAnnotationRemove.Call(ast, SymbolTable.StringToObject(name));
		}
		public bool IsAnnotation(string name) {
			return (bool) astAnnotationQ.Call(ast, SymbolTable.StringToObject(name));
		}
		public void WeaveAnnotations(string type, string name, object v) {
			astWeaveAnnotations.Call(ast, SymbolTable.StringToObject(type), SymbolTable.StringToObject(name), v);
		}
	}


}



class App {


	public static void Main() {

		var spec = new Racr.Specification();
		spec.AstRule("A->B");
		spec.AstRule("B->");

//		spec.AstRule("A->D");
//		spec.AstRule("B:A->");
//		spec.AstRule("C:A->A-A*-t-B-B*");
//		spec.AstRule("D->");

		spec.CompileAstSpecifications("A");
		spec.CompileAgSpecifications();
		Racr.Node root = spec.CreateAst("A", spec.CreateAst("B"));

		Console.WriteLine(root);
		Console.WriteLine("IsNode: {0}", root.IsNode());
		Console.WriteLine("HasParent: {0}", root.HasParent());
		Console.WriteLine("NumChildren: {0}", root.NumChildren());
		Console.WriteLine("HasChild: {0}", root.HasChild("B"));
		Console.WriteLine("HasChild: {0}", root.HasChild("Foo"));


		Console.WriteLine("");

		var child = root.Child(1);
		Console.WriteLine(child);
		Console.WriteLine("IsNode: {0}", child.IsNode());
		Console.WriteLine("HasParent: {0}", child.HasParent());
		Console.WriteLine("ChildIndex: {0}", child.ChildIndex());
		Console.WriteLine("NumChildren: {0}", child.NumChildren());

	}

}

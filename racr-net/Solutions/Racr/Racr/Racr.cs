using System;
using System.Collections.Generic;
//using System.Dynamic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Reflection.Emit;

using IronScheme;
using IronScheme.Runtime;
using IronScheme.Scripting;

static public class Racr {

	private static Callable nodeDotNetInstance;
	private static Callable nodeDotNetInstanceSet;

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
	private static Callable astChildren;
	private static Callable astForEachChild;
	private static Callable astFindChild;
	private static Callable astFindChildA;
	private static Callable astNodeQ;
	private static Callable astHasParentQ;
	private static Callable astChildIndex;
	private static Callable astHasChildQ;
	private static Callable astNumChildren;
	private static Callable astHasSiblingQ;
	private static Callable astNodeType;
	private static Callable astListNodeQ;
	private static Callable astBudNodeQ;
	private static Callable astSubtypeQ;

	private static Callable specifyAttribute;
	private static Callable attValue;

	private static Callable rewriteTerminal;

	private static Callable astAnnotationSet;
	private static Callable astWeaveAnnotations;
	private static Callable astAnnotationRemove;
	private static Callable astAnnotationQ;
	private static Callable astAnnotation;

	private static Callable specificationFindAstRule;
	private static Callable astRuleProduction;
	private static Callable symbolIsNonTerminal;


	static Racr() {
		//"(library-path (cons {0} (library-path)))".Eval("../racr-repo/examples");

		"(import (racr core) (racr testing))".Eval();

		// bridge
		nodeDotNetInstance			= "node-dot-net-instance".Eval<Callable>();
		nodeDotNetInstanceSet		= "node-dot-net-instance-set!".Eval<Callable>();


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
		astChildren					= "ast-children".Eval<Callable>();
		astForEachChild				= "ast-for-each-child".Eval<Callable>();
		astFindChild				= "ast-find-child".Eval<Callable>();
		astFindChildA				= "ast-find-child*".Eval<Callable>();
		astNodeQ					= "ast-node?".Eval<Callable>();
		astHasParentQ				= "ast-has-parent?".Eval<Callable>();
		astChildIndex				= "ast-child-index".Eval<Callable>();
		astHasChildQ				= "ast-has-child?".Eval<Callable>();
		astNumChildren				= "ast-num-children".Eval<Callable>();
		astHasSiblingQ				= "ast-has-sibling?".Eval<Callable>();
		astNodeType					= "ast-node-type".Eval<Callable>();
		astListNodeQ				= "ast-list-node?".Eval<Callable>();
		astBudNodeQ					= "ast-bud-node?".Eval<Callable>();
		astSubtypeQ					= "ast-subtype?".Eval<Callable>();


		// attribution
		specifyAttribute			= "specify-attribute".Eval<Callable>();
		attValue					= "att-value".Eval<Callable>();

		// rewriting
		rewriteTerminal				= "rewrite-terminal".Eval<Callable>();

		// ast annotations
		astAnnotationSet			= "ast-annotation-set!".Eval<Callable>();
		astWeaveAnnotations			= "ast-weave-annotations".Eval<Callable>();
		astAnnotationRemove			= "ast-annotation-remove!".Eval<Callable>();
		astAnnotationQ				= "ast-annotation?".Eval<Callable>();
		astAnnotation				= "ast-annotation".Eval<Callable>();


		// query interface
		specificationFindAstRule	= "specification->find-ast-rule".Eval<Callable>();
		astRuleProduction			= "ast-rule->production".Eval<Callable>();
		symbolIsNonTerminal			= "symbol->non-terminal?".Eval<Callable>();
	}




	public class Specification {
		internal object spec;

		public Specification() {
			spec = createSpecification.Call();
		}
		public void AstRule(string rule) {
			astRule.Call(spec, SymbolTable.StringToObject(rule));
		}
		public void CompileAstSpecifications(string startSymbol) {
			compileAstSpecifications.Call(spec, SymbolTable.StringToObject(startSymbol));
		}

		public void RegisterAgRules() {
			RegisterAgRules(this.GetType());
		}
		public void RegisterAgRules(Type type=null) {
			var classes = type.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic);
			foreach (var @class in classes) {
				if (@class.BaseType != typeof(object)) continue;

				var methods = @class.GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
				foreach (var method in methods) {
					if (method.DeclaringType != @class || method.Name[0] == '<') continue;

					string contextName = "*";
					bool cached = true;

					foreach (var attr in method.GetCustomAttributes(false)) {
						var contextNameAttr = attr as ContextNameAttribute;
						if (contextNameAttr != null) contextName = contextNameAttr.NonTerminal;
						var uncachedAttr = attr as UncachedAttribute;
						if (uncachedAttr != null) cached = !uncachedAttr.Uncached;
					}

					//Console.WriteLine("{0} {1} {2}", method.Name, @class.Name, contextName);

					specifyAttribute.Call(
						spec,
						SymbolTable.StringToObject(method.Name),
						SymbolTable.StringToObject(@class.Name),
						SymbolTable.StringToObject(contextName),
						cached,
						WrapToCallable(method),
						false);

				}
			}
		}

		public void CompileAgSpecifications() { compileAgSpecifications.Call(spec); }
		

		static readonly Type[][] paramTypesArray = new Type[][] {
			new Type[] {},
			new Type[] { typeof(object) },
			new Type[] { typeof(object), typeof(object) },
			new Type[] { typeof(object), typeof(object), typeof(object) },
			new Type[] { typeof(object), typeof(object), typeof(object), typeof(object) },
			new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object) },
			// ...
		};

		static readonly Type[] callTargets = new Type[] {
			typeof(CallTarget0),
			typeof(CallTarget1),
			typeof(CallTarget2),
			typeof(CallTarget3),
			typeof(CallTarget4),
			typeof(CallTarget5),
			// ...
		};

		static Callable WrapToCallable(MethodInfo method) {
			var paramTypes = method.GetParameters().Select(p => p.ParameterType).ToArray();
			if (paramTypes.Length == 0 || !typeof(AstNode).IsAssignableFrom(paramTypes[0])) {
				throw new ArgumentException("type of delegate's first argument must be AstNode.");
			}

			var outType = callTargets[paramTypes.Length];
			var dynmeth = new DynamicMethod("", typeof(object), paramTypesArray[paramTypes.Length], true);
			var gen = dynmeth.GetILGenerator();

			gen.Emit(OpCodes.Ldarg_0);
			var getNodeInfo = ((Delegate)(Func<object, AstNode>)GetNode).Method;
			gen.Emit(OpCodes.Call, getNodeInfo);

			for (int i = 1; i < paramTypes.Length; i++) {
				gen.Emit(OpCodes.Ldarg_S, i);
				if (paramTypes[i].IsValueType) gen.Emit(OpCodes.Unbox_Any, paramTypes[i]);
			}
			gen.Emit(OpCodes.Call, method);
			if (method.ReturnType.IsValueType) gen.Emit(OpCodes.Box, method.ReturnType);
			gen.Emit(OpCodes.Ret);

			return Closure.Create(dynmeth.CreateDelegate(callTargets[paramTypes.Length]), paramTypes.Length);
		}

		// TODO circDef!!!
		public void SpecifyAttribute(string attName, string nonTerminal, string contextName, bool cached, Delegate equation) {

			specifyAttribute.Call(
				spec,
				SymbolTable.StringToObject(attName),
				SymbolTable.StringToObject(nonTerminal),
				SymbolTable.StringToObject(contextName),
				cached,
				WrapToCallable(equation.Method),
				false);
		}

		public void SpecifyAttribute<N,R>(string attName, string nonTerminal, string contextName, bool cached, Func<N,R> equation)
		where N : AstNode {
			SpecifyAttribute(attName, nonTerminal, contextName, cached, (Delegate) equation);
		}
		public void SpecifyAttribute<N,A1,R>(string attName, string nonTerminal, string contextName, bool cached, Func<N,A1,R> equation)
		where N : AstNode {
			SpecifyAttribute(attName, nonTerminal, contextName, cached, (Delegate) equation);
		}


		public object Scheme(string lambda) {
			return lambda.Eval<Callable>().Call(spec);
		}
	}


	public struct Range {
		public int min;
		public int max;
		public Range(int min, int max) {
			this.min = min;
			this.max = max;
		}
		public Range(int min) {
			this.min = min;
			this.max = 0;
		}
		internal Cons ToCons() {
			return new Cons(min, max > 0 ? max : SymbolTable.StringToObject("*"));
		}
	}

	public static bool IsTrue(object o) {
		return (o is bool) ? (bool) o : true;
	}

	private static AstNode GetNode(object ast) {
		return nodeDotNetInstance.Call(ast) as AstNode;
	}

	public class AstNode /*: DynamicObject*/ {
		//internal object ast;
		public object ast;
		private bool[] nonTermChilren;		// are children non-terminal?
		protected AstNode() {}

		public AstNode(Specification spec, string nonTerminal, params object[] children) {

			var nt = SymbolTable.StringToObject(nonTerminal);
			var rule = specificationFindAstRule.Call(spec.spec, nt);
			var symbols = astRuleProduction.Call(rule) as Cons;

			nonTermChilren = new bool[children.Length];

			Cons list = null;
			Cons marker = null;
			for (int i = 0; i < children.Length; i++) {
				symbols = symbols.cdr as Cons;
				nonTermChilren[i] = IsTrue(symbolIsNonTerminal.Call(symbols.car));
				var child = nonTermChilren[i] ? (children[i] as AstNode).ast : children[i];
				var cons = new Cons(child);
				if (list == null) list = marker = cons;
				else {
					marker.cdr = cons;
					marker = cons;
				}
			}
			ast = createAst.Call(spec.spec, nt, list);
			nodeDotNetInstanceSet.Call(ast, this);
		}

		public AstNode Parent() {
			return GetNode(astParent.Call(ast));
		}

		// non-terminal
		public AstNode Child(int index) {
			return GetNode(astChild.Call(index, ast));
		}
		public AstNode Child(string name) {
			return GetNode(astChild.Call(SymbolTable.StringToObject(name), ast));
		}
		public AstNode Sibling(int index) {
			return GetNode(astSibling.Call(index, ast));
		}
		public AstNode Sibling(string name) {
			return GetNode(astSibling.Call(SymbolTable.StringToObject(name), ast));
		}


		// terminal
		public T Child<T>(int index) {
			return (T) astChild.Call(index, ast);
		}
		public T Child<T>(string name) {
			return (T) astChild.Call(SymbolTable.StringToObject (name), ast);
		}
		public T Sibling<T>(int index) {
			return (T) astSibling.Call(index, ast);
		}
		public T Sibling<T>(string name) {
			return (T) astSibling.Call(SymbolTable.StringToObject(name), ast);
		}


		public bool IsNode() {
			return (bool) astNodeQ.Call(ast);
		}
		public bool HasParent() {
			object ret = astHasParentQ.Call(ast);
			return IsTrue(ret);
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
		public bool HasSibling(string name) {
			return (bool) astHasSiblingQ.Call(SymbolTable.StringToObject(name), ast);
		}
		public string NodeType() {
			return ((SymbolId) astNodeType.Call(ast)).ToString();
		}
		public bool IsListNode() {
			return (bool) astListNodeQ.Call(ast);
		}
		public bool IsBudNode() {
			return (bool) astBudNodeQ.Call(ast);
		}


		public virtual object[] Children(params Range[] bounds) {
			object[] l = new object[bounds.Length + 1];
			l[0] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 1] = bounds[i].ToCons();
			var e = astChildren.Call(l) as Cons;
			var children = new List<object>();
			int j = 0;
			while (e != null) {
				children.Add(nonTermChilren[j] ? GetNode(e.car) : e.car);
				e = e.cdr as Cons;
				j++;
			}
			return children.ToArray();
		}
		public virtual void ForEachChild(Action<int, object> f, params Range[] bounds) {
			Func<int, object, object> wrap = (i, n) => {
				f(i, nonTermChilren[i - 1] ? GetNode(n) : n);
				return Builtins.Unspecified;
			};
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			astForEachChild.Call(l);
		}
		public virtual object FindChild(Func<int, object, bool> f, params Range[] bounds) {
			int index = 0;
			Func<int, object, bool> wrap = (i, n) => {
				bool found = f(i, nonTermChilren[i - 1] ? GetNode(n) : n);
				if (found) index = i;
				return found;
			};
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			var ret = astFindChild.Call(l);
			return (index > 0 && nonTermChilren[index - 1]) ? GetNode(ret) : ret;
		}
		public virtual object FindChildA(Func<int, object, object> f, params Range[] bounds) {
			Func<int, object, object> wrap = (i, n) => {
				return f(i, nonTermChilren[i - 1] ? GetNode(n) : n);
			};
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			return astFindChildA.Call(l);
		}


		// attribution
		public object AttValue(string attName, params object[] args) {
			var l = new object[args.Length + 2];
			l[0] = SymbolTable.StringToObject(attName);
			l[1] = ast;
			args.CopyTo(l, 2);
			return attValue.Call(l);
		}
		public T AttValue<T>(string attName, params object[] args) {
			return (T) AttValue(attName, args);
		}


		// rewriting
		public void RewriteTerminal(string name, object newValue) {
			rewriteTerminal.Call(SymbolTable.StringToObject(name), ast, newValue);
		}
		public void RewriteTerminal(int index, object newValue) {
			rewriteTerminal.Call(index, ast, newValue);
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



/*
		public override bool TryGetMember(GetMemberBinder binder, out Object result) {
			result = Child(binder.Name);
			return true;
		}
*/
	}

	public class AstList : AstNode {
		public AstList(params AstNode[] children) {
			Cons list = null;
			for (int i = children.Length - 1; i >= 0; i--) list = new Cons(children[i].ast, list);
			ast = createAstList.Call(list);
			nodeDotNetInstanceSet.Call(ast, this);
		}

		public override object[] Children(params Range[] bounds) {
			object[] l = new object[bounds.Length + 1];
			l[0] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 1] = bounds[i].ToCons();
			var e = astChildren.Call(l) as Cons;
			var children = new List<object>();
			while (e != null) {
				children.Add(GetNode (e.car));
				e = e.cdr as Cons;
			}
			return children.ToArray();
		}
		public override void ForEachChild(Action<int, object> f, params Range[] bounds) {
			Func<int, object, object> wrap = (i, n) => {
				f(i, GetNode(n));
				return Builtins.Unspecified;
			};
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			astForEachChild.Call(l);
		}
		public override object FindChild(Func<int, object, bool> f, params Range[] bounds) {
			Func<int, object, bool> wrap = (i, n) => { return f(i, GetNode(n)); };
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			return GetNode(astFindChild.Call(l));
		}
		public override object FindChildA(Func<int, object, object> f, params Range[] bounds) {
			Func<int, object, object> wrap = (i, n) => { return f(i, GetNode(n)); };
			object[] l = new object[2 + bounds.Length];
			l[0] = wrap.ToSchemeProcedure();
			l[1] = ast;
			for (int i = 0; i < bounds.Length; i++) l[i + 2] = bounds[i].ToCons();
			return astFindChildA.Call(l);
		}
	}

	public class AstBud : AstNode {
		public AstBud() {
			ast = createAstBud.Call();
			nodeDotNetInstanceSet.Call(ast, this);
		}
	}

	public static bool IsSubtype(this string t, AstNode n) {
		return (bool) astSubtypeQ.Call(SymbolTable.StringToObject(t), n.ast);
	}
	public static bool IsSubtype(this AstNode n, string t) {
		return (bool) astSubtypeQ.Call(n.ast, SymbolTable.StringToObject(t));
	}
	public static bool IsSubtype(this AstNode n1, AstNode n2) {
		return (bool) astSubtypeQ.Call(n1.ast, n2.ast);
	}

	[AttributeUsage(AttributeTargets.Method)]
	public class ContextNameAttribute : Attribute {
		public readonly string NonTerminal;
		public ContextNameAttribute(string nonTerminal) {
			NonTerminal = nonTerminal;
		}
	}
	[AttributeUsage(AttributeTargets.Method)]
	public class UncachedAttribute : Attribute {
		public readonly bool Uncached;
		public UncachedAttribute(bool uncached=true) {
			Uncached = uncached;
		}
	}
}
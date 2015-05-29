/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Windows.Forms;

enum ValueTypes { Boolean, String, Number, ErrorType }

class QL : Racr.Specification {
	static QL ql;

	public static QL Ql {
		get {
			if (ql == null) ql = new QL();
			return ql;
		}
	}

	QL() {
		AstRule("Form->Element*<Body");
		AstRule("Element->");
		AstRule("Group:Element->Expression-Element*<Body");
		AstRule("Question:Element->name-label");
		AstRule("OrdinaryQuestion:Question->type-value");
		AstRule("ComputedQuestion:Question->Expression");
		AstRule("Expression->");
		AstRule("Use:Expression->name");
		AstRule("Constant:Expression->value");
		AstRule("Computation:Expression->operator-Expression*<Operands");
		CompileAstSpecifications("Form");
		RegisterAgRules();
		RegisterAgRules(typeof(WidgetAttributeMethods));
		RegisterAgRules(typeof(RenderAttributeMethods));
		CompileAgSpecifications();
	}

	static Racr.AstNode findL(string name, Racr.AstNode l, int i) {
		for (int j = 1; j <= i; j++) {
			var r = l.Child(j).LLookup(name);
			if (r != null) return r;
		}
		return null;
	}

	[Racr.AgRule("Root", "Form")]
	static Racr.AstNode FormRoot(Racr.AstNode n) { return n; }
	[Racr.AgRule("ErrorQuestion", "Form")]
	static Racr.AstNode FormErrorQuestion(Racr.AstNode n) { return n.GetBody().Child(1); }
	[Racr.AgRule("IsValid", "Form")]
	static bool FormIsValid(Racr.AstNode n) {
		return n.GetBody().Children(new Racr.Range(2)).All(x => ((Racr.AstNode)x).IsValid());
	}
	[Racr.AgRule("GLookup", "Form", Context = "Body")]
	static Racr.AstNode FromGLookup(Racr.AstNode n, string name) {
		var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
		return (ret != null) ? ret : n.ErrorQuestion();
	}
	[Racr.AgRule("IsActive", "Form")]
	static bool FormIsActive(Racr.AstNode n) { return true; }


	[Racr.AgRule("IsErrorQuestion", "Element")]
	static bool ElementIsErrorQuestion(Racr.AstNode n) { return n == n.ErrorQuestion(); }
	[Racr.AgRule("FindActive", "Element")]
	static Racr.AstNode ElementFindActive(Racr.AstNode n, string name) {
		var current = n.GLookup(name);
		while (!current.IsActive()) current = current.GLookup(name);
		return current;
	}
	[Racr.AgRule("IsShown", "Element")]
	static bool ElementIsShown(Racr.AstNode n) { return !n.IsErrorQuestion() && n.IsActive(); }

	[Racr.AgRule("GLookup", "Group", Context = "Body")]
	static Racr.AstNode GroupGLookup(Racr.AstNode n, string name) {
		var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
		return (ret != null) ? ret : n.Parent().GLookup(name);
	}

	[Racr.AgRule("LLookup", "Group")]
	static Racr.AstNode GroupLLookup(Racr.AstNode n, string name) {
		return findL(name, n.GetBody(), n.GetBody().NumChildren());
	}
	[Racr.AgRule("IsValid", "Group")]
	static bool GroupIsValid(Racr.AstNode n) {
		return n.IsLValid() && n.GetBody().Children().All(x => ((Racr.AstNode)x).IsValid());
	}
	[Racr.AgRule("IsLValid", "Group")]
	static bool GroupIsLValid(Racr.AstNode n) { return n.GetExpression().Type() == ValueTypes.Boolean; }
	[Racr.AgRule("IsActive", "Group")]
	static bool GroupIsActive(Racr.AstNode n) {
		var v = n.GetExpression().Value();
		if (v == null) return false;
		return (bool) v;
	}



	[Racr.AgRule("LLookup", "Question")]
	static Racr.AstNode QuestionLLookup(Racr.AstNode n, string name) {
		if (n.GetName() == name) return n;
		return null;
	}
	[Racr.AgRule("IsValid", "Question")]
	static bool QuestionIsValid(Racr.AstNode n) { return n.IsLValid(); }
	[Racr.AgRule("IsLValid", "Question")]
	static bool QuestionIsLValid(Racr.AstNode n) {
		if (n.Type() == ValueTypes.ErrorType) return false;
		var prev = n.GLookup(n.GetName());
		return prev.IsErrorQuestion() || n.Type() == prev.Type();
	}
	[Racr.AgRule("IsActive", "Question")]
	static bool QuestionIsActive(Racr.AstNode n) {
		return n.IsErrorQuestion() || (n.Parent().IsActive() && n.FindActive(n.GetName()).IsErrorQuestion());
	}

	[Racr.AgRule("Type", "OrdinaryQuestion")]
	static ValueTypes OrdinaryQuestionType(Racr.AstNode n) { return n.GetValueType(); }
	[Racr.AgRule("Value", "OrdinaryQuestion")]
	static object OrdinaryQuestionValue(Racr.AstNode n) { return n.GetValue(); }



	[Racr.AgRule("Type", "ComputedQuestion")]
	static ValueTypes ComputedQuestionType(Racr.AstNode n) { return n.GetExpression().Type(); }
	[Racr.AgRule("Value", "ComputedQuestion")]
	static object ComputedQuestionValue(Racr.AstNode n) { return n.GetExpression().Value(); }

	[Racr.AgRule("Type", "Use")]
	static ValueTypes UseType(Racr.AstNode n) { return n.GLookup(n.GetName()).Type(); }
	[Racr.AgRule("Value", "Use")]
	static object UseValue(Racr.AstNode n) { return n.FindActive(n.GetName()).Value(); }

	[Racr.AgRule("Type", "Constant")]
	static ValueTypes ConstantType(Racr.AstNode n) {
		var val = n.GetValue();
		if (val is bool) return ValueTypes.Boolean;
		if (val is double) return ValueTypes.Number;
		if (val is string) return ValueTypes.String;
		return ValueTypes.ErrorType;
	}
	[Racr.AgRule("Value", "Constant")]
	static object ConstantValue(Racr.AstNode n) { return n.GetValue(); }

	[Racr.AgRule("Type", "Computation")]
	static ValueTypes ComputationType(Racr.AstNode n) {
		var op = n.GetOperator();
		var inType = ValueTypes.ErrorType;
		var outType = ValueTypes.ErrorType;
		var operands = n.GetOperands().Children();
		if (op == "&&" || op == "//" || op == "not") inType = outType = ValueTypes.Boolean;
		else if (op == "=" || op == "<" || op ==  ">" || op == "<=" || op == ">=" || op == "!=") {
			inType = ValueTypes.Number;
			outType = ValueTypes.Boolean;
		}
		else if (op == "string=?" || op == "string<?" || op ==  "string>?" || op == "string<=?" || op == "string>=?") {
			inType = ValueTypes.String;
			outType = ValueTypes.Boolean;
		}
		else if (op == "+" || op == "-" || op == "*" || op == "/") inType = outType = ValueTypes.Number;
		else if (op == "string-append") inType = outType = ValueTypes.String;
		if (operands.Any(x => ((Racr.AstNode) x).Type() != inType)) return ValueTypes.ErrorType;
		return outType;
	}

	static private readonly Dictionary<string, Func<object[], object>> opTable = new Dictionary<string, Func<object[], object>>() {
		{ "&&", (object[] l) => { return l.All(x => (bool) x); } },
		{ "//", (object[] l) => { return l.Any(x => (bool) x); } },
		{ "not", (object[] l) => { return !l.All(x => (bool) x); } },
		{ "+", (object[] l) => { return l.Aggregate(0.0, (s, x) => s + (double) x); } },
		{ "-", (object[] l) => { return l.Skip(1).Aggregate((double)l[0], (s, x) => s - (double) x); } },
		{ "*", (object[] l) => { return l.Aggregate(1.0, (s, x) => s * (double) x); } },
		{ "/", (object[] l) => { return l.Skip(1).Aggregate((double)l[0], (s, x) => s / (double) x); } },
		{ "!=", (object[] l) => {
			var s = new HashSet<double>();
			foreach (var x in l) if (!s.Add((double) x)) return false;
			return true;
		} },
		{ "=", (object[] l) => { return l.All(x => (double) x == (double) l[0]); } },
		{ "<", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if ((double)l[i-1] >= (double)l[i]) return false;
			return true;
		} },
		{ "<=", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if ((double)l[i-1] > (double)l[i]) return false;
			return true;
		} },
		{ ">", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if ((double)l[i-1] <= (double)l[i]) return false;
			return true;
		} },
		{ ">=", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if ((double)l[i-1] < (double)l[i]) return false;
			return true;
		} },
		{ "string=?", (object[] l) => { return l.All(x => (string) x == (string) l[0]); } },
		{ "string<?", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if (StringComparer.Ordinal.Compare(l[i-1], l[i]) >= 0) return false;
			return true;
		} },
		{ "string<=?", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if (StringComparer.Ordinal.Compare(l[i-1], l[i]) > 0) return false;
			return true;
		} },
		{ "string>?", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if (StringComparer.Ordinal.Compare(l[i-1], l[i]) <= 0) return false;
			return true;
		} },
		{ "string>=?", (object[] l) => {
			for (int i = 1; i < l.Length; i++) if (StringComparer.Ordinal.Compare(l[i-1], l[i]) < 0) return false;
			return true;
		} },
		{ "string-append", (object[] l) => { return l.Aggregate("", (s, x) => s + (string) x); } },
	};
	[Racr.AgRule("Value", "Computation")]
	static object ComputationValue(Racr.AstNode n) {
		var op = n.GetOperator();
		var operands = n.GetOperands().Children();
		var args = operands.Select(p => ((Racr.AstNode) p).Value()).ToArray();
		var func = opTable[op];
		try { return func(args); }
		catch { return null; }
	}

	[Racr.AgRule("SExpr", "Form")]
	static string FormSExpr(Racr.AstNode n) {
		return "(Form " + String.Join(" ", n.GetBody().Children().Skip(1).Select(x => ((Racr.AstNode)x).SExpr())) + ")";
	}
	[Racr.AgRule("SExpr", "Group")]
	static string GroupSExpr(Racr.AstNode n) {
		return "(If " + n.GetExpression().SExpr() + " " + String.Join(" ", n.GetBody().Children().Select(x => ((Racr.AstNode)x).SExpr())) + ")";
	}
	[Racr.AgRule("SExpr", "OrdinaryQuestion")]
	static string OrdinaryQuestionSExpr(Racr.AstNode n) {
		return "(?? '" + n.GetName() + " " + Lexer.EscapeString(n.GetLabel()) + " " + n.Type() + " " + Lexer.EscapeValue(n.Value()) + ")";
	}
	[Racr.AgRule("SExpr", "ComputedQuestion")]
	static string ComputedQuestionSExpr(Racr.AstNode n) {
		return "(~? '" + n.GetName() + " " + Lexer.EscapeString(n.GetLabel()) + " " + n.GetExpression().SExpr() + ")";
	}
	[Racr.AgRule("SExpr", "Use")]
	static string UseSExpr(Racr.AstNode n) {
		return "(~> '" + n.GetName() + ")";
	}
	[Racr.AgRule("SExpr", "Constant")]
	static string ConstantSExpr(Racr.AstNode n) {
		return "(~! " + Lexer.EscapeValue(n.Value()) + ")";
	}
	[Racr.AgRule("SExpr", "Computation")]
	static string ComputationSExpr(Racr.AstNode n) {
		return "(~~ " + n.GetOperator() + " " + String.Join(" ", n.GetOperands().Children().Select(x => ((Racr.AstNode)x).SExpr())) + ")";
	}
}


class Questionnaire {

	static public void UpdateQuestions(Racr.AstNode n) {
		switch (n.NodeType()) {
		case "Form":
		case "Group":
			foreach (var c in n.GetBody().Children()) UpdateQuestions(c as Racr.AstNode);
			break;
		case "ComputedQuestion":
			break;
		default:
			(n.Widget() as Widget).Set(n.Value());
			break;
		}
	}

	[STAThread]
	public static void Main(string[] args) {
		Racr.AstNode ast;
		if (args.Length == 1) {
			var parser = new Parser(QL.Ql, File.ReadAllText(args[0]));
			ast = parser.ParseAst();
		}
		else ast = QL.Ql.CreateAst("Form", QL.Ql.CreateAstList());
		ast.Render();
		UpdateQuestions(ast);
		Application.Run();
	}
}

/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.Linq;
using System.Collections.Generic;

using Ast = Racr.AstNode;
using AgRule = Racr.AgRuleAttribute;

static class QuestionnairesLanguage {
	public static Racr.Specification QL;

	static QuestionnairesLanguage() {
		QL = new Racr.Specification();

		QL.AstRule("Form->Element*<Body");
		QL.AstRule("Element->");
		QL.AstRule("Group:Element->Expression-Element*<Body");
		QL.AstRule("Question:Element->name-label");
		QL.AstRule("OrdinaryQuestion:Question->type-value");
		QL.AstRule("ComputedQuestion:Question->Expression");
		QL.AstRule("Expression->");
		QL.AstRule("Use:Expression->name");
		QL.AstRule("Constant:Expression->value");
		QL.AstRule("Computation:Expression->operator-Expression*<Operands");
		QL.CompileAstSpecifications("Form");

		QL.RegisterAgRules(typeof(QuestionnairesLanguage));
		QL.RegisterAgRules(typeof(QuestionnairesGui));
		QL.CompileAgSpecifications();
	}

	// AST Accessors:

	public static Ast GetBody(this Ast n)			{ return n.Child("Body"); }
	public static Ast GetExpression(this Ast n)		{ return n.Child("Expression"); }
	public static string GetName(this Ast n)		{ return n.Child<string>("name"); }
	public static string GetLabel(this Ast n)		{ return n.Child<string>("label"); }
	public static Types GetValueType(this Ast n)		{ return n.Child<Types>("type"); }
	public static object GetValue(this Ast n)		{ return n.Child<object>("value"); }
	public static Ast GetOperands(this Ast n)		{ return n.Child("Operands"); }
	public static string GetOperator(this Ast n)		{ return n.Child<string>("operator"); }

	// Attribute Accessors:

	public static Ast Root(this Ast n)			{ return n.AttValue<Ast>("Root"); }
	public static Ast ErrorQuestion(this Ast n)		{ return n.AttValue<Ast>("ErrorQuestion"); }
	public static bool IsErrorQuestion(this Ast n)		{ return n.AttValue<bool>("IsErrorQuestion"); }
	public static Ast GLookup(this Ast n, string name)	{ return n.AttValue<Ast>("GLookup", name); }
	public static Ast LLookup(this Ast n, string name)	{ return n.AttValue<Ast>("LLookup", name); }
	public static Types Type(this Ast n)			{ return n.AttValue<Types>("Type"); }
	public static bool IsValid(this Ast n)			{ return n.AttValue<bool>("IsValid"); }
	public static bool IsLValid(this Ast n)			{ return n.AttValue<bool>("IsLValid"); }
	public static string SExpr(this Ast n)			{ return n.AttValue<string>("SExpr"); }
	public static bool IsActive(this Ast n)			{ return n.AttValue<bool>("IsActive"); }
	public static bool IsShown(this Ast n)			{ return n.AttValue<bool>("IsShown"); }
	public static Ast FindActive(this Ast n, string name)	{ return n.AttValue<Ast>("FindActive", name); }
	public static object Value(this Ast n)			{ return n.AttValue<object>("Value"); }

	// Rewriting:

	public static void SetValue(this Ast n, object value)	{ n.RewriteTerminal("value", value); }

	// Support Attributes:
	
	[AgRule("Root", "Form")] static Ast FormRoot(Ast n) {
		return n;
	}

	[AgRule("ErrorQuestion", "Form")] static Ast FormErrorQuestion(Ast n) {
		return n.GetBody().Child(1);
	}

	[AgRule("IsErrorQuestion", "Element")] static bool ElementIsErrorQuestion(Ast n) {
		return n == n.ErrorQuestion();
	}

	// Name Analysis:

	static Ast findL(string name, Ast l, int i) {
		for (int j = 1; j <= i; j++) {
			var r = l.Child(j).LLookup(name);
			if (r != null) return r;
		}
		return null; }

	[AgRule("GLookup", "Form", Context = "Body")] static Ast FormGLookup(Ast n, string name) {
		var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
		return (ret != null) ? ret : n.ErrorQuestion();
	}

	[AgRule("GLookup", "Group", Context = "Body")] static Ast GroupGLookup(Ast n, string name) {
		var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
		return (ret != null) ? ret : n.Parent().GLookup(name);
	}

	[AgRule("LLookup", "Question")] static Ast QuestionLLookup(Ast n, string name) {
		if (n.GetName() == name) return n;
		return null;
	}
	
	[AgRule("LLookup", "Group")] static Ast GroupLLookup(Ast n, string name) {
		return findL(name, n.GetBody(), n.GetBody().NumChildren());
	}

	// Type Analysis:

	[AgRule("Type", "OrdinaryQuestion")] static Types OrdinaryQuestionType(Ast n) {
		var type = n.GetValueType();
		return (TypeToAcceptor(type) != null) ? type : Types.ErrorType;
	}

	[AgRule("Type", "ComputedQuestion")] static Types ComputedQuestionType(Ast n) {
		return n.GetExpression().Type();
	}

	[AgRule("Type", "Use")] static Types UseType(Ast n) {
		return n.GLookup(n.GetName()).Type();
	}

	[AgRule("Type", "Constant")] static Types ConstantType(Ast n) {
		return ValueToType(n.GetValue());
	}

	[AgRule("Type", "Computation")] static Types ComputationType(Ast n) {
		var op = n.GetOperator();
		var inType = Types.ErrorType;
		var outType = Types.ErrorType;
		var operands = n.GetOperands().Children();
		if (op == "&&" || op == "//" || op == "not") {
			inType = outType = Types.Boolean;
		} else if (op == "=" || op == "<" || op ==  ">" || op == "<=" ||
			 op == ">=" || op == "!=") {
			inType = Types.Number;
			outType = Types.Boolean;
		} else if (op == "+" || op == "-" || op == "*" || op == "/") {
			inType = outType = Types.Number;
		} else if (op == "string-append") {
			inType = outType = Types.String;
		} else if (op == "string=?" || op == "string<?" || op ==  "string>?" ||
			 op == "string<=?" || op == "string>=?") {
			inType = Types.String;
			outType = Types.Boolean;
		}
		if (operands.Any(x => ((Ast) x).Type() != inType)) return Types.ErrorType;
		return outType;
	}

	// Well-formedness:

	[AgRule("IsValid", "Form")] static bool FormIsValid(Ast n) {
		return n.GetBody().Children(new Racr.Range(2)).All(x => ((Ast)x).IsValid());
	}

	[AgRule("IsValid", "Group")] static bool GroupIsValid(Ast n) {
		return n.IsLValid() && n.GetBody().Children().All(x => ((Ast)x).IsValid());
	}

	[AgRule("IsValid", "Question")] static bool QuestionIsValid(Ast n) {
		return n.IsLValid();
	}

	[AgRule("IsLValid", "Group")] static bool GroupIsLValid(Ast n) {
		return n.GetExpression().Type() == Types.Boolean;
	}

	[AgRule("IsLValid", "Question")] static bool QuestionIsLValid(Ast n) {
		if (n.Type() == Types.ErrorType) return false;
		var prev = n.GLookup(n.GetName());
		return prev.IsErrorQuestion() || n.Type() == prev.Type();
	}

	// Persistency:

	[AgRule("SExpr", "Form")] static string FormSExpr(Ast n) {
		return "(Form " + String.Join(" ",
			n.GetBody().Children().Skip(1).Select(x => ((Ast)x).SExpr())) + ")";
	}

	[AgRule("SExpr", "Group")] static string GroupSExpr(Ast n) {
		return "(If " + n.GetExpression().SExpr() + " " +
			String.Join(" ", n.GetBody().Children().Select(x => ((Ast)x).SExpr())) + ")";
	}

	[AgRule("SExpr", "OrdinaryQuestion")] static string OrdinaryQuestionSExpr(Ast n) {
		return "(?? '" + n.GetName() + " " + Lexer.EscapeString(n.GetLabel()) + " " + n.Type() + " " +
			(n.GetValue() == ErrorValue ? "" : Lexer.EscapeValue(n.GetValue())) + ")";
	}

	[AgRule("SExpr", "ComputedQuestion")] static string ComputedQuestionSExpr(Ast n) {
		return "(~? '" + n.GetName() + " " + Lexer.EscapeString(n.GetLabel()) +
			" " + n.GetExpression().SExpr() + ")";
	}

	[AgRule("SExpr", "Use")] static string UseSExpr(Ast n) {
		return "(~> '" + n.GetName() + ")";
	}

	[AgRule("SExpr", "Constant")] static string ConstantSExpr(Ast n) {
		return "(~! " + Lexer.EscapeValue(n.GetValue()) + ")";
	}

	[AgRule("SExpr", "Computation")] static string ComputationSExpr(Ast n) {
		return "(~~ " + n.GetOperator() + " " +
			String.Join(" ", n.GetOperands().Children().Select(x => ((Ast)x).SExpr())) + ")";
	}

	// Interpretation:

	[AgRule("FindActive", "Element")] static Ast ElementFindActive(Ast n, string name) {
		var current = n.GLookup(name);
		while (!current.IsActive()) current = current.GLookup(name);
		return current;
	}

	[AgRule("IsActive", "Form")] static bool FormIsActive(Ast n) {
		return true;
	}

	[AgRule("IsActive", "Group")] static bool GroupIsActive(Ast n) {
		var v = n.GetExpression().Value();
		return (v is bool) ? (bool) v : false;
	}

	[AgRule("IsActive", "Question")] static bool QuestionIsActive(Ast n) {
		return n.IsErrorQuestion() || (
			n.Parent().IsActive() &&
			n.FindActive(n.GetName()).IsErrorQuestion());
	}

	[AgRule("IsShown", "Element")] static bool ElementIsShown(Ast n) {
		return !n.IsErrorQuestion() && n.IsActive();
	}

	[AgRule("Value", "ComputedQuestion")] static object ComputedQuestionValue(Ast n) {
		return n.GetExpression().Value();
	}

	[AgRule("Value", "Constant")] static object ConstantValue(Ast n) {
		return n.Type() == Types.ErrorType ? ErrorValue : n.GetValue();
	}

	[AgRule("Value", "Use")] static object UseValue(Ast n) {
		if (n.Type() == Types.ErrorType) return ErrorValue;
		var active = n.FindActive(n.GetName());
		return active.Type() == n.Type() ? active.Value() : ErrorValue;
	}

	[AgRule("Value", "OrdinaryQuestion")] static object OrdinaryQuestionValue(Ast n) {
		if (n.Type() == Types.ErrorType) return ErrorValue;
		var acceptor = TypeToAcceptor(n.Type()); // acceptor is never null
		return acceptor(n.GetValue()) ? n.GetValue() : ErrorValue;
	}

	[AgRule("Value", "Computation")] static object ComputationValue(Ast n) {
		if (n.Type() == Types.ErrorType) return ErrorValue;
		var args = n.GetOperands().Children().Select(p => ((Ast) p).Value()).ToArray();
		foreach (var arg in args) if (arg == ErrorValue) return ErrorValue;
		try { return opTable[n.GetOperator()](args); }
		catch { return ErrorValue; }
	}

	public static readonly object ErrorValue = new Object();

	public enum Types { Boolean, Number, String, ErrorType }

	public static Func<object,bool> TypeToAcceptor(Types t) {
		switch (t) {
		case Types.Boolean: return v => v is bool;
		case Types.Number: return v => v is double;
		case Types.String: return v => v is string;
		default: return null;
		}
	}

	public static Types ValueToType(object v) {
		if (v is bool) return Types.Boolean;
		if (v is double) return Types.Number;
		if (v is string) return Types.String;
		return Types.ErrorType;
	}

	private static readonly Dictionary<string, Func<object[], object>> opTable =
		new Dictionary<string, Func<object[], object>>() {
		{ "&&", (object[] l) => {
			return l.All(x => (bool) x); } },
		{ "//", (object[] l) => {
			return l.Any(x => (bool) x); } },
		{ "not", (object[] l) => {
			return !l.All(x => (bool) x); } },
		{ "+", (object[] l) => {
			return l.Aggregate(0.0, (s, x) => s + (double) x); } },
		{ "-", (object[] l) => {
			return l.Skip(1).Aggregate((double)l[0], (s, x) => s - (double) x); } },
		{ "*", (object[] l) => {
			return l.Aggregate(1.0, (s, x) => s * (double) x); } },
		{ "/", (object[] l) => {
			return l.Skip(1).Aggregate((double)l[0], (s, x) => s / (double) x); } },
		{ "!=", (object[] l) => {
			var s = new HashSet<double>();
			foreach (var x in l)
				if (!s.Add((double) x)) return false;
			return true;
		} },
		{ "=", (object[] l) => {
			return l.All(x => (double) x == (double) l[0]); } },
		{ "<", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if ((double)l[i-1] >= (double)l[i]) return false;
			return true;
		} },
		{ "<=", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if ((double)l[i-1] > (double)l[i]) return false;
			return true;
		} },
		{ ">", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if ((double)l[i-1] <= (double)l[i]) return false;
			return true;
		} },
		{ ">=", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if ((double)l[i-1] < (double)l[i]) return false;
			return true;
		} },
		{ "string=?", (object[] l) => {
			return l.All(x => (string) x == (string) l[0]); } },
		{ "string<?", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if (StringComparer.Ordinal.Compare(l[i-1], l[i]) >= 0) return false;
			return true;
		} },
		{ "string<=?", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if (StringComparer.Ordinal.Compare(l[i-1], l[i]) > 0) return false;
			return true;
		} },
		{ "string>?", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if (StringComparer.Ordinal.Compare(l[i-1], l[i]) <= 0) return false;
			return true;
		} },
		{ "string>=?", (object[] l) => {
			for (int i = 1; i < l.Length; i++)
				if (StringComparer.Ordinal.Compare(l[i-1], l[i]) < 0) return false;
			return true;
		} },
		{ "string-append", (object[] l) => {
			return l.Aggregate("", (s, x) => s + (string) x); } },
	};
}

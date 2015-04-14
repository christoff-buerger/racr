using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;


enum ValueTypes { Boolean, String, Number, ErrorType }


static class Accessors {
	// AST Accessors
	public static Racr.AstNode GetBody(this Racr.AstNode n) { return n.Child("Body"); }
	public static Racr.AstNode GetExpression(this Racr.AstNode n) { return n.Child("Expression"); }
	public static string GetName(this Racr.AstNode n) { return n.Child<string>("name"); }
	public static ValueTypes GetValueType(this Racr.AstNode n) { return n.Child<ValueTypes>("type"); }
	public static object GetValue(this Racr.AstNode n) { return n.Child<object>("value"); }
	public static Racr.AstNode GetOperands(this Racr.AstNode n) { return n.Child("Operands"); }
	public static string GetOperator(this Racr.AstNode n) { return n.Child<string>("operantor"); }


	// Attribute Accessors
	public static Racr.AstNode Root(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("Root"); }
	public static Racr.AstNode ErrorQuestion(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("ErrorQuestion"); }
	public static Racr.AstNode GLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("GLookup", name); }
	public static Racr.AstNode LLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("LLookup", name); }
	public static Racr.AstNode FindActive(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("FindActive", name); }

	public static bool IsErrorQuestion(this Racr.AstNode n) { return n.AttValue<bool>("IsErrorQuestion"); }
	public static bool IsValid(this Racr.AstNode n) { return n.AttValue<bool>("IsValid"); }
	public static bool IsLValid(this Racr.AstNode n) { return n.AttValue<bool>("IsLValid"); }
	public static bool IsActive(this Racr.AstNode n) { return n.AttValue<bool>("IsActive"); }
	public static bool IsShown(this Racr.AstNode n) { return n.AttValue<bool>("IsShown"); }

	public static ValueTypes Type(this Racr.AstNode n) { return n.AttValue<ValueTypes>("Type"); }
}


class QL : Racr.Specification {

	public QL() {
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
		CompileAgSpecifications();
	}




	static Racr.AstNode findL(string name, Racr.AstNode l, int i) {
		return l.FindChildA((int j, object e) => {
			return ((Racr.AstNode) e).LLookup(name);
		}, new Racr.Range(1, i)) as Racr.AstNode;
	}

	static class Form {
		static Racr.AstNode Root(Racr.AstNode n) { return n; }
		static Racr.AstNode ErrorQuestion(Racr.AstNode n) { return n.GetBody().Child(1); }
		static bool IsValid(Racr.AstNode n) {
			return n.GetBody().Children(new Racr.Range(2)).All(x => ((Racr.AstNode)x).IsValid());
		}

		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n, string name) {
			var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
			if (ret != null) return ret;
			return n.ErrorQuestion();
		}
	}

	static class Element {
		static bool IsErrorQuestion(Racr.AstNode n) { return n == n.ErrorQuestion(); }
	}

	static class Group {
		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n, string name) {
			var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
			if (ret != null) return ret;
			return n.ErrorQuestion();
		}

		static Racr.AstNode LLookup(Racr.AstNode n, string name) {
			return findL(name, n.GetBody(), n.GetBody().NumChildren());
		}
		static bool IsValid(Racr.AstNode n) {
			return n.IsLValid() && n.GetBody().Children().All(x => ((Racr.AstNode)x).IsValid());
		}
		static bool IsLValid(Racr.AstNode n) { return n.GetExpression().Type() == ValueTypes.Boolean; }
	}

	static class Question {
		static Racr.AstNode LLookup(Racr.AstNode n, string name) {
			if (n.GetName() == name) return n;
			return null;
		}
		static bool IsValid(Racr.AstNode n) { return n.IsLValid(); }
		static bool IsLValid(Racr.AstNode n) {
			if (n.Type() == ValueTypes.ErrorType) return false;
			var prev = n.GLookup(n.GetName());
			return prev.IsErrorQuestion() || n.Type() == prev.Type();
		}
	}

	static class OrdinaryQuestion {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GetValueType();
		}
	}

	static class ComputedQuestion {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GetExpression().Type();
		}
	}

	static class Use {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GLookup(n.GetName()).Type();
		}
	}

	static class Constant {
		static ValueTypes Type(Racr.AstNode n) {
			var val = n.GetValue();
			if (val is bool) return ValueTypes.Boolean;
			if (val is double) return ValueTypes.Number;
			if (val is string) return ValueTypes.String;
			return ValueTypes.ErrorType;
		}
	}

	static class Computation {
		static ValueTypes Type(Racr.AstNode n) {
			var op = n.GetOperator();
			var inType = ValueTypes.ErrorType;
			var outType = ValueTypes.ErrorType;

			var operands = n.GetOperands().Children() as Racr.AstNode[];

			if (op == "&&" || op == "//" || op == "not") inType = outType = ValueTypes.Boolean;
			else if (op == "=" || op == "<" || op ==  ">" || op == "<=" || op == ">=" || op == "!=") {
				var t = operands[0].Type();
				if (t != ValueTypes.Number && t != ValueTypes.String) return ValueTypes.ErrorType;
				inType = t;
				outType = ValueTypes.Boolean;
			}
			else if (op == "+" || op == "-" || op == "*" || op == "/") inType = outType = ValueTypes.Number;
			else if (op == ".") inType = outType = ValueTypes.String;

			if (operands.Any(x => x.Type() != inType)) return ValueTypes.ErrorType;
			return outType;
		}
	}


}


class Lexer {

	public enum Lexemes {
		LeftParenthesis,
		RightParenthesis,
		String,
		Symbol,
		Identifier
	};


	public Lexer(string src) {
		source = src;
		position = 0;
		NextChar();
	}


	public Lexemes NextLexeme() {
		Lexemes l = lexeme;
		lexeme = Scan();
		return l;
	}


	private char NextChar() {
		char c = character;
		if (position >= source.Length) character = '\0';
		else character = source[position++];
		return c;
	}

	private Lexemes Scan() {
		while (Char.IsWhiteSpace(character)) NextChar();
		switch (character) {
		case '(': NextChar(); return Lexemes.LeftParenthesis;
		case ')': NextChar(); return Lexemes.RightParenthesis;
		default: break;
		}

/*
		if (Char.IsDigit(character)) {
			var token = "";
			while (isdigit(character)) token += next_character();
			if (character == '.') {
				token += '.';
				next_character();
				while (isdigit(character)) token += next_character();
			}
		}
*/

		return Lexemes.Identifier;
	}



	private char character;
	private Lexemes lexeme;

	private string source;
	private int position;
}



class Questionnaire {

	static QL ql;

	public static void Main(string[] args) {

		string path;

		if (args.Length == 1) path = args[0];
		else {
			Console.Write("Enter name: ");
			path = Console.ReadLine();
		}

		var input = File.OpenText(path).ReadToEnd();
		Console.WriteLine(input);


		ql = new QL();

	}
}

using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Windows.Forms;


enum ValueTypes { Boolean, String, Number, ErrorType }


static class Accessors {
	// AST Accessors
	public static Racr.AstNode GetBody(this Racr.AstNode n) { return n.Child("Body"); }
	public static Racr.AstNode GetExpression(this Racr.AstNode n) { return n.Child("Expression"); }
	public static string GetName(this Racr.AstNode n) { return n.Child<string>("name"); }
	public static string GetLabel(this Racr.AstNode n) { return n.Child<string>("label"); }
	public static ValueTypes GetValueType(this Racr.AstNode n) { return n.Child<ValueTypes>("type"); }
	public static object GetValue(this Racr.AstNode n) { return n.Child<object>("value"); }
	public static Racr.AstNode GetOperands(this Racr.AstNode n) { return n.Child("Operands"); }
	public static string GetOperator(this Racr.AstNode n) { return n.Child<string>("operator"); }


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
	public static object Value(this Racr.AstNode n) { return n.AttValue<object>("Value"); }

	public static Control Widget(this Racr.AstNode n) { return n.AttValue<Control>("Widget"); }
	public static bool Render(this Racr.AstNode n) {
		Console.WriteLine("Render {0}", n.NodeType());
		return n.AttValue<bool>("Render");
	}

	// Rewriting
	public static void SetValue(this Racr.AstNode n, object value) { return n.RewriteTerminal("value", value); }
}


abstract class Widget : FlowLayoutPanel {
	public Widget(string label) {
		AutoSize = true;
		WrapContents = false;
		FlowDirection = FlowDirection.LeftToRight;
		this.label = new System.Windows.Forms.Label();
		this.label.Text = label;
		this.label.AutoSize = true;
		this.label.Anchor = AnchorStyles.Left;
		Controls.Add(this.label);
	}
	public abstract void Set(object v);
	private System.Windows.Forms.Label label;
	public virtual TextBox GetTextBox() { return null; }
	public virtual CheckBox GetCheckBox() { return null; }
}
class TextWidget : Widget {
	public TextWidget(string label) : base(label) {
		tb = new TextBox();
		Controls.Add(tb);
	}
	public override void Set(object v) {
		tb.Text = (v == null) ? "#f" : (string) v;
	}
	public override TextBox GetTextBox() { return tb; }
	private TextBox tb;
}
class CheckWidget : Widget {
	public CheckWidget(string label) : base(label) {
		cb = new CheckBox();
		Controls.Add(cb);
	}
	public override void Set(object v) {
		cb.Checked = (v == null) ? false : (bool) v;
	}
	public override CheckBox GetCheckBox() { return cb; }
	private CheckBox cb;
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
		for (int j = 1; j <= i; j++) {
			var r = l.Child(j).LLookup(name);
			if (r != null) return r;
		}
		return null;
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
			return (ret != null) ? ret : n.ErrorQuestion();
		}
		static bool IsActive(Racr.AstNode n) { return true; }
		static Control Widget(Racr.AstNode n) {

			var form = new System.Windows.Forms.Form();
			form.Text = "Questionnaire";

			// TODO: menu

			var panel = new FlowLayoutPanel();
			panel.AutoSize = true;
			panel.Dock = DockStyle.Fill;
			panel.FlowDirection = FlowDirection.TopDown;
			panel.WrapContents = false;

			form.Controls.Add(panel);

			return panel;
		}
		static bool Render(Racr.AstNode n) {
			foreach (var c in n.GetBody().Children()) {
				var child = c as Racr.AstNode;
				if (child.IsShown()) {
					var w = child.Widget();
					child.Render();
					w.Show();
				}
				else child.Widget().Hide();
			}
			return true;
		}
	}

	static class Element {
		static bool IsErrorQuestion(Racr.AstNode n) { return n == n.ErrorQuestion(); }
		static Racr.AstNode FindActive(Racr.AstNode n, string name) {
			var current = n.GLookup(name);
			while (!current.IsActive()) current = current.GLookup(name);
			return current;
		}
		static bool IsShown(Racr.AstNode n) { return !n.IsErrorQuestion() && n.IsActive(); }
	}

	static class Group {
		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n, string name) {
			var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
			return (ret != null) ? ret : n.Parent().GLookup(name);
		}

		static Racr.AstNode LLookup(Racr.AstNode n, string name) {
			return findL(name, n.GetBody(), n.GetBody().NumChildren());
		}
		static bool IsValid(Racr.AstNode n) {
			return n.IsLValid() && n.GetBody().Children().All(x => ((Racr.AstNode)x).IsValid());
		}
		static bool IsLValid(Racr.AstNode n) { return n.GetExpression().Type() == ValueTypes.Boolean; }
		static bool IsActive(Racr.AstNode n) {
			return (bool) n.GetExpression().Value();
		}
		static Control Widget(Racr.AstNode n) {
			var panel = new FlowLayoutPanel();
			panel.BorderStyle = BorderStyle.Fixed3D;
			panel.Dock = DockStyle.Fill;
			panel.FlowDirection = FlowDirection.TopDown;
			panel.WrapContents = false;
			n.Parent().Widget().Controls.Add(panel);
			return panel;
		}
		static bool Render(Racr.AstNode n) {
			foreach (var c in n.GetBody().Children()) {
				var child = c as Racr.AstNode;
				if (child.IsShown()) {
					var w = child.Widget();
					child.Render();
					w.Show();
				}
				else child.Widget().Hide();
			}
			return true;
		}
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
		static bool IsActive(Racr.AstNode n) {
			return n.IsErrorQuestion() || (n.Parent().IsActive() && n.FindActive(n.GetName()).IsErrorQuestion());
		}
	}

	static class OrdinaryQuestion {
		static ValueTypes Type(Racr.AstNode n) { return n.GetValueType(); }
		static object Value(Racr.AstNode n) { return n.GetValue(); }
		static Control Widget(Racr.AstNode n) {
			// TODO
			Widget w;
			if (n.Type() == ValueTypes.Boolean) {
				w = new CheckWidget(n.GetLabel());
				var cb = w.GetCheckBox();
				cb.CheckedChanged += (object sender, EventArgs e) => {
					if (!cb.ContainsFocus) return;
					n.SetValue(cb.Checked);
					n.Root().Render();
				};
			}
			else {
				w = new TextWidget(n.GetLabel());
				var tb = w.GetTextBox();
				tb.TextChanged += (object sender, EventArgs e) => {
					if (!tb.ContainsFocus) return;
					if (n.Type() == ValueTypes.Number) {
						try {
							n.SetValue(Convert.ToDouble(tb.Text));
						}
						catch { return; }
					}
					else n.SetValue(tb.Text);
					n.Root().Render();
				};
			}
			n.Parent().Widget().Controls.Add(w);
			return w;
		}
		static bool Render(Racr.AstNode n) { return false; }
	}

	static class ComputedQuestion {
		static ValueTypes Type(Racr.AstNode n) { return n.GetExpression().Type(); }
		static object Value(Racr.AstNode n) { return n.GetExpression().Value(); }
		static Control Widget(Racr.AstNode n) {
			Widget w;
			if (n.Type() == ValueTypes.Boolean) w = new CheckWidget(n.GetLabel());
			else w = new TextWidget(n.GetLabel());
			n.Parent().Widget().Controls.Add(w);
			return w;
		}
		static bool Render(Racr.AstNode n) {
			var v = n.Value();
			var ctrl = n.Widget();
			switch (n.Type()) {
			case ValueTypes.Boolean:
				var cb = ctrl as CheckBox;
				cb.Checked = (bool) v;
				break;
			case ValueTypes.String:
				ctrl.Text = (string) v;
				break;
			case ValueTypes.Number:
				ctrl.Text = Convert.ToString(Convert.ToDouble((string) v));
				break;
			default: break;
			}
			return true;
		}
	}

	static class Use {
		static ValueTypes Type(Racr.AstNode n) { return n.GLookup(n.GetName()).Type(); }
		static object Value(Racr.AstNode n) { return n.FindActive(n.GetName()).Value(); }
	}

	static class Constant {
		static ValueTypes Type(Racr.AstNode n) {
			var val = n.GetValue();
			if (val is bool) return ValueTypes.Boolean;
			if (val is double) return ValueTypes.Number;
			if (val is string) return ValueTypes.String;
			return ValueTypes.ErrorType;
		}
		static object Value(Racr.AstNode n) { return n.GetValue(); }
	}

	static class Computation {
		static ValueTypes Type(Racr.AstNode n) {
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
			{ "+", (object[] l) => { return l.Aggregate(0.0, (s, x) => s + (double) x); } },
			{ "-", (object[] l) => { return l.Aggregate(0.0, (s, x) => s - (double) x); } },
			{ "*", (object[] l) => { return l.Aggregate(1.0, (s, x) => s * (double) x); } },
			{ "string-append", (object[] l) => { return l.Aggregate("", (s, x) => s + (string) x); } },
			// TODO
		};
		static object Value(Racr.AstNode n) {
			var op = n.GetOperator();
			var operands = n.GetOperands().Children();
			var args = operands.Select(p => ((Racr.AstNode) p).Value()).ToArray();
			object result;
			try {
				result = opTable[op](args);
			}
			catch {
				result = null;
			}
			return result;
		}
	}


}


class Lexer {

	public enum Lexemes {
		EOF,
		LeftParenthesis,
		RightParenthesis,
		String,
		Symbol,
		Number,
		Identifier,
		Error
	};
	
	public Lexer(string src) {
		source = src;
		position = 0;
		NextChar();
		NextLexeme();
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
		for (;;) {
			if (Char.IsWhiteSpace(character)) {
				NextChar();
				continue;
			}
			if (character == ';') {
				do NextChar();
				while (character != '\0' && character != '\n');
				continue;
			}
			break;
		}

		token = "";

		switch (character) {
		case '(': NextChar(); return Lexemes.LeftParenthesis;
		case ')': NextChar(); return Lexemes.RightParenthesis;
		case '\0': return Lexemes.EOF;
		case '\'':
			NextChar();
			while (Char.IsLetterOrDigit(character)) token += NextChar();
			return Lexemes.Symbol;
		case '"':
			NextChar();
			while (character != '"') {
				if (character == '\0') return Lexemes.Error;
				else if (character == '\\') {
					NextChar();
					if (character == '\\') token += '\\';
					else if (character == '"') token += '"';
					else if (character == 'n') token += '\n';
					else return Lexemes.Error;
				}
				else token += NextChar();
			}
			NextChar();
			return Lexemes.String;
		default: break;
		}

		if (Char.IsDigit(character)) {
			while (Char.IsDigit(character)) token += NextChar();
			if (character == '.') {
				token += NextChar();
				while (Char.IsDigit(character)) token += NextChar();
			}
			return Lexemes.Number;
		}

		while (Char.IsLetterOrDigit(character) || "+-*/:&=<>?!~$#".Contains(character)) {
			token += NextChar();
		}
		if (token.Length > 0) return Lexemes.Identifier;

		return Lexemes.Error;
	}

	public Lexemes Lexeme { get { return lexeme; } }
	public string Token { get { return token; } }

	private char character;
	private Lexemes lexeme;
	private string token;

	private string source;
	private int position;
}


class Parser : Lexer {
	private Racr.Specification spec;

	public Parser(Racr.Specification spec, string src) : base(src) {
		this.spec = spec;
	}

	private void Consume(Lexemes lexeme) {
		if (Lexeme != lexeme) throw new Exception("Parse Exception");
		NextLexeme();
	}
	private string ParseIdentifier() {
		var t = Token;
		Consume(Lexemes.Identifier);
		return t;
	}
	private string ParseString() {
		var t = Token;
		Consume(Lexemes.String);
		return t;
	}
	private string ParseSymbol() {
		var t = Token;
		Consume(Lexemes.Symbol);
		return t;
	}
	private object ParseValue() {
		var t = Token;
		switch (NextLexeme()) {
		case Lexemes.Identifier:
			if (t == "#t") return true;
			if (t == "#f") return false;
			throw new Exception("Parse Error");
		case Lexemes.Number: return Convert.ToDouble(t);
		case Lexemes.String: return t;
		default: throw new Exception("Parse Error");
		}
	}

	private Racr.AstNode ParseExpression() {
		Racr.AstNode c;
		List<Racr.AstNode> e, a;
		string n, l, o;
		ValueTypes t;
		object v;

		Consume(Lexemes.LeftParenthesis);
		switch (ParseIdentifier()) {
		case "Form":
			e = new List<Racr.AstNode>();
			e.Add(new Racr.AstNode(spec, "ComputedQuestion", "ErrorType", "", new Racr.AstNode(spec, "Constant", false)));
			while (Lexeme == Lexemes.LeftParenthesis) e.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "Form", new Racr.AstList(e.ToArray()));
		case "If":
			c = ParseExpression();
			e = new List<Racr.AstNode>();
			while (Lexeme == Lexemes.LeftParenthesis) e.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "Group", c, new Racr.AstList(e.ToArray()));
		case "??":
			n = ParseSymbol();
			l = ParseString();
			t = (ValueTypes) Enum.Parse(typeof(ValueTypes), ParseIdentifier());
			if (Lexeme == Lexemes.RightParenthesis) v = null;
			else v = ParseValue();
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "OrdinaryQuestion", n, l, t, v);
		case "~?":
			c = new Racr.AstNode(spec, "ComputedQuestion", ParseSymbol(), ParseString(), ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return c;
		case "~>":
			n = ParseSymbol();
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "Use", n);
		case "~!":
			v = ParseValue();
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "Constant", v);
		case "~~":
			o = ParseIdentifier();
			a = new List<Racr.AstNode>();
			while (Lexeme == Lexemes.LeftParenthesis) a.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return new Racr.AstNode(spec, "Computation", o, new Racr.AstList(a.ToArray()));
		default: throw new Exception("Parse Exception");
		}
	}

	public Racr.AstNode ParseAst() {
		var ast = ParseExpression();
		Consume(Lexemes.EOF);
		return ast;
	}
}


class Questionnaire {

	static QL ql;

	static void UpdateQuestions(Racr.AstNode n) {
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

	public static void Main(string[] args) {

		string path;
		/*
		if (args.Length == 1) path = args[0];
		else {
			Console.Write("Enter name: ");
			path = Console.ReadLine();
		}
		*/
		path = "../../../../../foo.questionnaire";
		//path = "../../../../../correct-1.questionnaire";

		ql = new QL();
		var parser = new Parser(ql, File.OpenText(path).ReadToEnd());
		var form = parser.ParseAst();
		UpdateQuestions(form);
		form.Render();

		Application.Run(form.Widget().Parent as Form);
	}
}

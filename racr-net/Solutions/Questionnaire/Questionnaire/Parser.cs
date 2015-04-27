using System;
using System.Linq;
using System.Collections.Generic;

public class Lexer {

	static public string EscapeString(string s) {
		string o = "\"";
		foreach (var c in s) {
			if (c == '\\' || c == '"') o += '\\' + c;
			else if (c == '\n') o += '\n';
			else o += c;
		}
		o += "\"";
		return o;
	}

	public static string EscapeValue(object v) {
		if (v == null) return "";
		if (v is string) return EscapeString((string) v);
		if (v is bool) return (bool) v ? "#t" : "#f";
		return Convert.ToString(v);
	}


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
					if (character == '\\' || character == '"') token += character;
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


public class Parser : Lexer {
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
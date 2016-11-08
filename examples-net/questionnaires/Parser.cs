/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.Linq;
using System.Collections.Generic;

using Ast = Racr.AstNode;
using Types = QuestionnairesLanguage.Types;

public class Lexer {
	private Lexemes lexeme;
	public Lexemes Lexeme { get { return lexeme; } }
	private string token;
	public string Token { get { return token; } }
	private char character;
	private string source;
	private int position;

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
		if (v is bool) return (bool) v ? "#t" : "#f";
		if (v is string) return EscapeString((string) v);
		return Convert.ToString(v);
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
		case '(':
			NextChar();
			return Lexemes.LeftParenthesis;
		case ')':
			NextChar();
			return Lexemes.RightParenthesis;
		case '\0':
			return Lexemes.EOF;
		case '\'':
			NextChar();
			while (Char.IsLetterOrDigit(character)) token += NextChar();
			return Lexemes.Symbol;
		case '"':
			NextChar();
			while (character != '"') {
				if (character == '\0') {
					return Lexemes.Error;
				} else if (character == '\\') {
					NextChar();
					if (character == '\\' || character == '"') token += character;
					else if (character == 'n') token += '\n';
					else return Lexemes.Error;
				} else token += NextChar();
			}
			NextChar();
			return Lexemes.String;
		default:
			break;
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
		case Lexemes.Number:
			return Convert.ToDouble(t);
		case Lexemes.String:
			return t;
		default:
			throw new Exception("Parse Error");
		}
	}

	private Ast ParseExpression() {
		Ast c;
		List<Ast> e, a;
		string n, l, o;
		Types t;
		object v;

		Consume(Lexemes.LeftParenthesis);
		switch (ParseIdentifier()) {
		case "Form":
			e = new List<Ast>();
			e.Add(spec.CreateAst("ComputedQuestion", "ErrorType", "", spec.CreateAst("Constant", false)));
			while (Lexeme == Lexemes.LeftParenthesis) e.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("Form", spec.CreateAstList(e.ToArray()));
		case "If":
			c = ParseExpression();
			e = new List<Ast>();
			while (Lexeme == Lexemes.LeftParenthesis) e.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("Group", c, spec.CreateAstList(e.ToArray()));
		case "??":
			n = ParseSymbol();
			l = ParseString();
			t = (Types) Enum.Parse(typeof(Types), ParseIdentifier());
			if (Lexeme == Lexemes.RightParenthesis) v = QuestionnairesLanguage.ErrorValue;
			else v = ParseValue();
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("OrdinaryQuestion", n, l, t, v);
		case "~?":
			c = spec.CreateAst("ComputedQuestion", ParseSymbol(), ParseString(), ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return c;
		case "~>":
			n = ParseSymbol();
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("Use", n);
		case "~!":
			v = ParseValue();
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("Constant", v);
		case "~~":
			o = ParseIdentifier();
			a = new List<Ast>();
			while (Lexeme == Lexemes.LeftParenthesis) a.Add(ParseExpression());
			Consume(Lexemes.RightParenthesis);
			return spec.CreateAst("Computation", o, spec.CreateAstList(a.ToArray()));
		default:
			throw new Exception("Parse Exception");
		}
	}

	public Ast ParseAst() {
		var ast = ParseExpression();
		Consume(Lexemes.EOF);
		return ast;
	}
}

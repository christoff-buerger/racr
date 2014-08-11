/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 *
 * @author C. Bürger
 */

package tinycpp;
import tinycpp.Parser.Terminals;

%%

%public
%final
%class Lexer
%extends beaver.Scanner
%type beaver.Symbol 
%function nextToken 
%yylexthrow beaver.Scanner.Exception
%line
%column

%{
	private beaver.Symbol sym(short id) {
		return new beaver.Symbol(id, yyline + 1, yycolumn + 1, yylength(), yytext());
	}
%}

LineTerminator	= \r|\n|\r\n
Comment			= "//" [^\r\n]*
WhiteSpace		= {LineTerminator} | [ \t\f] | {Comment}
Identifier		= [a-zA-Z] [a-zA-Z0-9]*

%%

{WhiteSpace}	{ /* ignore */ }
"class"			{ return sym(Terminals.CLASS); }
"public"		{ return sym(Terminals.PUBLIC); }
"static"		{ return sym(Terminals.STATIC); }
"int"			{ return sym(Terminals.INT); }
"void"			{ return sym(Terminals.VOID); }
{Identifier}	{ return sym(Terminals.IDENTIFIER); }
";"				{ return sym(Terminals.SEMICOLON); }
","				{ return sym(Terminals.COMMA); }
"::"			{ return sym(Terminals.COLON_COLON); }
":"				{ return sym(Terminals.COLON); }
"="				{ return sym(Terminals.EQUAL); }
"("				{ return sym(Terminals.LPAREN); }
")"				{ return sym(Terminals.RPAREN); }
"{"				{ return sym(Terminals.LBRACE); }
"}"				{ return sym(Terminals.RBRACE); }
<<EOF>>			{ return sym(Terminals.EOF); }

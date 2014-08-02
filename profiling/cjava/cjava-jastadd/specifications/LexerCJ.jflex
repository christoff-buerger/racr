/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 *
 * @author M. Tasić, C. Bürger
 */

package cjava;
import cjava.ParserCJ.Terminals;

%%

%public
%final
%class LexerCJ 
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
Identifier		= [:jletter:]([:jletterdigit:])*

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
"."				{ return sym(Terminals.POINT); }
"="				{ return sym(Terminals.EQUAL); }
"("				{ return sym(Terminals.LPAREN); }
")"				{ return sym(Terminals.RPAREN); }
"{"				{ return sym(Terminals.LBRACE); }
"}"				{ return sym(Terminals.RBRACE); }
"["				{ return sym(Terminals.LSQUARE); }
"]"				{ return sym(Terminals.RSQUARE); }
<<EOF>>			{ return sym(Terminals.EOF); }

/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 *
 * @author M. Tasić, C. Bürger
 */

package cjava;
import cjava.ParserCL.Terminals;

%%

%public
%final
%class LexerCL 
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
Identifier = [:jletter:]([:jletterdigit:] | "." | "*")*

%%

{WhiteSpace}	{ /* ignore */ }
"bind"			{ return sym(Terminals.BIND); }
{Identifier}	{ return sym(Terminals.IDENTIFIER); }
";"				{ return sym(Terminals.SEMICOLON); }
<<EOF>>			{ return sym(Terminals.EOF); }

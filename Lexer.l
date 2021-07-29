%{

#include "Parser.tab.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define fileno _fileno

extern size_t number_line;

%}

%option noyywrap
%option yylineno

%%

[0-9]+		{ int intval = atoi(yytext); yylval.ival = intval; return NUMBER; }
\+			{ yylval.ival = 1; return SIGN; }
\-			{ yylval.ival = -1; return SIGN; }
\*			{ return MUL; }
\^			{ return POWER; }
\(			{ return OPENBRACKETS; }
\)			{ return CLOSEBRACKETS; }
\$			{ return DECLARATION; }
\=			{ return EQUALLY; }
\;			{ return SEMICOLON; }
[a-z]		{ yylval.cval = (char)yytext[0]; return X; }
[A-Z]{1,10} { memset(yylval.sval, 0, 12); memcpy(yylval.sval, yytext, yyleng); yylval.sval[yyleng] = 0; return VAR; }
[A-Z]{11,}	{ yyerror("variable name is too long."); }
print		{ return PRINT; }

(#[^\n]*)								{ ; }
[ \t]+									{ ; }
\n										{ number_line++; }
[\+\*\-]{2,}							{ yyerror("duplicate operators."); }
[^0-9a-zA-Z\+\-\*\^\(\)\$\=\n\r\t \#]	{ yyerror("unknown symbol."); }

%%
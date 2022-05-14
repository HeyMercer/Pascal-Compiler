%{
#include "GrammarAnalysis.tab.h"
int lineCount=1;
%}

Identifier		[a-z][a-z0-9]*
Integer			0|[1-9][0-9]*
Float			(0|[1-9][0-9]*)\.[0-9]*
Char			'.'
String          '[^\n]*'
ConstSystem		false|true|maxint
TypeSystem		boolean|char|integer|real
CallSystem		write|writeln
FunctionSystem	abs|chr|int|odd|ord|pred|round|sqr|sqrt|succ
LineBreak		\n
%x Comment

%option caseless

%%

"{^*"+[^\n]*+"}"					;
"(*"								BEGIN Comment;
<Comment>\n							{ lineCount++; }
<Comment>"*)"						{ BEGIN INITIAL; }
<Comment>.							;

{LineBreak}							lineCount++;
"and"								return AND;
"array"								return ARRAY;
"begin"								return BEGIN_;
"case"								return CASE;
"const"								return CONST;
"div"								return DIV;
"do"								return DO;
"downto"							return DOWNTO;
"else"								return ELSE;
"end"								return END;
"for"								return FOR;
"function"							return FUNCTION;
"goto"								return GOTO;
"if"								return IF;
"mod"								return MOD;
"not"								return NOT;
"of"								return OF;
"or"								return OR;
"procedure"							return PROCEDURE;
"program"							return PROGRAM;
"record"							return RECORD;
"repeat"							return REPEAT;
"then"								return THEN;
"to"								return TO;
"type"								return TYPE;
"until"								return UNTIL;
"var"								return VAR;
"while"								return WHILE;

"read"								return READ;
":="								return ASSIGN;
"..."								return ELLIPSIS;
"("									return LP;
")"									return RP;
"["									return LB;
"]"									return RB;
"."									return PERIOD;
";"									return SEMICOLON;
":"									return COLON;
","									return COMMA;
"="									return EQ;
">"									return GT;
"<"									return LT;
"<>"								return NE;
">="								return GE;
"<="								return LE;
"%"									return MOD;
"*"									return MUL;
"/"									return DIV;
"+"									return PLUS;
"-"									return MINUS;

{Integer}							{ printf("%60s -> CONST_INTEGER\n", yytext); return CONST_INTEGER; }
{Float}								{ printf("%60s -> CONST_FLOAT\n", yytext); return CONST_FLOAT; }
{Char}								{ printf("%60s -> CONST_CHAR\n", yytext); return CONST_CHAR; }
{String}                            { printf("%60s -> CONST_STRING\n", yytext); return CONST_STRING; }
{ConstSystem}						{ printf("%60s -> CONST_SYSTEM\n", yytext); return CONST_SYSTEM; }
{TypeSystem}						{ printf("%60s -> TYPE_SYSTEM\n", yytext); return TYPE_SYSTEM; }
{CallSystem}						{ printf("%60s -> CALL_SYSTEM\n", yytext); return CALL_SYSTEM; }
{FunctionSystem}					{ printf("%60s -> FUNCTION_SYSTEM\n", yytext); return FUNCTION_SYSTEM; }
{Identifier}						{ printf("%60s -> IDENTIFIER\n", yytext); return IDENTIFIER; }
.									;

%%
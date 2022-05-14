%{
#include <stdio.h>
#include <string.h>
int yywrap() {
	return 1;
}
int yydebug = 1;
%}

%union {
	int integerValue;
	float floatValue;
	char charValue;
	char* stringValue;
}

%token ARRAY BEGIN_ CASE CONST DO ELSE DOWNTO END FOR FUNCTION GOTO IF OF PROCEDURE PROGRAM RECORD REPEAT THEN TO TYPE UNTIL VAR WHILE
%token IDENTIFIER CONST_INTEGER CONST_FLOAT CONST_CHAR CONST_SYSTEM CONST_STRING TYPE_SYSTEM CALL_SYSTEM READ ASSIGN FUNCTION_SYSTEM
%token ELLIPSIS LP RP LB RB PERIOD SEMICOLON COLON COMMA

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%left EQ LT GT NE GE LE
%left PLUS MINUS OR
%left MUL DIV MOD AND
%left NOT

%start program

%%

program
	: program_header program_body		 							{ printf("                                 program_header program_body -> program\n"); }
	;

program_header
	: PROGRAM IDENTIFIER SEMICOLON 									{ printf("                              PROGRAM PROGRAM_NAME SEMICOLON -> program_header\n"); }
	;
	
program_body
	: const_part type_part var_part routine_part main_part 			{ printf("                  const_part type_part var_part routine_part -> program_body\n"); }
	;

const_part
	: 																{ printf("                                                           e -> const_part\n"); }
	| CONST const_list												{ printf("                                            CONST const_list -> const_part\n"); }
	;
	
const_list
	: const_list const_declaration									{ printf("                                const_list const_declaration -> const_list\n"); }
	| const_declaration												{ printf("                                           const_declaration -> const_list\n"); }
	;

const_declaration
	: IDENTIFIER EQ const_value SEMICOLON							{ printf("                         IDENTIFIER EQ const_value SEMICOLON -> const_declaration\n"); }
	
const_value
	: CONST_INTEGER													{ printf("                                               CONST_INTEGER -> const_value\n"); }
	| CONST_FLOAT													{ printf("                                                 CONST_FLOAT -> const_value\n"); }
	| CONST_CHAR													{ printf("                                                  CONST_CHAR -> const_value\n"); }
	| CONST_STRING													{ printf("                                                CONST_STRING -> const_value\n"); }
	| CONST_SYSTEM													{ printf("                                                CONST_SYSTEM -> const_value\n"); }
	;
	
type_part
	:																{ printf("                                                           e -> type_part\n"); }
	| TYPE type_list												{ printf("                                              TYPE type_list -> type_part\n"); }
	;

type_list
	: type_list type_definition										{ printf("                                   type_list type_definition -> type_list\n"); }
	| type_definition												{ printf("                                             type_definition -> type_list\n"); }
	;

type_definition
	: IDENTIFIER EQ type_declaration SEMICOLON						{ printf("                    IDENTIFIER EQ type_declaration SEMICOLON -> type_definition\n"); }
	;

type_declaration
	: normal_type_declaration										{ printf("                                     normal_type_declaration -> type_declaration\n"); }
	| array_type_declaration										{ printf("                                      array_type_declaration -> type_declaration\n"); }
	| record_type_declaration										{ printf("                                     record_type_declaration -> type_declaration\n"); }
	;

normal_type_declaration
	: TYPE_SYSTEM													{ printf("                                                 TYPE_SYSTEM -> normal_type_declaration\n"); }
	| IDENTIFIER													{ printf("                                                  IDENTIFIER -> normal_type_declaration\n"); }
	| LP name_list RP												{ printf("                                             LP name_list RP -> normal_type_declaration\n"); }
	| const_value ELLIPSIS const_value								{ printf("                            const_value ELLIPSIS const_value -> normal_type_declaration\n"); }
	| IDENTIFIER ELLIPSIS const_value								{ printf("                             IDENTIFIER ELLIPSIS const_value -> normal_type_declaration\n"); }
	| const_value ELLIPSIS IDENTIFIER								{ printf("                             const_value ELLIPSIS IDENTIFIER -> normal_type_declaration\n"); }
	| IDENTIFIER ELLIPSIS IDENTIFIER								{ printf("                              IDENTIFIER ELLIPSIS IDENTIFIER -> normal_type_declaration\n"); }
	;

name_list
	: name_list COMMA IDENTIFIER									{ printf("                                  name_list COMMA IDENTIFIER -> name_list\n"); }
	| IDENTIFIER													{ printf("                                                  IDENTIFIER -> name_list\n"); }
	;

array_type_declaration
	: ARRAY LB normal_type_declaration RB OF type_declaration		{ printf("     ARRAY LB normal_type_declaration RB OF type_declaration -> array_type_declaration\n"); }
	;

record_type_declaration
	: RECORD field_declaration_list END								{ printf("                           RECORD field_declaration_list END -> record_type_declaration\n"); }
	;

field_declaration_list
	: field_declaration_list field_declaration						{ printf("                    field_declaration_list field_declaration -> field_declaration_list\n"); }
	| field_declaration												{ printf("                                           field_declaration -> field_declaration_list\n"); }
	;

field_declaration
	: name_list COLON type_declaration SEMICOLON					{ printf("                  name_list COLON type_declaration SEMICOLON -> field_declaration\n"); }
	;

var_part
	:																{ printf("                                                           e -> var_part\n"); }
	| VAR var_list													{ printf("                                                VAR var_list -> var_part\n"); }
	;

var_list
	: var_list var_declaration										{ printf("                                    var_list var_declaration -> var_list\n"); }
	| var_declaration												{ printf("                                             var_declaration -> var_list\n"); }
	;

var_declaration
	: name_list COLON type_declaration SEMICOLON					{ printf("                  name_list COLON type_declaration SEMICOLON -> var_declaration\n"); }
	;

routine_part
	: 																{ printf("                                                           e -> routine_part\n"); }
	| routine_part routine_declaration								{ printf("                            routine_part routine_declaration -> routine_part\n"); }
	;

routine_declaration
	: function_declaration											{ printf("                                        function_declaration -> routine_declaration\n"); }
	| procedure_declaration											{ printf("                                       procedure_declaration -> routine_declaration\n"); }
	;

function_declaration
	: function_header SEMICOLON var_part routine_body SEMICOLON		{ printf("                       function_header var_part routine_body -> function_declaration\n"); }
	;

function_header
	: FUNCTION IDENTIFIER parameters COLON normal_type_declaration	{ printf("FUNCTION IDENTIFIER parameters COLON normal_type_declaration -> function_header\n"); }
	;

procedure_declaration
	: procedure_header SEMICOLON var_part routine_body SEMICOLON	{ printf("                      procedure_header var_part routine_body -> procedure_declaration\n"); }
	;

procedure_header
	: PROCEDURE IDENTIFIER parameters								{ printf("                             PROCEDURE IDENTIFIER parameters -> procedure_header\n"); }
	;

parameters
	:																{ printf("                                                           e -> parameters\n"); }
	| LP parameter_list RP											{ printf("                                        LP parameter_list RP -> parameters\n"); }
	;

parameter_list
	: parameter_list SEMICOLON parameter_declaration				{ printf("              parameter_list SEMICOLON parameter_declaration -> parameter_list\n"); }
	| parameter_declaration											{ printf("                                       parameter_declaration -> parameter_list\n"); }
	;

parameter_declaration
	: name_list COLON normal_type_declaration						{ printf("                     name_list COLON normal_type_declaration -> parameter_declaration\n"); }
	;

routine_body
	: BEGIN_ statements END											{ printf("                                       BEGIN_ statements END -> routine_body\n"); }
	;

main_part
	: BEGIN_ statements END PERIOD									{ printf("                                BEGIN_ statements END PERIOD -> main_part\n"); }
	;

statements
	:																{ printf("                                                           e -> statements\n"); }
	| statements CONST_INTEGER COLON statement SEMICOLON			{ printf("          statements CONST_INTEGER COLON statement SEMICOLON -> statements\n"); }
	| statements statement SEMICOLON								{ printf("                              statements statement SEMICOLON -> statements\n"); }
	;

statement
	: assignment_statement											{ printf("                                        assignment_statement -> statement\n"); }
	| call_statement												{ printf("                                              call_statement -> statement\n"); }
	| if_statement													{ printf("                                                if_statement -> statement\n"); }
	| repeat_statement												{ printf("                                            repeat_statement -> statement\n"); }
	| while_statement												{ printf("                                             while_statement -> statement\n"); }
	| for_statement													{ printf("                                               for_statement -> statement\n"); }
	| case_statement												{ printf("                                              case_statement -> statement\n"); }
	| goto_statement												{ printf("                                              goto_statement -> statement\n"); }
	| routine_body													{ printf("                                                routine_body -> statement\n"); }
	;

assignment_statement
	: IDENTIFIER ASSIGN expression									{ printf("                                IDENTIFIER ASSIGN expression -> assignment_statement\n"); }
	| IDENTIFIER LB expression RB ASSIGN expression					{ printf("               IDENTIFIER LP expression RP ASSIGN expression -> assignment_statement\n"); }
	| IDENTIFIER PERIOD IDENTIFIER ASSIGN expression				{ printf("              IDENTIFIER PERIOD IDENTIFIER ASSIGN expression -> assignment_statement\n"); }
	;

call_statement
	: IDENTIFIER													{ printf("                                                  IDENTIFIER -> call_statement\n"); }
	| IDENTIFIER LP arg_list RP										{ printf("                                   IDENTIFIER LP arg_list RP -> call_statement\n"); }
	| CALL_SYSTEM													{ printf("                                                 CALL_SYSTEM -> call_statement\n"); }
	| CALL_SYSTEM LP arg_list RP									{ printf("                                  CALL_SYSTEM LP arg_list RP -> call_statement\n"); }
	| READ LP arg_list RP											{ printf("                                         READ LP arg_list RP -> call_statement\n"); }
	;

if_statement
	: IF expression THEN statement %prec LOWER_THAN_ELSE			{ printf("                IF expression THEN statement LOWER_THAN_ELSE -> if_statement\n"); }
	| IF expression THEN statement ELSE statement					{ printf("                 IF expression THEN statement ELSE statement -> if_statement\n"); }
	;

repeat_statement
	: REPEAT statements UNTIL expression							{ printf("                          REPEAT statements UNTIL expression -> repeat_statement\n"); }
	;

while_statement
	: WHILE expression DO statement									{ printf("                               WHILE expression DO statement -> while_statement\n"); }
	;

for_statement
	: FOR for_condition DO statement								{ printf("                              FOR for_condition DO statement -> for_statement\n"); }
	;

for_condition
	: IDENTIFIER ASSIGN expression direction expression				{ printf("           IDENTIFIER ASSIGN expression direction expression -> for_condition\n"); }
	;

direction
	: TO															{ printf("                                                          TO -> direction\n"); }
	| DOWNTO														{ printf("                                                      DOWNTO -> direction\n"); }
	;

case_statement
	: CASE expression OF case_list END								{ printf("                            CASE expression OF case_list END -> case_statement\n"); }
	;

case_list
	: case_list case_expression										{ printf("                                   case_list case_expression -> case_list\n"); }
	| case_expression												{ printf("                                             case_expression -> case_list\n"); }
	;

case_expression
	: const_value COLON statement SEMICOLON							{ printf("                       const_value COLON statement SEMICOLON -> case_expression\n"); }
	| IDENTIFIER COLON statement SEMICOLON							{ printf("                        IDENTIFIER COLON statement SEMICOLON -> case_expression\n"); }
	;

goto_statement
	: GOTO CONST_INTEGER											{ printf("                                          GOTO CONST_INTEGER -> goto_statement\n"); }
	;

expression
	: expression GE expr											{ printf("                                          expression GE expr -> expression\n"); }
	| expression LE expr											{ printf("                                          expression LE expr -> expression\n"); }
	| expression NE expr											{ printf("                                          expression NE expr -> expression\n"); }
	| expression GT expr											{ printf("                                          expression GT expr -> expression\n"); }
	| expression LT expr											{ printf("                                          expression LT expr -> expression\n"); }
	| expression EQ expr											{ printf("                                          expression EQ expr -> expression\n"); }
	| expr															{ printf("                                                        expr -> expression\n"); }
	;

expr
	: expr PLUS term												{ printf("                                              expr PLUS term -> expr\n"); }
	| expr MINUS term												{ printf("                                             expr MINUS term -> expr\n"); }
	| expr OR term													{ printf("                                                expr OR term -> expr\n"); }
	| term															{ printf("                                                        term -> expr\n"); }
	;

term
	: term MUL factor												{ printf("                                             term MUL factor -> term\n"); }
	| term DIV factor												{ printf("                                             term DIV factor -> term\n"); }
	| term MOD factor												{ printf("                                             term MOD factor -> term\n"); }
	| term AND factor												{ printf("                                             term AND factor -> term\n"); }
	| factor														{ printf("                                                      factor -> term\n"); }
	;

factor
	: IDENTIFIER													{ printf("                                                  IDENTIFIER -> factor\n"); }
	| IDENTIFIER LP arg_list RP										{ printf("                                   IDENTIFIER LP arg_list RP -> factor\n"); }
	| IDENTIFIER LB expression RB										{ printf("                             IDENTIFIER LB expression RB -> factor\n"); }
	| FUNCTION_SYSTEM												{ printf("                                             FUNCTION_SYSTEM -> factor\n"); }
	| FUNCTION_SYSTEM LP arg_list RP								{ printf("                              FUNCTION_SYSTEM LP arg_list RP -> factor\n"); }
	| const_value													{ printf("                                                 const_value -> factor\n"); }
	| LP expression RP												{ printf("                                            LP expression RP -> factor\n"); }
	| NOT factor													{ printf("                                                  NOT factor -> factor\n"); }
	| MINUS factor													{ printf("                                                MINUS factor -> factor\n"); }
	| IDENTIFIER PERIOD IDENTIFIER									{ printf("                                IDENTIFIER PERIOD IDENTIFIER -> factor\n"); }
	;

arg_list
	: arg_list COMMA expression										{ printf("                                   arg_list COMMA expression -> arg_list\n"); }
	| expression													{ printf("                                                  expression -> arg_list\n"); }
	;

%%

extern int yylex();
extern int yyparse();
extern FILE *yyin;
extern int lineCount;
extern char *yytext;

int yyerror(char* s)
{
    fprintf(stderr, "Error in Line %d: %s at symbol '%c'", lineCount, s, yytext[0]);
}

int main(int argc, char* argv[])
{
	if(argc <= 1) return 0;
	FILE *file;
	file = fopen(argv[1], "r");
	if (!file) {
		return 1;
	}
	yyin = file;
	yyparse();
	return 0;
}
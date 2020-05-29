%{
	#include <stdio.h>
	#include <stdlib.h>
	void yyerror(const char *msg);
	extern int currLine;
	extern int currPos;
	FILE * yyin;
	int yylex();
%}

%union{
	int num_val;	/* number: NUMBER token */
	char* str_val;	/* identifier: IDENT token */
}

%define parse.error verbose
%start start_program

%token <num_val> NUMBER
%token <str_val> IDENT

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGIN_LOOP END_LOOP CONTINUE READ WRITE TRUE FALSE RETURN

%right ASSIGN
%left OR
%left AND
%right NOT
%left EQ NEQ LT GT LTE GTE
%left ADD SUB
%left MULT DIV MOD
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
%left L_PAREN R_PAREN

%token COLON SEMICOLON COMMA

%%
			/* DECLARATIONS */

	// PROGRAMS :
start_program: functions {printf("program -> functions\n");};

	//FUNCTIONS :
functions : %empty 	{printf("functions -> epsilon\n");}
	|function functions {printf("functions -> function functions\n");};
function: FUNCTION Ident SEMICOLON BEGIN_PARAMS Declarations END_PARAMS BEGIN_LOCALS Declarations END_LOCALS BEGIN_BODY Statements END_BODY {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");};

	// DECLARATIONS :
Declaration:	Identifiers COLON INTEGER	{printf("declaration -> identifiers COLON INTEGER\n");} 
	| Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");} 
	| Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");};
Declarations:	%empty {printf("declarations -> epsilon\n");} 
	| Declaration SEMICOLON Declarations {printf("declarations -> declaration SEMICOLON declarations\n");};

	//STATEMENTS :
Statement: Statement1 | Statement2 | Statement3 | Statement4 | Statement5 | Statement6 | Statement7 | Statement8 | Statement9;
Statements: Statement SEMICOLON Statements {printf("statements -> statement SEMICOLON statements\n");}
	| %empty {printf("statements -> epsilon\n");};
Statement1: Var ASSIGN Expression {printf("statement -> var ASSIGN expression\n");};
Statement2: IF BoolExp THEN Statements ENDIF {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
	| IF BoolExp THEN Statements ELSE Statements ENDIF {printf("statement -> IF bool_exp THEN statements ELSE statements ENDIF\n");};
Statement3: WHILE BoolExp BEGIN_LOOP Statements END_LOOP {printf("statement -> WHILE bool_exp BEGIN_LOOP statements ENDLOOP\n");};
Statement4: DO BEGIN_LOOP Statements END_LOOP WHILE BoolExp {printf("statement -> DO BEGIN_LOOP statements END_LOOP WHILE bool_exp\n");};
Statement5: FOR Var ASSIGN NUMBER SEMICOLON BoolExp SEMICOLON Var ASSIGN Expression BEGIN_LOOP Statements END_LOOP {printf("statement -> FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGIN_LOOP statements END_LOOP\n");};
Statement6: READ Vars {printf("statement -> READ vars\n");};
Statement7: WRITE Vars {printf("statement -> WRITE vars\n");};
Statement8: CONTINUE {printf("statement -> CONTINUE\n");};
Statement9: RETURN Expression {printf("statement -> RETURN expressions\n");};

	//BOOL-EXPR :
BoolExp: RelAExp {printf("bool_exp -> relation_and_exp\n");}
	| RelAExp OR BoolExp {printf("bool_exp -> relation_and_exp OR bool_exp\n");};

	//REL-AND-EXPR :
RelAExp: RelExps {printf("relation_and_exp -> relation_exp\n");}
	| RelExps AND RelAExp {printf("relation_and_exp -> relation_exp AND relation_and_exp\n");};

	//REL-EXPR :
RelExps: RelExp | NOT RelExp;
RelExp: Expression Comp Expression {printf("relation_exp -> expression comp expression\n");}
	| TRUE	{printf("relation_exp -> TRUE\n");}
	| FALSE {printf("relation_exp -> FALSE\n");}
	| L_PAREN BoolExp R_PAREN {printf("L_PAREN bool_exp R_PAREN\n");};

	//COMP :
Comp: EQ {printf("comp -> EQ\n");}
	| NEQ {printf("comp -> NEQ\n");}
	| LT {printf("comp -> LT\n");}
	| GT {printf("comp -> GT\n");}
	| LTE {printf("comp -> LTE\n");}
	| GTE {printf("comp -> GTE\n");};

	//EXPERSSION:
Expression: MultExp {printf("expression -> multiplicative_expression\n");}
	| MultExp ADD Expression {printf("expression -> multiplicative_Expression ADD expression\n");}
	| MultExp SUB Expression {printf("expression -> multiplicative_Expression SUB expression\n");};
Expressions: %empty {printf("expressions -> epsilon\n");}
	| Expression {printf("expressions -> expression\n");}
	| Expression COMMA Expressions {printf("expressions -> expression COMMA expressions\n");};

	//MULT-EXPR:
MultExp: Term {printf("multiplicative_expression -> term\n");}
	| Term MULT MultExp {printf("multiplicative_expression -> term MULT multiplicative_expression\n");}
	| Term DIV MultExp {printf("multiplicative_expression -> term DIV multiplicative_expression\n");}
	| Term MOD MultExp {printf("multiplicative_expression -> term MOD multiplicative_expression\n");};

	//TERM:
Term: Var {printf("term -> var\n");}
	| SUB Var {printf("term -> SUB var\n");}
	| NUMBER {printf("term -> NUMBER \n");}
	| SUB NUMBER {printf("term -> SUB NUMBER \n");}
	| L_PAREN Expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
	| SUB L_PAREN Expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
	| Ident L_PAREN R_PAREN {printf("term -> ident L_PAREN R_PAREN\n");}
	| Ident L_PAREN Expressions R_PAREN {printf("term -> ident L_PAREN expressions R_PAREN\n");};

	//VAR:
Vars: Var COMMA Vars {printf("vars -> var COMMA vars\n");}
	| Var {printf("vars -> var\n");};
Var: Ident {printf("var -> ident\n");}
	| Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
	| Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");};

	//IDENT:
Identifiers: Ident {printf("identifiers -> ident\n");}
	| Ident COMMA Identifiers {printf("Identifiers -> identifer COMMA ident\n");};
Ident: IDENT {printf("ident -> IDENT %s \n", $1);};

	/* END OF DECLARATIONS */

	/* MAIN */
%%
int main (int argc, char* argv[]) {
	if (argc > 1) { // An input file has been entered
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			printf("syntax: %s filename\n", argv[0]);
		}
	}
	yyparse();
	return 0;
}
void yyerror(const char *msg)
{
    printf("* Line %d, position %d: %s\n",currLine,currPos,msg);
}
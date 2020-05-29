		/* CS152-SPRING 2020 */
	/* A Flex scanner specification for the calculator language */
	/* Project by: JaeHyun Park and Garret Lim */

	/* DEFINITIONS */
%{
#include <iostream>
#define YY_DECL yy::parser::symbol_type yylex()
#include "parser.tab.hh"
int currLine = 1, currPos = 1;
static yy::location loc;
%}
	/* DEFINITIONS */

%option noyywrap 

%{
#define YY_USER_ACTION loc.columns(yyleng);
%}

	/* your definitions here */
DIGIT    [0-9]
COMMENT "##".*
LETTER [a-zA-Z]`
UNDERSCORE [_]
IDENTIFIER {LETTER}(({LETTER}|{UNDERSCORE}|{DIGIT})*({DIGIT}|{LETTER}))|{LETTER}
	/* your definitions end */

%%

%{
loc.step(); 
%}

	/* your rules here */

	/* use this structure to pass the Token :
	 * return yy::parser::make_TokenName(loc)
	 * if the token has a type you can pass it's value
	 * as the first argument. as an example we put
	 * the rule to return token function.
	 */

"function"			{currPos += yyleng; return yy::parser::make_FUNCTION(loc);}
"beginparams"		{currPos += yyleng; return yy::parser::make_BEGIN_PARAMS(loc);}
"endparams"			{currPos += yyleng; return yy::parser::make_END_PARAMS(loc);}
"beginlocals"		{currPos += yyleng; return yy::parser::make_BEGIN_LOCALS(loc);}
"endlocals"			{currPos += yyleng; return yy::parser::make_END_LOCALS(loc);}
"beginbody"			{currPos += yyleng; return yy::parser::make_BEGIN_BODY(loc);}
"endbody"			{currPos += yyleng; return yy::parser::make_END_BODY(loc);}
"integer"			{currPos += yyleng; return yy::parser::make_INTEGER(loc);}
"array"				{currPos += yyleng; return yy::parser::make_ARRAY(loc);}
"of"				{currPos += yyleng; return yy::parser::make_OF(loc);}
"if"				{currPos += yyleng; return yy::parser::make_IF(loc);}
"then"				{currPos += yyleng; return yy::parser::make_THEN(loc);}
"endif"				{currPos += yyleng; return yy::parser::make_ENDIF(loc);}
"else"				{currPos += yyleng; return yy::parser::make_ELSE(loc);}
"while"				{currPos += yyleng; return yy::parser::make_WHILE(loc);}
"do"				{currPos += yyleng; return yy::parser::make_DO(loc);}
"for"				{currPos += yyleng; return yy::parser::make_FOR(loc);}
"beginloop"			{currPos += yyleng; return yy::parser::make_BEGIN_LOOP(loc);}
"endloop"			{currPos += yyleng; return yy::parser::make_END_LOOP(loc);}
"continue"			{currPos += yyleng; return yy::parser::make_CONTINUE(loc);}
"read"				{currPos += yyleng; return yy::parser::make_READ(loc);}
"write"				{currPos += yyleng; return yy::parser::make_WRITE(loc);}
"and"				{currPos += yyleng; return yy::parser::make_AND(loc);}
"or"				{currPos += yyleng; return yy::parser::make_OR(loc);}
"not"				{currPos += yyleng; return yy::parser::make_NOT(loc);}
"true"				{currPos += yyleng; return yy::parser::make_TRUE(loc);}
"false"				{currPos += yyleng; return yy::parser::make_FALSE(loc);}
"return"			{currPos += yyleng; return yy::parser::make_RETURN(loc);}
"-"					{currPos += yyleng; return yy::parser::make_SUB(loc);}
"+"					{currPos += yyleng; return yy::parser::make_ADD(loc);}
"*"					{currPos += yyleng; return yy::parser::make_MULT(loc);}
"/"					{currPos += yyleng; return yy::parser::make_DIV(loc);}
"%"					{currPos += yyleng; return yy::parser::make_MOD(loc);}
"=="				{currPos += yyleng; return yy::parser::make_EQ(loc);}
"<>"				{currPos += yyleng; return yy::parser::make_NEQ(loc);}
"<"					{currPos += yyleng; return yy::parser::make_LT(loc);}
">"					{currPos += yyleng; return yy::parser::make_GT(loc);}
"<="				{currPos += yyleng; return yy::parser::make_LTE(loc);}
">="				{currPos += yyleng; return yy::parser::make_GTE(loc);}
";"					{currPos += yyleng; return yy::parser::make_SEMICOLON(loc);}
":"					{currPos += yyleng; return yy::parser::make_COLON(loc);}
","					{currPos += yyleng; return yy::parser::make_COMMA(loc);}
"("					{currPos += yyleng; return yy::parser::make_L_PAREN(loc);}
")"					{currPos += yyleng; return yy::parser::make_R_PAREN(loc);}
"["					{currPos += yyleng; return yy::parser::make_L_SQUARE_BRACKET(loc);}
"]"					{currPos += yyleng; return yy::parser::make_R_SQUARE_BRACKET(loc);}
":="				{currPos += yyleng; return yy::parser::make_ASSIGN(loc);}
{DIGIT}+			{currPos += yyleng; return yy::parser::make_NUMBER(stoi(yytext), loc);}
{IDENTIFIER}		{currPos += yyleng; return yy::parser::make_IDENT(yytext, loc);}
{COMMENT}			{currPos += yyleng;}
[ \t]+				{currPos += yyleng;}
"\n"				{++currLine; currPos = 1;}
"\r"				{++currLine; currPos = 1;}


({DIGIT}|{UNDERSCORE})+{IDENTIFIER}                               {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(0);}

{IDENTIFIER}{UNDERSCORE}+                                        {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(0);}

.                                                                 {printf("Error at line %d, column %d: unrecognized symbol \"%s\" \n", currLine, currPos, yytext); exit(0);}


 <<EOF>>	{return yy::parser::make_END(loc);}
	/* your rules end */

%%
/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

bool is_commented = false;
int comment_count = 0;
int str_count = 0;
%}


%x COMMENT
%x STRING_LITTERAL
%x STRING_ERROR

/*
 * Define names for regular expressions here.
 */

/* OPERATORS */
DARROW          =>
ASSIGN		<-

DIGIT 		[0-9]
INTEGER		[0-9]+

TYPEID		[A-Z]([A-Za-z_0-9])*
OBJECTID	[a-z]([A-Za-z_0-9])*

/* COMMENTS */
OPEN_COMMENT	\(\*
CLOSE_COMMENT	\*\)
LINE_COMMENT	--.*

/* PUNCTATION MARKS */
OPEN_PAREN    \(
CLOSE_PAREN   \)
SEMI_COLON    ;
COMMA	      ,
COLON	      :
TIMES	      "*"
PLUS	      "+"
MINUS	      "-"
SLASH	      "/"
EQUAL	      "="
OPEN_BRACKET	 "{"
CLOSE_BRACKET	 "}"
INF		 "<"
INF_EQ		 <=
DOT		 "."
TILDA		 "~"
AT		 "@"

ALL_CHAR	 .
ESC_CHAR	 \\[^tfbn]
ESC_TAB 	 \\t
ESC_BSP		 \\b
ESC_BS		 \\\\
ESC_FF		 \\f
ESC_NL		 \\n
ESC_NEWLINE	 \\\n

/* WHITESPACE */
WHITESPACE	[ \f\t\v\r\n]
NEWLINE		\n

/* KEYWORDS */
CLASS           (?i:class)
ELSE		(?i:else)
FI		(?i:fi)
IF		(?i:if)
IN		(?i:in)
INHERITS	(?i:inherits)
ISVOID		(?i:isvoid)
LET		(?i:let)
LOOP		(?i:loop)
POOL		(?i:pool)
THEN		(?i:then)
WHILE		(?i:while)
CASE		(?i:case)
ESAC		(?i:esac)
NEW		(?i:new)
OF		(?i:of)
NOT		(?i:not)
TRUE		t(?i:rue)
FALSE		f(?i:alse)

/* STRING LITTERAL */
QUOTE		   \"
%%

 /*
  * Newlines
  */
<INITIAL,COMMENT>{NEWLINE}		{ curr_lineno++; }


 /*
  *  Comments
  */

<INITIAL,COMMENT>{OPEN_COMMENT}		{ comment_count++; is_commented = true; BEGIN(COMMENT); }
<INITIAL>{CLOSE_COMMENT} { cool_yylval.error_msg = "Unmatched *)"; return (ERROR); }
<COMMENT>{CLOSE_COMMENT}		{ 
			if (comment_count > 0) {
			 comment_count--;
			}
		        if (comment_count == 0) {
			 BEGIN(INITIAL);
			 is_commented = false; 
			}
			}		 
<COMMENT>{ALL_CHAR} 	{ }
<COMMENT><<EOF>> 	{ cool_yylval.error_msg = "EOF in comment."; BEGIN(INITIAL); return (ERROR); }

{LINE_COMMENT}		{ }

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}		{ return (ASSIGN); }
{INF_EQ}		{ return (LE); }


 /*
  *  Single character marks.
  */

{OPEN_PAREN}		{ return '('; }
{CLOSE_PAREN}		{ return ')'; }
{COMMA}			{ return ','; }
{SEMI_COLON}		{ return ';'; }
{COLON}			{ return ':'; }
{TIMES}			{ return '*'; }
{PLUS}			{ return '+'; }
{MINUS}			{ return '-'; }
{SLASH}			{ return '/'; }
{EQUAL}			{ return '='; }
{OPEN_BRACKET}		{ return '{'; }
{CLOSE_BRACKET}		{ return '}'; }
{INF}			{ return '<'; }
{DOT}			{ return '.'; }
{TILDA}			{ return '~'; }
{AT}			{ return '@'; }


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  *
  */

{CLASS}			{ return (CLASS); }
{IF}			{ return (IF); }
{FI}			{ return (FI); }
{CASE}			{ return (CASE); }
{ESAC}			{ return (ESAC); }
{WHILE}			{ return (WHILE); }
{LOOP}			{ return (LOOP); }
{POOL}			{ return (POOL); }
{IN}			{ return (IN); }
{ISVOID}		{ return (ISVOID); }
{LET}			{ return (LET); }
{NOT}			{ return (NOT); }
{NEW}			{ return (NEW); }
{OF}			{ return (OF); }
{INHERITS}		{ return (INHERITS); }
{THEN}			{ return (THEN); }
{ELSE}			{ return (ELSE); }
{TRUE}			{ cool_yylval.boolean = true; return (BOOL_CONST); }
{FALSE}			{ cool_yylval.boolean = false; return (BOOL_CONST); }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>{QUOTE}	{ BEGIN(STRING_LITTERAL); string_buf[0] = '\0'; string_buf_ptr = string_buf; }
<STRING_LITTERAL>{QUOTE}  { *string_buf_ptr = '\0';  cool_yylval.symbol = new StringEntry(string_buf, string_buf_ptr - string_buf, str_count++); BEGIN(INITIAL); return (STR_CONST); }
<STRING_LITTERAL>{ESC_BS}    { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = '\\'; string_buf_ptr++; } 
<STRING_LITTERAL>{ESC_TAB}   { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = '\t'; string_buf_ptr++; } 
<STRING_LITTERAL>{ESC_BSP}   { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = '\b'; string_buf_ptr++; } 
<STRING_LITTERAL>{ESC_FF}    { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = '\f'; string_buf_ptr++; } 
<STRING_LITTERAL>{ESC_NL}    { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = '\n'; string_buf_ptr++; } 
<STRING_LITTERAL>{ESC_CHAR}  { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } if(yytext[1] == '\0') { cool_yylval.error_msg = "String contains null character."; *string_buf_ptr = '\0'; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = yytext[1]; string_buf_ptr++; } 
<STRING_LITTERAL>{ALL_CHAR}  { if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) { cool_yylval.error_msg = "String constant is too long."; BEGIN(STRING_ERROR); return (ERROR); } if(yytext[0] == '\0') { cool_yylval.error_msg = "String contains null character."; *string_buf_ptr = '\0'; BEGIN(STRING_ERROR); return (ERROR); } *string_buf_ptr = yytext[0]; string_buf_ptr++; } 

<STRING_LITTERAL>{NEWLINE}  { curr_lineno++; BEGIN(INITIAL); *string_buf_ptr = '\0'; cool_yylval.error_msg = "Unterminated string constant."; return (ERROR); }
<STRING_LITTERAL>{ESC_NEWLINE} { curr_lineno++; *string_buf_ptr = '\n'; string_buf_ptr++; }
<STRING_LITTERAL><<EOF>> { cool_yylval.error_msg = "EOF in string constant."; BEGIN(INITIAL); return (ERROR); }

<STRING_ERROR>{NEWLINE} { BEGIN(INITIAL); }
<STRING_ERROR>{QUOTE} { BEGIN(INITIAL); }
<STRING_ERROR>{ALL_CHAR} {}
 /*
  * Integer constants
  */

{INTEGER}		{ cool_yylval.symbol = new IntEntry(yytext, MAX_STR_CONST, str_count++); return (INT_CONST); }

 /*
  * Identifiers
  */

{TYPEID}		{ cool_yylval.symbol = new StringEntry(yytext, MAX_STR_CONST, str_count++); return (TYPEID); }
{OBJECTID}		{ cool_yylval.symbol = new StringEntry(yytext, MAX_STR_CONST, str_count++); return (OBJECTID); }

<INITIAL>{WHITESPACE} 		{ }

{ALL_CHAR}		{ cool_yylval.error_msg = yytext; return (ERROR); }
%%


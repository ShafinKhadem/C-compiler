%option yylineno

%{
#include "SymbolInfo.h"

#define YYSTYPE SymbolInfo*

#include "y.tab.h"

extern YYSTYPE yylval;
void yyerror(char *);

extern int cntLine, cntErr;

FILE *flog = fopen("1605045_log.txt", "w");


string convertSlashedToReal(const string &s) {
    string ns;
    int sz = int(s.size());
    for (int i = 0; i < sz; i++) {
        if (s[i]=='\\') {
            if (s[i+1]=='n') ns.push_back('\n');
            else if (s[i+1]=='t') ns.push_back('\t');
            else if (s[i+1]=='\\') ns.push_back('\\');
            else if (s[i+1]=='\'') ns.push_back('\'');
            else if (s[i+1]=='a') ns.push_back('\a');
            else if (s[i+1]=='f') ns.push_back('\f');
            else if (s[i+1]=='r') ns.push_back('\r');
            else if (s[i+1]=='b') ns.push_back('\b');
            else if (s[i+1]=='v') ns.push_back('\v');
            else if (s[i+1]=='0') ns.push_back('\0');
            else if (s[i+1]=='\r') i++;
            else i--;
            i++;
        } else {
            ns.push_back(s[i]);
        }
    }
    return ns;
}

%}

WHITESPACE [ \t\f\r\v]+
LETTER [a-zA-Z]
AlphaNumeric [a-zA-Z0-9]

%%

{WHITESPACE}    {}

\n  { cntLine++; }

"if"    	{
            yylval = new SymbolInfo(yytext, "IF");
			return IF;
		}

"else"	{
            yylval = new SymbolInfo(yytext, "ELSE");
			return ELSE;
		}

"for"   {
            yylval = new SymbolInfo(yytext, "FOR");
            return FOR;
        }

"while" {
            yylval = new SymbolInfo(yytext, "WHILE");
            return WHILE;
        }

"do"    {
            yylval = new SymbolInfo(yytext, "DO");
            return DO;
        }

"break" {
            yylval = new SymbolInfo(yytext, "BREAK");
            return BREAK;
        }

"int"   {
            yylval = new SymbolInfo(yytext, "INT");
            return INT;
        }

"char"  {
            yylval = new SymbolInfo(yytext, "CHAR");
            return CHAR;
        }

"float" {
            yylval = new SymbolInfo(yytext, "FLOAT");
            return FLOAT;
        }

"double"    {
                yylval = new SymbolInfo(yytext, "DOUBLE");
                return DOUBLE;
            }

"void"  {
            yylval = new SymbolInfo(yytext, "VOID");
            return VOID;
        }

"return"    {
                yylval = new SymbolInfo(yytext, "RETURN");
                return RETURN;
            }

"switch"    {
                yylval = new SymbolInfo(yytext, "SWITCH");
                return SWITCH;
            }

"case"  {
            yylval = new SymbolInfo(yytext, "CASE");
            return CASE;
        }

"default"    {
                yylval = new SymbolInfo(yytext, "DEFAULT");
                return DEFAULT;
            }

"continue"    {
                yylval = new SymbolInfo(yytext, "CONTINUE");
                return CONTINUE;
            }
"println"	{
	yylval = new SymbolInfo(yytext, "PRINTLN");
	return PRINTLN;
}

[0-9]+  {
    yylval = new SymbolInfo(yytext, "CONST_INT");
    return CONST_INT;
}

(([0-9]+\.?[0-9]*)|(\.[0-9]+))([Ee][+-]?[0-9]+)?    {
    yylval = new SymbolInfo(yytext, "CONST_FLOAT");
    return CONST_FLOAT;
}

[0-9]*\.?[0-9]*[Ee][+-]?[0-9]*\.[0-9\.]*  {
    fprintf(flog, "\nError at line %d: Ill formed number %s\n", cntLine, yytext);
    cntErr++;
}

[0-9]*\.[0-9]*\.[0-9\.]*([Ee][+-]?[0-9\.]+)?    {
    fprintf(flog, "\nError at line %d: Too many decimal point %s\n", cntLine, yytext);
    cntErr++;
}

'[^\'\n\\]'|"'\\n'"|"'\\t'"|"'\\\\'"|"'\\''"|"'\\a'"|"'\\f'"|"'\\r'"|"'\\b'"|"'\\v'"|"'\\0'"    {
    string s(yytext);
    s = s.substr(1, s.size()-2);
    s = convertSlashedToReal(s);
    yylval = new SymbolInfo(s, "CONST_CHAR");
    return CONST_CHAR;
}

''  {
    fprintf(flog, "\nError at line %d: Empty character constant error %s\n", cntLine, yytext);
    cntErr++;
}

('[^\'\n]*)|('\\')  {
    fprintf(flog, "\nError at line %d: Unterminated character %s\n", cntLine, yytext);
    cntErr++;
}

'[^\'\n][^\'\n]+'    {
    fprintf(flog, "\nError at line %d: Multi character constant error %s\n", cntLine, yytext);
    cntErr++;
}

\"([^\\\"\n]|(\\\r?\n)|(\\.))*\"   {
    string s(yytext);
    s = s.substr(1, s.size()-2);
    s = convertSlashedToReal(s);
    cntLine = yylineno;
    yylval = new SymbolInfo(yytext, "STRING");
    return STRING;
}

\"([^\\\"\n]|(\\\r?\n)|(\\.))* {
    fprintf(flog, "\nError at line %d: Unterminated String %s\n", cntLine, yytext);
    cntLine = yylineno, cntErr++;
}

\/\/(.|(\\\r?\n))*   {
    string str(yytext);
    str.erase(remove(str.begin(), str.end(), '\r'), str.end());
    cntLine = yylineno;
}

\/\*([^\*]|(\*[^\/]))*\*\/ {
    cntLine = yylineno;
}

\/\*([^\*]|(\*[^\/]))*   {
    fprintf(flog, "\nError at line %d: Unterminated comment %s\n", cntLine, yytext);
    cntLine = yylineno, cntErr++;
}

[\+-]   {
    yylval = new SymbolInfo(yytext, "ADDOP");
	return ADDOP;
}

[*\/%]  {
    yylval = new SymbolInfo(yytext, "MULOP");
    return MULOP;
}

\+\+    {
    yylval = new SymbolInfo(yytext, "INCOP");
    return INCOP;
}

--     {
    yylval = new SymbolInfo(yytext, "DECOP");
    return DECOP;
}

"<="|"<"|">="|">"|"=="|"!=" {
    yylval = new SymbolInfo(yytext, "RELOP");
    return RELOP;
}

=   {
    yylval = new SymbolInfo(yytext, "ASSIGNOP");
    return ASSIGNOP;
}

&&|\|\| {
    yylval = new SymbolInfo(yytext, "LOGICOP");
    return LOGICOP;
}

&|\||\^|<<|>>   {
    yylval = new SymbolInfo(yytext, "BITOP");
    return BITOP;
}

\!  {
    yylval = new SymbolInfo(yytext, "NOT");
    return NOT;
}

\(  {
    yylval = new SymbolInfo(yytext, "LPAREN");
    return LPAREN;
}

\)  {
    yylval = new SymbolInfo(yytext, "RPAREN");
    return RPAREN;
}

\{  {
    yylval = new SymbolInfo(yytext, "LCURL");
    return LCURL;
}

\}  {
    yylval = new SymbolInfo(yytext, "RCURL");
    return RCURL;
}

\[  {
    yylval = new SymbolInfo(yytext, "LTHIRD");
    return LTHIRD;
}

\]  {
    yylval = new SymbolInfo(yytext, "RTHIRD");
    return RTHIRD;
}

,   {
    yylval = new SymbolInfo(yytext, "COMMA");
    return COMMA;
}

;   {
    yylval = new SymbolInfo(yytext, "SEMICOLON");
    return SEMICOLON;
}

({LETTER}|_)({AlphaNumeric}|_)*   {
    yylval = new SymbolInfo(yytext, "ID");
    return ID;
}

({AlphaNumeric}|[\._])+ {  // 1D10.7 is invalid, but 1E10.7 is ill formed. 1.2.E1 is too many decimal
    fprintf(flog, "\nError at line %d: Invalid prefix on ID or invalid suffix on Number %s\n", cntLine, yytext);
    cntErr++;
}



.  {
    fprintf(flog, "\nError at line %d: Unrecognized character %s\n", cntLine, yytext);
    cntErr++;
}

%%

int yywrap(void)
{
    return 1;
}

%option yylineno

%{
#include "SymbolInfo.h"

#define YYSTYPE SymbolInfo*

#include "y.tab.h"

extern YYSTYPE yylval;
void yyerror(char *);

extern int cntLine, cntErr;

FILE *loglex = fopen("1605045_lexlog.txt", "w");


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
			fprintf(loglex, "\nLine No. %d: Token <IF> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "IF");
			return IF;
		}

"else"	{
			fprintf(loglex, "\nLine No. %d: Token <ELSE> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "ELSE");
			return ELSE;
		}

"for"   {
            fprintf(loglex, "\nLine No. %d: Token <FOR> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "FOR");
            return FOR;
        }

"while" {
            fprintf(loglex, "\nLine No. %d: Token <WHILE> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "WHILE");
            return WHILE;
        }

"do"    {
            fprintf(loglex, "\nLine No. %d: Token <DO> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "DO");
            return DO;
        }

"break" {
            fprintf(loglex, "\nLine No. %d: Token <BREAK> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "BREAK");
            return BREAK;
        }

"int"   {
            fprintf(loglex, "\nLine No. %d: Token <INT> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "INT");
            return INT;
        }

"char"  {
            fprintf(loglex, "\nLine No. %d: Token <CHAR> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "CHAR");
            return CHAR;
        }

"float" {
            fprintf(loglex, "\nLine No. %d: Token <FLOAT> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "FLOAT");
            return FLOAT;
        }

"double"    {
                fprintf(loglex, "\nLine No. %d: Token <DOUBLE> Lexeme %s found\n", cntLine, yytext);
                yylval = new SymbolInfo(yytext, "DOUBLE");
                return DOUBLE;
            }

"void"  {
            fprintf(loglex, "\nLine No. %d: Token <VOID> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "VOID");
            return VOID;
        }

"return"    {
                fprintf(loglex, "\nLine No. %d: Token <RETURN> Lexeme %s found\n", cntLine, yytext);
                yylval = new SymbolInfo(yytext, "RETURN");
                return RETURN;
            }

"switch"    {
                fprintf(loglex, "\nLine No. %d: Token <SWITCH> Lexeme %s found\n", cntLine, yytext);
                yylval = new SymbolInfo(yytext, "SWITCH");
                return SWITCH;
            }

"case"  {
            fprintf(loglex, "\nLine No. %d: Token <CASE> Lexeme %s found\n", cntLine, yytext);
            yylval = new SymbolInfo(yytext, "CASE");
            return CASE;
        }

"default"    {
                fprintf(loglex, "\nLine No. %d: Token <DEFAULT> Lexeme %s found\n", cntLine, yytext);
                yylval = new SymbolInfo(yytext, "DEFAULT");
                return DEFAULT;
            }

"continue"    {
                fprintf(loglex, "\nLine No. %d: Token <CONTINUE> Lexeme %s found\n", cntLine, yytext);
                yylval = new SymbolInfo(yytext, "CONTINUE");
                return CONTINUE;
            }
"println"	{
	fprintf(loglex, "\nLine No. %d: Token <PRINTLN> Lexeme %s found\n", cntLine, yytext);
	yylval = new SymbolInfo(yytext, "PRINTLN");
	return PRINTLN;
}

[0-9]+  {
    fprintf(loglex, "\nLine No. %d: Token <CONST_INT> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "CONST_INT");
    return CONST_INT;
}

(([0-9]+\.?[0-9]*)|(\.[0-9]+))([Ee][+-]?[0-9]+)?    {
    fprintf(loglex, "\nLine No. %d: Token <CONST_FLOAT> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "CONST_FLOAT");
    return CONST_FLOAT;
}

[0-9]*\.?[0-9]*[Ee][+-]?[0-9]*\.[0-9\.]*  {
    fprintf(loglex, "\nError at line %d: Ill formed number %s\n", cntLine, yytext);
    cntErr++;
}

[0-9]*\.[0-9]*\.[0-9\.]*([Ee][+-]?[0-9\.]+)?    {
    fprintf(loglex, "\nError at line %d: Too many decimal point %s\n", cntLine, yytext);
    cntErr++;
}

'[^\'\n\\]'|"'\\n'"|"'\\t'"|"'\\\\'"|"'\\''"|"'\\a'"|"'\\f'"|"'\\r'"|"'\\b'"|"'\\v'"|"'\\0'"    {
    string s(yytext);
    fprintf(loglex, "\nLine No. %d: Token <CONST_CHAR> Lexeme %s found\n", cntLine, yytext);
    s = s.substr(1, s.size()-2);
    s = convertSlashedToReal(s);
    yylval = new SymbolInfo(s, "CONST_CHAR");
    return CONST_CHAR;
}

''  {
    fprintf(loglex, "\nError at line %d: Empty character constant error %s\n", cntLine, yytext);
    cntErr++;
}

('[^\'\n]*)|('\\')  {
    fprintf(loglex, "\nError at line %d: Unterminated character %s\n", cntLine, yytext);
    cntErr++;
}

'[^\'\n][^\'\n]+'    {
    fprintf(loglex, "\nError at line %d: Multi character constant error %s\n", cntLine, yytext);
    cntErr++;
}

\"([^\\\"\n]|(\\\r?\n)|(\\.))*\"   {
    string s(yytext);
    s = s.substr(1, s.size()-2);
    s = convertSlashedToReal(s);
    fprintf(loglex, "\nLine No. %d: Token <STRING> Lexeme %s found\n", cntLine, yytext);
    cntLine = yylineno;
    yylval = new SymbolInfo(yytext, "STRING");
    return STRING;
}

\"([^\\\"\n]|(\\\r?\n)|(\\.))* {
    fprintf(loglex, "\nError at line %d: Unterminated String %s\n", cntLine, yytext);
    cntLine = yylineno, cntErr++;
}

\/\/(.|(\\\r?\n))*   {
    string str(yytext);
    str.erase(remove(str.begin(), str.end(), '\r'), str.end());
    fprintf(loglex, "\nLine No. %d: Token <COMMENT> Lexeme %s found\n", cntLine, str.c_str());
    cntLine = yylineno;
}

\/\*([^\*]|(\*[^\/]))*\*\/ {
    fprintf(loglex, "\nLine No. %d: Token <COMMENT> Lexeme %s found\n", cntLine, yytext);
    cntLine = yylineno;
}

\/\*([^\*]|(\*[^\/]))*   {
    fprintf(loglex, "\nError at line %d: Unterminated comment %s\n", cntLine, yytext);
    cntLine = yylineno, cntErr++;
}

[\+-]   {
	fprintf(loglex, "\nLine No. %d: Token <ADDOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "ADDOP");
	return ADDOP;
}

[*\/%]  {
    fprintf(loglex, "\nLine No. %d: Token <MULOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "MULOP");
    return MULOP;
}

\+\+    {
    fprintf(loglex, "\nLine No. %d: Token <INCOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "INCOP");
    return INCOP;
}

--     {
    fprintf(loglex, "\nLine No. %d: Token <DECOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "DECOP");
    return DECOP;
}

"<="|"<"|">="|">"|"=="|"!=" {
    fprintf(loglex, "\nLine No. %d: Token <RELOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "RELOP");
    return RELOP;
}

=   {
    fprintf(loglex, "\nLine No. %d: Token <ASSIGNOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "ASSIGNOP");
    return ASSIGNOP;
}

&&|\|\| {
    fprintf(loglex, "\nLine No. %d: Token <LOGICOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "LOGICOP");
    return LOGICOP;
}

&|\||\^|<<|>>   {
    fprintf(loglex, "\nLine No. %d: Token <BITOP> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "BITOP");
    return BITOP;
}

\!  {
    fprintf(loglex, "\nLine No. %d: Token <NOT> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "NOT");
    return NOT;
}

\(  {
    fprintf(loglex, "\nLine No. %d: Token <LPAREN> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "LPAREN");
    return LPAREN;
}

\)  {
    fprintf(loglex, "\nLine No. %d: Token <RPAREN> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "RPAREN");
    return RPAREN;
}

\{  {
    fprintf(loglex, "\nLine No. %d: Token <LCURL> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "LCURL");
    return LCURL;
}

\}  {
    fprintf(loglex, "\nLine No. %d: Token <RCURL> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "RCURL");
    return RCURL;
}

\[  {
    fprintf(loglex, "\nLine No. %d: Token <LTHIRD> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "LTHIRD");
    return LTHIRD;
}

\]  {
    fprintf(loglex, "\nLine No. %d: Token <RTHIRD> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "RTHIRD");
    return RTHIRD;
}

,   {
    fprintf(loglex, "\nLine No. %d: Token <COMMA> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "COMMA");
    return COMMA;
}

;   {
    fprintf(loglex, "\nLine No. %d: Token <SEMICOLON> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "SEMICOLON");
    return SEMICOLON;
}

({LETTER}|_)({AlphaNumeric}|_)*   {
    fprintf(loglex, "\nLine No. %d: Token <ID> Lexeme %s found\n", cntLine, yytext);
    yylval = new SymbolInfo(yytext, "ID");
    return ID;
}

({AlphaNumeric}|[\._])+ {  // 1D10.7 is invalid, but 1E10.7 is ill formed. 1.2.E1 is too many decimal
    fprintf(loglex, "\nError at line %d: Invalid prefix on ID or invalid suffix on Number %s\n", cntLine, yytext);
    cntErr++;
}



.  {
    fprintf(loglex, "\nError at line %d: Unrecognized character %s\n", cntLine, yytext);
    cntErr++;
}

%%

int yywrap(void)
{
    fclose(loglex);
    return 1;
}

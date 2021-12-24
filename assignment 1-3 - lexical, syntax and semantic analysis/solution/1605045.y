%{
#include "SymbolInfo.h"

#define YYSTYPE SymbolInfo*

int yyparse(void);
int yylex(void);
extern FILE *yyin;
extern char *yytext;
FILE *fin, *flog, *ferr;
int cntLine = 1, cntErr = 0;

void yyerror(char *msg) {
    fprintf(ferr, "\nSyntax Error at Line %d: %s at '%s'\n\n\n", cntLine, msg, yytext), cntErr++;
}

void err(string msg) {
    fprintf(ferr, "Error at Line %d: %s\n\n", cntLine, msg.c_str()), cntErr++;
}

#define nsi new SymbolInfo
#define gn(s) s->get_name()
#define pd(x, y) ds[dsi++] = { gn(x), y }
#define log(lhs, rhs, s) fprintf(flog, "At line no: %d %s : %s\n\n%s\n\n", cntLine, lhs, rhs, gn(s).c_str());
// fprintf(flog, "%s\n", s->declt.c_str());

string cn(const initializer_list<SymbolInfo*> &il) {
    string cs = "";
    for (auto &i : il) cs += i->get_name();
    return cs;
}

// --------------- Symbol table begins ---------------//{

#define inf 1000000007
using ull = unsigned long long;

class ScopeTable
{
    SymbolInfo **ht;
    static int cntScope;

    int hash(const string &s) const {
        ull h = 0, power = 1;
        for (auto &i : s) {
            h = (h+i*power)%inf;
            power = power*131%inf;
        }
        return int(h);
    }

public:
    ScopeTable *parentScope;
    int id, n;
    string returnType;

    ScopeTable(int _n) : n(_n), parentScope(0) { id = ++cntScope, ht = nsi*[n]{0}; }
    ~ScopeTable() {
        for (int i = 0; i < n; i++) {
            SymbolInfo *cur = ht[i], *nxt;
            while (cur) {
                nxt = cur->nxt;
                delete cur;
                cur = nxt;
            }
            ht[i] = 0;
        }
    }

    SymbolInfo* lookup(const string &name, bool only = 1) {
        int loc = hash(name)%n, pos = 0;
        SymbolInfo *cur = ht[loc];
        while (cur) {
            if (cur->get_name()==name) {
                printf("%s Found in ScopeTable# %d in position %d, %d\n", name.c_str(), id, loc, pos);
                return cur;
            }
            cur = cur->nxt, pos++;
        }
        if (only) printf("Not found\n");
        return 0;
    }

    SymbolInfo* insert(const string &name, const string &type, const string &decl = "") {
        if (lookup(name, 0)) {
            err("Multiple Declaration of "+name);
            return NULL;
        }
        int loc = hash(name)%n, pos = 0;
        if (ht[loc]) {
            SymbolInfo *cur = ht[loc];
            while (cur->nxt) cur = cur->nxt, pos++;
            return pos++, cur->nxt = nsi(name, type, decl);
        } else return ht[loc] = nsi(name, type, decl);
        return NULL;
    }

    bool delete_(const string &name) {
        int loc = hash(name)%n, pos = 0;
        SymbolInfo *cur = ht[loc], *prv = 0;
        while (cur) {
            if (cur->get_name()==name) {
                printf("Found in ScopeTable# %d in position %d, %d\n", id, loc, pos);
                if (prv) prv->nxt = cur->nxt;
                else ht[loc] = cur->nxt;
                delete cur;
                printf("Deleted entry\n");
                return 1;
            }
            prv = cur, cur = cur->nxt, pos++;
        }
        printf("Not found\n");
        return 0;
    }

    void print() {
        fprintf(flog, "\n ScopeTable # %d\n", id);
        for (int i = 0; i < n; i++) {
            if (!ht[i]) continue;
            fprintf(flog, " %d --> ", i);
            SymbolInfo *cur = ht[i];
            while (cur) {
                fprintf(flog, "<%s, %s> ", cur->get_name().c_str(), cur->get_type().c_str());
                fprintf(flog, "%s ", cur->declt.c_str());
                if (cur->isFunction) {
                    fprintf(flog, "(");
                    for (auto &i : cur->params) fprintf(flog, "%s ", i.c_str());
                    fprintf(flog, ")  ");
                }
                cur = cur->nxt;
            }
            fprintf(flog, "\n");
        }
    }
};

int ScopeTable::cntScope = 0;

class SymbolTable
{
    int sz;
public:
    SymbolTable(int _sz) : sz(_sz) { cur = new ScopeTable(sz); }
    ~SymbolTable() {
        while (cur) {
            ScopeTable *par = cur->parentScope;
            delete cur;
            cur = par;
        }
    }
    ScopeTable *cur;

    void enterScope(string returnType = "") {
        ScopeTable *st = new ScopeTable(sz);
        st->returnType = returnType;
        st->parentScope = cur, cur = st;
        fprintf(flog, " New ScopeTable with id %d created\n", cur->id);
    }

    void exitScope() {
        ScopeTable *par = cur->parentScope;
        fprintf(flog, " ScopeTable with id %d removed\n", cur->id);
        delete cur;
        cur = par;
    }

    SymbolInfo* insert(const string &name, const string &type) { return cur->insert(name, type); }

    SymbolInfo* insert(const string &name, const string &type, const string &decl) { return cur->insert(name, type, decl); }

    bool remove(const string &name) { return cur->delete_(name); }

    SymbolInfo* lookup(const string &name) {
        ScopeTable *now = cur;
        while (now) {
            SymbolInfo *ret = now->lookup(name, 0);
            if (ret) return ret;
            now = now->parentScope;
        }
        printf("%s Not found\n", name.c_str());
        return 0;
    }

    void printCur() { cur->print(); }

    void printAll() {
        ScopeTable *now = cur;
        while (now) {
            now->print(), now = now->parentScope;
        }
    }
} st(29);

// --------------- Symbol table ends ---------------//}


pair<string, string> ds[500];
int dsi = 0, linehack;
vector<vector<pair<string, string> > > args;   // 2D vector is must to handle nested function calls: f1(f2(a, b), c, d, f3(e, f), g)
string funcType;

void newScope() {
    st.enterScope(funcType);
    funcType = "";
    swap(cntLine, linehack);    // to give error at the line where parameter_list ended, not where lcurl is.
    for (int i = 0; i < dsi; i++) st.insert(ds[i].first, "ID", ds[i].second);
    dsi = 0;
    swap(cntLine, linehack);
}

void func(SymbolInfo *type, SymbolInfo *name, bool definition) {
    funcType = gn(type);
    SymbolInfo *x = st.lookup(gn(name));
    if (x) {
        string str = definition ? "definition and declaration" : "2 declarations";
        if (funcType!=x->declt) err("Return Type Mismatch between "+str+" of "+gn(x)+": "+funcType+" and "+x->declt);
        if (dsi!=x->params.size())
            err("Number of arguments Mismatch between "+str+" of "+gn(x)+": Expected "+to_string(x->params.size())+", given "+to_string(dsi));
        else
            for (int i = 0; i < dsi; i++) if (ds[i].second != x->params[i])
                err("Type Mismatch between "+str+" of "+gn(x)+": "+ds[i].first+"("+ds[i].second+") and "+x->params[i]);
    } else {
        SymbolInfo *si = st.insert(gn(name), "ID", funcType);
        si->isFunction = 1;
        for (int i = 0; i < dsi; i++) si->params.push_back(ds[i].second);
    }
    if (!definition) dsi = 0;
}

void typMach(SymbolInfo *res, SymbolInfo *ls, SymbolInfo *rs, bool isAssign = 0) {
    if (ls->declt=="" or rs->declt=="") return;
    if (ls->declt=="void" or rs->declt=="void") { err("Both sides must be non void: "+gn(res)); return; }
    if (ls->declt==rs->declt) res->declt = ls->declt;
    else {
        res->declt = "float";
        if (isAssign) err("Type Mismatch between "+gn(ls)+"("+ls->declt+") and "+gn(rs)+"("+rs->declt+")");
    }
}

bool mod(SymbolInfo *x, SymbolInfo *y) {
    if (x->declt=="int" and y->declt=="int") return 1;
    err("Non integer operand on modulus operator: "+gn(x)+"%"+gn(y));
    return 0;
}

void look(SymbolInfo *x, SymbolInfo *y, SymbolInfo *a) {
    if (!y) {
        SymbolInfo *z = st.lookup(gn(x));
        if (!z or (z->declt.substr(0, 3)!="int" and z->declt.substr(0, 5)!="float")) err("Undeclared Variable: "+gn(x));
        else {
            //if (z->declt=="intara" or z->declt=="floatara") err("Array access without index: "+gn(x));
            x->declt = z->declt;
        }
    } else {
        SymbolInfo *z = st.lookup(gn(y));
        if (!a) {
            if (!z or !z->isFunction) err("Undeclared function call: "+gn(y));
            else {
                int cntArgs = args.back().size();
                if (cntArgs!=z->params.size()) err("Wrong number of arguments in call of "+gn(z)+": Expected "+to_string(z->params.size())+", given "+to_string(cntArgs));
                else {
                    for (int i = 0; i < cntArgs; i++)
                        if (args.back()[i].second!=z->params[i])
                            err("Type Mismatch between function call and declaration of "+gn(x)+": "+args.back()[i].first+"("+args.back()[i].second+") and "+z->params[i]);
                }
                x->declt = z->declt;
            }
            args.pop_back();
        } else {
            if (a->declt!="int") { err("Non-integer Array Index: "+gn(x)); return; }
            if (!z or (z->declt!="intara" and z->declt!="floatara")) err("Use of index on non-Array: "+gn(x));
            else if (z->declt=="intara") x->declt = "int";
            else if (z->declt=="floatara") x->declt = "float";
        }
    }
}

void voidExpressionCheck(SymbolInfo *x) {
    if (x->declt=="void") err("Expression must be non void: "+gn(x));
}

void voidVariableCheck(SymbolInfo *x, SymbolInfo *y) {
    if (gn(x)=="void") err("Variable must be non void: "+gn(y));
}

%}

%token FOR WHILE DO BREAK INT CHAR FLOAT DOUBLE VOID RETURN SWITCH CASE DEFAULT CONTINUE LPAREN RPAREN LCURL RCURL LTHIRD RTHIRD SEMICOLON PRINTLN
%token CONST_INT CONST_FLOAT CONST_CHAR STRING ID
%token IF
%nonassoc LESS_ELSE
%nonassoc ELSE  // precedence is only for opreators, not tokens.

%left COMMA
%right ASSIGNOP
%nonassoc LOGICOP
%left BITOP
%nonassoc RELOP
%left ADDOP
%left MULOP
%right NOT
%left INCOP DECOP

%%

start : program
	{
		//write your code in this block in all the similar blocks below
	}
	;

program : program unit { $$ = nsi(gn($1)+"\n"+gn($2), "program"), log("program", "program unit", $$); }
	| unit { $$ = nsi(gn($1), "program"), log("program", "unit", $$); }
	;

unit : var_declaration { $$ = nsi(gn($1), "unit"), log("unit", "var_declaration", $$); }
     | func_declaration { $$ = nsi(gn($1), "unit"), log("unit", "func_declaration", $$); }
     | func_definition { $$ = nsi(gn($1), "unit"), log("unit", "func_definition", $$); }
     | error {dsi = 0;}
     ;

func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON {
             func($1, $2, 0);
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5, $6}), "func_declaration"), log("func_declaration", "type_specifier ID LPAREN parameter_list RPAREN SEMICOLON", $$);
         }
		| type_specifier ID LPAREN RPAREN SEMICOLON {
             func($1, $2, 0);
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5}), "func_declaration"), log("func_declaration", "type_specifier ID LPAREN RPAREN SEMICOLON", $$);
         }
		;

func_definition : type_specifier ID LPAREN parameter_list RPAREN {
             func($1, $2, 1);
         } compound_statement {
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5, $7}), "func_definition"), log("func_definition", "type_specifier ID LPAREN parameter_list RPAREN compound_statement", $$);
         }
		| type_specifier ID LPAREN RPAREN { func($1, $2, 1); }
            compound_statement { $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $6}), "func_definition"), log("func_definition", "type_specifier ID LPAREN RPAREN compound_statement", $$); }
 		;



/* (int, int, int) is a valid parameter_list which can be used in both func_definition & func_declaration. But it shouldn't be allowed in definition. */
parameter_list  : parameter_list COMMA type_specifier ID    {
             pd($4, gn($3)), linehack = cntLine;
             $$ = nsi(cn({$1, $2, $3})+" "+gn($4), "parameter_list"), log("parameter_list", "parameter_list COMMA type_specifier ID", $$);
             voidVariableCheck($3, $$);
         }
		| parameter_list COMMA type_specifier     {
             ds[dsi++] = { "", gn($3) };
             $$ = nsi(cn({$1, $2, $3}), "parameter_list"), log("parameter_list", "parameter_list COMMA type_specifier", $$);
             if (gn($3)=="void") err("Invalid use of void in: "+gn($$));
         }
 		| type_specifier ID  { pd($2, gn($1)); $$ = nsi(gn($1)+" "+gn($2), "parameter_list"), log("parameter_list", "type_specifier ID", $$); voidVariableCheck($1, $$); }
		| type_specifier  { if (gn($1)!="void") ds[dsi++] = { "", gn($1) }, $$ = nsi(gn($1), "parameter_list"), log("parameter_list", "type_specifier", $$); }
 		;


compound_statement : LCURL {
                 newScope();
             } statements RCURL {
                 $$ = nsi(gn($1)+"\n"+gn($3)+"\n"+gn($4), "compound_statement"), log("compound_statement", "LCURL statements RCURL", $$);
                 st.printAll(), st.exitScope();
             }
 		    | LCURL {
                 newScope();
             } RCURL {
                 $$ = nsi(gn($1)+gn($3), "compound_statement"), log("compound_statement", "LCURL RCURL", $$);
                 st.printAll(), st.exitScope();
             }
 		    ;

var_declaration : type_specifier declaration_list SEMICOLON {
    for (int i = 0; i < dsi; i++) st.insert(ds[i].first, "ID", gn($1)+ds[i].second);
    dsi = 0;
    $$ = nsi(gn($1)+" "+cn({$2, $3}), "var_declaration"), log("var_declaration", "type_specifier declaration_list SEMICOLON", $$);
    voidVariableCheck($1, $$);
}
;

type_specifier	: INT    { $$ = nsi(gn($1), "type_specifier"), log("type_specifier", "INT", $$); }
 		| FLOAT    { $$ = nsi(gn($1), "type_specifier"), log("type_specifier", "FLOAT", $$); }
 		| VOID    { $$ = nsi(gn($1), "type_specifier"), log("type_specifier", "VOID", $$); }
 		;

declaration_list : declaration_list COMMA ID    { pd($3, ""); $$ = nsi(cn({$1, $2, $3}), "declaration_list"), log("declaration_list", "declaration_list COMMA ID", $$); }
 		  | declaration_list COMMA ID LTHIRD CONST_INT RTHIRD {
                pd($3, "ara");
                $$ = nsi(cn({$1, $2, $3, $4, $5, $6}), "declaration_list"), log("declaration_list", "declaration_list COMMA ID LTHIRD CONST_INT RTHIRD", $$);
            }
 		  | ID { pd($1, ""), $$ = nsi(gn($1), "declaration_list"), log("declaration_list", "ID", $$); }
 		  | ID LTHIRD CONST_INT RTHIRD { pd($1, "ara"), $$ = nsi(cn({$1, $2, $3, $4}), "declaration_list"), log("declaration_list", "ID LTHIRD CONST_INT RTHIRD", $$); }
 		  ;

statements : statement  { $$ = nsi(gn($1), "statements"), log("statements", "statement", $$); }
	   | statements statement  { $$ = nsi(gn($1)+"\n"+gn($2), "statements"), log("statements", "statements statement", $$); }
       | error {dsi = 0;}
	   ;

statement : var_declaration     { $$ = nsi(gn($1), "statement"), log("statement", "var_declaration", $$); }
	  | expression_statement   { $$ = nsi(gn($1), "statement"), log("statement", "expression_statement", $$); }
	  | compound_statement     { $$ = nsi(gn($1), "statement"), log("statement", "compound_statement", $$); }
	  | FOR LPAREN expression_statement expression_statement expression RPAREN statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5, $6, $7}), "statement"), log("statement", "FOR LPAREN expression_statement expression_statement expression RPAREN statement", $$);
            voidExpressionCheck($4);
        }
	  | IF LPAREN expression RPAREN statement %prec LESS_ELSE  {
            $$ = nsi(cn({$1, $2, $3, $4, $5}), "statement"), log("statement", "IF LPAREN expression RPAREN statement", $$);
            voidExpressionCheck($3);
        }
	  | IF LPAREN expression RPAREN statement ELSE statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5, $6, $7}), "statement"), log("statement", "IF LPAREN expression RPAREN statement ELSE statement", $$);
            voidExpressionCheck($3);
        }
	  | WHILE LPAREN expression RPAREN statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5}), "statement"), log("statement", "WHILE LPAREN expression RPAREN statement", $$);
            voidExpressionCheck($3);
        }
	  | PRINTLN LPAREN ID RPAREN SEMICOLON     { $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5}), "statement"), log("statement", "PRINTLN LPAREN ID RPAREN SEMICOLON", $$); }
	  | RETURN expression SEMICOLON {
            $$ = nsi(gn($1)+" "+cn({$2, $3}), "statement"), log("statement", "RETURN expression SEMICOLON", $$);
            string ret = st.cur->returnType;
            if (ret!=$2->declt and ret!="" and $2->declt!="") err("Type mismatch between function's return type ("+ret+") and returned type ("+$2->declt+") in: "+gn($$));
        }
	  ;

expression_statement 	: SEMICOLON   { $$ = nsi(gn($1), "expression_statement"), log("expression_statement", "SEMICOLON", $$); }
			| expression SEMICOLON   { $$ = nsi(cn({$1, $2}), "expression_statement", $1->declt), log("expression_statement", "expression SEMICOLON", $$); }
			;


variable : ID   { $$ = nsi(gn($1), "variable"), look($$, 0, 0), log("variable", "ID", $$); }
	 | ID LTHIRD expression RTHIRD     { $$ = nsi(cn({$1, $2, $3, $4}), "variable"), look($$, $1, $3), log("variable", "ID LTHIRD expression RTHIRD", $$); }
	 ;

expression : logic_expression   { $$ = nsi(gn($1), "expression", $1->declt), log("expression", "logic_expression", $$); }
	   | variable ASSIGNOP logic_expression    { $$ = nsi(cn({$1, $2, $3}), "expression"), typMach($$, $1, $3, 1), log("expression", "variable ASSIGNOP logic_expression", $$); }
	   ;

logic_expression : rel_expression   { $$ = nsi(gn($1), "logic_expression", $1->declt), log("logic_expression", "rel_expression", $$); }
		 | rel_expression LOGICOP rel_expression  { $$ = nsi(cn({$1, $2, $3}), "logic_expression", "int"), log("logic_expression", "rel_expression LOGICOP rel_expression", $$); }
		 ;

rel_expression	: simple_expression  { $$ = nsi(gn($1), "rel_expression", $1->declt), log("rel_expression", "simple_expression", $$); }
		| simple_expression RELOP simple_expression   {
                $$ = nsi(cn({$1, $2, $3}), "rel_expression", "int"), log("rel_expression", "simple_expression RELOP simple_expression", $$);
                if ($1->declt=="void" or $3->declt=="void") err("Both sides must be non void: "+gn($$));
            }
		;

simple_expression : term    { $$ = nsi(gn($1), "simple_expression", $1->declt), log("simple_expression", "term", $$); }
		  | simple_expression ADDOP term  { $$ = nsi(cn({$1, $2, $3}), "simple_expression"), typMach($$, $1, $3), log("simple_expression", "simple_expression ADDOP term", $$); }
		  ;

term :	unary_expression     { $$ = nsi(gn($1), "term", $1->declt), log("term", "unary_expression", $$); }
     |  term MULOP unary_expression     { $$ = nsi(cn({$1, $2, $3}), "term"); if (gn($2)!="%" or mod($1, $3)) typMach($$, $1, $3); log("term", "term MULOP unary_expression", $$); }
     ;

unary_expression : ADDOP unary_expression   { $$ = nsi(cn({$1, $2}), "unary_expression", $2->declt), log("unary_expression", "ADDOP unary_expression", $$); }
		 | NOT unary_expression   { $$ = nsi(cn({$1, $2}), "unary_expression", $2->declt), log("unary_expression", "NOT unary_expression", $$); }
		 | factor     { $$ = nsi(gn($1), "unary_expression", $1->declt), log("unary_expression", "factor", $$); }
		 ;

factor	: variable   { $$ = nsi(gn($1), "factor", $1->declt), log("factor", "variable", $$); }
	| ID LPAREN argument_list RPAREN   { $$ = nsi(cn({$1, $2, $3, $4}), "factor"), look($$, $1, 0), log("factor", "ID LPAREN argument_list RPAREN", $$); }
	| LPAREN expression RPAREN     { $$ = nsi(cn({$1, $2, $3}), "factor", $2->declt), log("factor", "LPAREN expression RPAREN", $$); }
	| CONST_INT    { $$ = nsi(gn($1), "factor", "int"), log("factor", "CONST_INT", $$); }
	| CONST_FLOAT  { $$ = nsi(gn($1), "factor", "float"), log("factor", "CONST_FLOAT", $$); }
	| variable INCOP   { $$ = nsi(cn({$1, $2}), "factor", $1->declt), log("factor", "variable INCOP", $$); }
	| variable DECOP   { $$ = nsi(cn({$1, $2}), "factor", $1->declt), log("factor", "variable DECOP", $$); }
	;



argument_list : arguments   { $$ = nsi(gn($1), "argument_list"), log("argument_list", "arguments", $$); }
			  | /*empty*/  { $$ = nsi("", "argument_list"), log("argument_list", "", $$); args.push_back(vector<pair<string, string>>()); }
			  ;

arguments : arguments COMMA logic_expression    {
                $$ = nsi(cn({$1, $2, $3}), "arguments"), log("arguments", "arguments COMMA logic_expression", $$);
                args.back().emplace_back(gn($3), $3->declt);
            }
	      | logic_expression   {
                $$ = nsi(gn($1), "arguments"), log("arguments", "logic_expression", $$);
                args.push_back(vector<pair<string, string>>());
                args.back().emplace_back(gn($1), $1->declt);
            }
	      ;


%%
int main(int argc, char *argv[])
{

	if ((fin = fopen(argv[1], "r"))==NULL) {
		printf("Cannot Open Input File.\n");
		exit(1);
	}

	flog = fopen("1605045_log.txt", "w");
	ferr = fopen("1605045_error.txt", "w");

	yyin = fin;
	yyparse();

    fprintf(flog, "\n\nSymbol table:\n\n");
    st.printAll();
    fprintf(flog, "\nTotal Lines: %d\n\nTotal Errors: %d\n", cntLine, cntErr);
    fprintf(ferr, "Total Errors: %d\n", cntErr);

	fclose(flog);
	fclose(ferr);

	return 0;
}

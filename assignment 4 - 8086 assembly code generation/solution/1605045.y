%{
#include "SymbolInfo.h"

#define YYSTYPE SymbolInfo*

int yyparse(void);
int yylex(void);
extern FILE *yyin, *flog;
extern char *yytext;
FILE *fin;
int cntLine = 1, cntErr = 0;

void yyerror(char *msg) {
    fprintf(flog, "\nSyntax Error at Line %d: %s at '%s'\n\n\n", cntLine, msg, yytext), cntErr++;
}

void err(string msg) {
    fprintf(flog, "Error at Line %d: %s\n\n", cntLine, msg.c_str()), cntErr++;
}

#define nsi new SymbolInfo
#define gn(s) s->get_name()
#define pd(x, y) ds[dsi++] = { gn(x), y }

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

    int hash(const string &s) const {
        ull h = 0, power = 1;
        for (auto &i : s) {
            h = (h+i*power)%inf;
            power = power*131%inf;
        }
        return int(h);
    }

public:
    static int cntScope;
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
                // printf("%s Found in ScopeTable# %d in position %d, %d\n", name.c_str(), id, loc, pos);
                return cur;
            }
            cur = cur->nxt, pos++;
        }
        // if (only) printf("Not found\n");
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
                // printf("Found in ScopeTable# %d in position %d, %d\n", id, loc, pos);
                if (prv) prv->nxt = cur->nxt;
                else ht[loc] = cur->nxt;
                delete cur;
                // printf("Deleted entry\n");
                return 1;
            }
            prv = cur, cur = cur->nxt, pos++;
        }
        // printf("Not found\n");
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
        //fprintf(flog, " New ScopeTable with id %d created\n", cur->id);
    }

    void exitScope() {
        ScopeTable *par = cur->parentScope;
        //fprintf(flog, " ScopeTable with id %d removed\n", cur->id);
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
        // printf("%s Not found\n", name.c_str());
        return 0;
    }

    int scopeFind(const string &name) {
        ScopeTable *now = cur;
        while (now) {
            SymbolInfo *ret = now->lookup(name, 0);
            if (ret) return now->id;
            now = now->parentScope;
        }
        // printf("%s Not found\n", name.c_str());
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


// --------------- Semantic error things begin ---------------//{
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

SymbolInfo* func(SymbolInfo *type, SymbolInfo *name, bool definition) {
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
        x = st.insert(gn(name), "ID", funcType);
        x->isFunction = 1;
        for (int i = 0; i < dsi; i++) x->params.push_back(ds[i].second);
    }
    if (!definition) dsi = 0;
    return x;
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
// --------------- Semantic error things end ---------------//}



int labelCount=0;
int tempCount=0;


char *newLabel()
{
    char *lb= new char[4];
    strcpy(lb,"L");
    char b[3];
    sprintf(b,"%d", labelCount);
    labelCount++;
    strcat(lb,b);
    return lb;
}

char *newTemp()
{
    char *t= new char[4];
    strcpy(t,"t");
    char b[3];
    sprintf(b,"%d", tempCount);
    tempCount++;
    strcat(t,b);
    return t;
}


string vn(string name) {
    return name+to_string(st.scopeFind(name));
}


vector<string> paramNames, funcVars, vars, curArgs;
vector<pair<string, string> > aras;
string sizes[100], curFunc;
unordered_map<string, string> moa{{"<", "jl"}, {">", "jg"}, {"<=", "jle"}, {">=", "jge"}, {"==", "je"}, {"!=", "jne"}, {"+", "add"}, {"-", "sub"}, {"*", "imul"}, {"/", "idiv"}, {"%", "idiv"}};


void funcVarDecl(string s) {
    vars.push_back(s), funcVars.push_back(s);
}

string funcCode(string name, string body) {
    vars.push_back("ret_"+curFunc);
    string code = "\n"+name+" PROC\n";
    if (name=="main") code += "MOV AX, @DATA\nMOV DS, AX\n\n"+body+"return"+name+":\n\nMOV AH, 4CH\nINT 21H\nmain ENDP\n";
    else {
        code += "PUSH ax\nPUSH bx\nPUSH cx\nPUSH dx\n";
        code += body+"return"+name+":\n";
        code += "POP dx\nPOP cx\nPOP bx\nPOP ax\nret\n"+name+" ENDP\n";
    }
    return code;
}



void optimize(string code) {
    istringstream iss(code);
    string s, s1, s2, ps1, ps2, o;
    // auto &fout = cerr;
    ofstream fout;
    fout.open("optimized-Code.asm");
    while (getline(iss, s)) {
        istringstream iss2(s);
        auto &ok = iss2 >> o;
		if (!ok) fout << s << '\n';
        else if (o=="mov") {
            iss2 >> s1 >> s2;
            o = s1.substr(0, s1.size()-1);
            if (!((o==ps2 and s2==ps1) or (o==ps1 and s2==ps2))) fout << s << '\n';
            ps1 = o, ps2 = s2;
        } else {
            fout << s << '\n';
            ps1 = ps2 = "";
        }
    }
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
        string code = ".MODEL SMALL\n.STACK 100H\n.DATA\n\n";
        for (auto &i : vars) code += i+" dw ?\n";
        for (auto &i : aras) code += i.first+" dw "+i.second+" dup (?)\n";
        code += "\n.CODE\n"+$1->code+"\noutdec proc\n\
    push ax\n\
    push bx\n\
    push cx\n\
    push dx\n\
    or ax, ax\n\
    jge end_if\n\
    push ax\n\
    mov dl, '-'\n\
    mov ah, 2\n\
    int 21h\n\
    pop ax\n\
    neg ax\n\
end_if:\n\
    xor cx, cx\n\
    mov bx, 10\n\
repeat:\n\
    xor dx, dx\n\
    div bx\n\
    push dx\n\
    inc cx\n\
    or ax, ax\n\
    jne repeat\n\
    mov ah, 2\n\
print_loop:\n\
    pop dx\n\
    or dl, 30h\n\
    int 21h\n\
    loop print_loop\n\
    mov dl, 0dh\n\
    int 21h\n\
    mov dl, 0ah\n\
    int 21h\n\
    pop dx\n\
    pop cx\n\
    pop bx\n\
    pop ax\n\
    ret \n\
outdec endp \n\
END MAIN\n";
        ofstream fout;
        fout.open("code.asm");
		fout << code;
        optimize(code);
	}
	;

program : program unit { $$ = nsi(gn($1)+"\n"+gn($2), "program"); $$->code=$1->code+$2->code; }
	| unit { $$ = nsi(gn($1), "program"); $$->code=$1->code; }
	;

unit : var_declaration { $$ = nsi(gn($1), "unit"); $$->code=$1->code; }
     | func_declaration { $$ = nsi(gn($1), "unit"); $$->code=$1->code; }
     | func_definition { $$ = nsi(gn($1), "unit"); $$->code=$1->code; }
     | error {dsi = 0;}
     ;

func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON {
             func($1, $2, 0);
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5, $6}), "func_declaration");
         }
		| type_specifier ID LPAREN RPAREN SEMICOLON {
             func($1, $2, 0);
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5}), "func_declaration");
         }
		;

func_definition : type_specifier ID LPAREN parameter_list RPAREN {
        // function er parameter list er shobar jonyo ekta kore notun variable lagbe. shei list save kora lagbe pore call korar jonyo.
            for (int i = 0; i < dsi; i++) {
                string varName = ds[i].first+to_string(ScopeTable::cntScope+1);
                paramNames.push_back(varName), vars.push_back(varName);
            }
            funcVars.clear();
            curFunc = gn($2);

             func($1, $2, 1)->paramNames = paramNames, paramNames.clear();
         } compound_statement {
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5, $7}), "func_definition");
             $$->code = funcCode(gn($2), $7->code);
         }
		| type_specifier ID LPAREN RPAREN { funcVars.clear(); curFunc = gn($2); func($1, $2, 1)->paramNames = paramNames, paramNames.clear(); }
            compound_statement {
             $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $6}), "func_definition");
             $$->code = funcCode(gn($2), $6->code);
         }
 		;



/* (int, int, int) is a valid parameter_list which can be used in both func_definition & func_declaration. But it shouldn't be allowed in definition. */
parameter_list  : parameter_list COMMA type_specifier ID    {
             pd($4, gn($3)), linehack = cntLine;
             $$ = nsi(cn({$1, $2, $3})+" "+gn($4), "parameter_list");
             voidVariableCheck($3, $$);
         }
		| parameter_list COMMA type_specifier     {
             ds[dsi++] = { "", gn($3) };
             $$ = nsi(cn({$1, $2, $3}), "parameter_list");
             if (gn($3)=="void") err("Invalid use of void in: "+gn($$));
         }
 		| type_specifier ID  { pd($2, gn($1)); $$ = nsi(gn($1)+" "+gn($2), "parameter_list"); voidVariableCheck($1, $$); }
		| type_specifier  { if (gn($1)!="void") ds[dsi++] = { "", gn($1) }, $$ = nsi(gn($1), "parameter_list"); }
 		;


compound_statement : LCURL {
                 newScope();
             } statements RCURL {
                 $$ = nsi(gn($1)+"\n"+gn($3)+"\n"+gn($4), "compound_statement");
                 st.exitScope();
                 $$->code = $3->code;
             }
 		    | LCURL {
                 newScope();
             } RCURL {
                 $$ = nsi(gn($1)+gn($3), "compound_statement");
                 st.exitScope();
             }
 		    ;

var_declaration : type_specifier declaration_list SEMICOLON {
    for (int i = 0; i < dsi; i++) {
        string varName = ds[i].first+to_string(ScopeTable::cntScope);
        if (ds[i].second=="ara") aras.push_back({varName, sizes[i]});
        else funcVarDecl(varName);
        st.insert(ds[i].first, "ID", gn($1)+ds[i].second);
    }
    dsi = 0;
    $$ = nsi(gn($1)+" "+cn({$2, $3}), "var_declaration");
    voidVariableCheck($1, $$);
}
;

type_specifier	: INT    { $$ = nsi(gn($1), "type_specifier"); }
 		| FLOAT    { $$ = nsi(gn($1), "type_specifier"); }
 		| VOID    { $$ = nsi(gn($1), "type_specifier"); }
 		;

declaration_list : declaration_list COMMA ID    { pd($3, ""); $$ = nsi(cn({$1, $2, $3}), "declaration_list"); }
 		  | declaration_list COMMA ID LTHIRD CONST_INT RTHIRD {
                sizes[dsi] = gn($5), pd($3, "ara");
                $$ = nsi(cn({$1, $2, $3, $4, $5, $6}), "declaration_list");
            }
 		  | ID { pd($1, ""), $$ = nsi(gn($1), "declaration_list"); }
 		  | ID LTHIRD CONST_INT RTHIRD { sizes[dsi] = gn($3), pd($1, "ara"), $$ = nsi(cn({$1, $2, $3, $4}), "declaration_list"); }
 		  ;

statements : statement  { $$ = nsi(gn($1), "statements"); $$->code=$1->code; }
	   | statements statement  { $$ = nsi(gn($1)+"\n"+gn($2), "statements"); $$->code=$1->code+$2->code; }
       | error {dsi = 0;}
	   ;

statement : var_declaration     { $$ = nsi(gn($1), "statement"); }
	  | expression_statement   { $$ = nsi(gn($1), "statement"); $$->code=$1->code; }
	  | compound_statement     { $$ = nsi(gn($1), "statement"); $$->code=$1->code; }
	  | FOR LPAREN expression_statement expression_statement expression RPAREN statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5, $6, $7}), "statement");
            voidExpressionCheck($4);
            $$->code = $3->code;
            string label1 = newLabel(), label2 = newLabel();
            $$->code += label1+":\n";
            $$->code += $4->code+"cmp "+$4->varName+", 0\n";
            $$->code += "je "+label2+"\n";
            $$->code += $7->code+$5->code+"jmp "+label1+"\n";
            $$->code += label2+":\n";
        }
	  | IF LPAREN expression RPAREN statement %prec LESS_ELSE  {
            $$ = nsi(cn({$1, $2, $3, $4, $5}), "statement");
            voidExpressionCheck($3);
            $$->code = $3->code;
            char *label=newLabel();
            $$->code+="mov ax, "+$3->varName+"\n";
            $$->code+="cmp ax, 0\n";
            $$->code+="je "+string(label)+"\n";
            $$->code+=$5->code;
            $$->code+=string(label)+":\n";
        }
	  | IF LPAREN expression RPAREN statement ELSE statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5, $6, $7}), "statement");
            voidExpressionCheck($3);
            string label1 = newLabel(), label2 = newLabel();
            $$->code = $3->code+"cmp "+$3->varName+", 0\n";
            $$->code += "je "+label1+"\n";
            $$->code += $5->code+"jmp "+label2+"\n";
            $$->code += label1+":\n"+$7->code;
            $$->code += label2+":\n";
        }
	  | WHILE LPAREN expression RPAREN statement   {
            $$ = nsi(cn({$1, $2, $3, $4, $5}), "statement");
            voidExpressionCheck($3);
            string label1 = newLabel(), label2 = newLabel();
            $$->code = label1+":\n";
            $$->code += $3->code+"cmp "+$3->varName+", 0\n";
            $$->code += "je "+label2+"\n";
            $$->code += $5->code+"jmp "+label1+"\n";
            $$->code += label2+":\n";
        }
	  | PRINTLN LPAREN ID RPAREN SEMICOLON {
            $$ = nsi(gn($1)+" "+cn({$2, $3, $4, $5}), "statement");
            $$->code = "mov ax, "+vn(gn($3))+"\ncall outdec\n";
        }
	  | RETURN expression SEMICOLON {
            $$ = nsi(gn($1)+" "+cn({$2, $3}), "statement");
            string ret = st.cur->returnType;
            if (ret!=$2->declt and ret!="" and $2->declt!="") err("Type mismatch between function's return type ("+ret+") and returned type ("+$2->declt+") in: "+gn($$));
            $$->code = $2->code+"mov ax, "+$2->varName+"\nmov ret_"+curFunc+", ax\njmp return"+curFunc+"\n";
        }
	  ;

expression_statement 	: SEMICOLON   { $$ = nsi(gn($1), "expression_statement"); }
			| expression SEMICOLON   { $$ = nsi(cn({$1, $2}), "expression_statement", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
			;


variable : ID   { $$ = nsi(gn($1), "variable"), look($$, 0, 0); $$->varName = vn(gn($1)); }
	 | ID LTHIRD expression RTHIRD     {
        $$ = nsi(cn({$1, $2, $3, $4}), "variable"), look($$, $1, $3);
        $$->code = $3->code+"mov bx, "+$3->varName+"\nadd bx, bx\n";
        $$->varName = vn(gn($1))+"[bx]";
    }
	 ;

expression : logic_expression   { $$ = nsi(gn($1), "expression", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
	   | variable ASSIGNOP logic_expression    {
            $$ = nsi(cn({$1, $2, $3}), "expression"), typMach($$, $1, $3, 1); $$->varName=$1->varName;
            $$->code = $3->code+"mov ax, "+$3->varName+"\n"+$1->code+"mov "+$1->varName+", ax\n";
        }
	   ;

logic_expression : rel_expression   { $$ = nsi(gn($1), "logic_expression", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
	   | rel_expression LOGICOP rel_expression  {
            $$ = nsi(cn({$1, $2, $3}), "logic_expression", "int");
            $$->code = $1->code+$3->code;
            string label1 = newLabel(), label2 = newLabel(), tmp = newTemp();
            $$->varName = tmp, funcVarDecl(tmp);
            if (gn($2)=="&&") {
                $$->code += "cmp "+$1->varName+", 0\n";
                $$->code += "je "+label1+"\n";
                $$->code += "cmp "+$3->varName+", 0\n";
                $$->code += "je "+label1+"\n";
                $$->code += "mov "+tmp+", 1\njmp "+label2+"\n";
                $$->code += label1+":\n"+"mov "+tmp+", 0\n";
                $$->code += label2+":\n";
            } else if (gn($2)=="||") {
                $$->code += "cmp "+$1->varName+", 0\n";
                $$->code += "jne "+label1+"\n";
                $$->code += "cmp "+$3->varName+", 0\n";
                $$->code += "jne "+label1+"\n";
                $$->code += "mov "+tmp+", 0\njmp "+label2+"\n";
                $$->code += label1+":\n"+"mov "+tmp+", 1\n";
                $$->code += label2+":\n";
            }
        }
	   ;

rel_expression	: simple_expression  { $$ = nsi(gn($1), "rel_expression", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
	   | simple_expression RELOP simple_expression   {
            $$ = nsi(cn({$1, $2, $3}), "rel_expression", "int");
            if ($1->declt=="void" or $3->declt=="void") err("Both sides must be non void: "+gn($$));
            $$->code = $1->code+$3->code;
            string label1 = newLabel(), label2 = newLabel(), tmp = newTemp();
            $$->varName = tmp, funcVarDecl(tmp);
            $$->code += "mov ax, "+$1->varName+"\ncmp ax, "+$3->varName+"\n";
            $$->code += moa[gn($2)]+" "+label1+"\n";
            $$->code += "mov "+tmp+", 0\njmp "+label2+"\n";
            $$->code += label1+":\n"+"mov "+tmp+", 1\n";
            $$->code += label2+":\n";
        }
	   ;

simple_expression : term    { $$ = nsi(gn($1), "simple_expression", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
		| simple_expression ADDOP term  {
            $$ = nsi(cn({$1, $2, $3}), "simple_expression"), typMach($$, $1, $3);
            $$->code = $1->code+$3->code;
            string tmp = newTemp();
            $$->varName = tmp, funcVarDecl(tmp);
            $$->code += "mov ax, "+$1->varName+"\n"+moa[gn($2)]+" ax, "+$3->varName+"\nmov "+tmp+", ax\n";
        }
		;

term :	unary_expression     { $$ = nsi(gn($1), "term", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
   |  term MULOP unary_expression     {
        $$ = nsi(cn({$1, $2, $3}), "term"); if (gn($2)!="%" or mod($1, $3)) typMach($$, $1, $3);
        $$->code = $1->code+$3->code;
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        if (gn($2)!="*") $$->code += "mov dx, 0\n";
        $$->code += "mov ax, "+$1->varName+"\n"+moa[gn($2)]+" "+$3->varName+"\n";
        $$->code += "mov "+tmp+", "+(gn($2)=="%" ? "dx" : "ax")+"\n";
    }
   ;

unary_expression : ADDOP unary_expression   {
           $$ = nsi(cn({$1, $2}), "unary_expression", $2->declt);
           $$->code = $2->code;
           if (gn($1)=="-") $$->code += "neg "+$2->varName+"\n";
           $$->varName=$2->varName;
       }
   | NOT unary_expression   {
        $$ = nsi(cn({$1, $2}), "unary_expression", $2->declt);
        $$->code = $2->code;
        $$->code += "not "+$2->varName+"\n";
        $$->varName=$2->varName;
    }
   | factor { $$ = nsi(gn($1), "unary_expression", $1->declt); $$->code=$1->code, $$->varName=$1->varName; }
   ;

factor	: variable   {
        $$ = nsi(gn($1), "factor", $1->declt);
        $$->code = $1->code;
        if ($$->get_type()=="intara" or $$->get_type()=="floatara") {
            string tmp = newTemp();
            $$->varName = tmp, funcVarDecl(tmp);
            $$->code += "mov ax, "+$1->varName+"\nmov "+tmp+", ax\n";
        } else {
            $$->varName = $1->varName;
        }
    }
   | ID LPAREN argument_list RPAREN   {
        $$ = nsi(cn({$1, $2, $3, $4}), "factor"), look($$, $1, 0);
        $$->code = $3->code;
        auto si = st.lookup(gn($1));
        int sz = int(si->paramNames.size());
        string pop = "";
        if (curFunc!="main") {  // paramNames and funcVars stack e rakha lagbe only for recursion.
            for (auto &i : si->paramNames) $$->code += "PUSH "+i+"\n", pop = "POP "+i+"\n"+pop;
            for (auto &i : funcVars) $$->code += "PUSH "+i+"\n", pop = "POP "+i+"\n"+pop;
        }
        for (int i = 0; i < sz; i++) {
            $$->code += "mov ax, "+curArgs[i]+"\nmov "+si->paramNames[i]+", ax\n";
        }
        curArgs.clear();
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        $$->code += "call "+gn($1)+"\nmov ax, ret_"+gn($1)+"\nmov "+tmp+", ax\n"+pop;
    }
   | LPAREN expression RPAREN     { $$ = nsi(cn({$1, $2, $3}), "factor", $2->declt); $$->code=$2->code, $$->varName=$2->varName; }
   | CONST_INT    {
        $$ = nsi(gn($1), "factor", "int");
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        $$->code = "mov "+tmp+", "+gn($1)+"\n";
    }
   | CONST_FLOAT  {
        $$ = nsi(gn($1), "factor", "float");
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        $$->code = "mov "+tmp+", "+gn($1)+"\n";
    }
   | variable INCOP   {
        $$ = nsi(cn({$1, $2}), "factor", $1->declt);
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        $$->code = $1->code+"mov ax, "+$1->varName+"\nmov "+tmp+", ax\ninc "+$1->varName+"\n";
    }
   | variable DECOP   {
        $$ = nsi(cn({$1, $2}), "factor", $1->declt);
        string tmp = newTemp();
        $$->varName = tmp, funcVarDecl(tmp);
        $$->code = $1->code+"mov ax, "+$1->varName+"\nmov "+tmp+", ax\ndec "+$1->varName+"\n";
    }
   ;



argument_list : arguments   { $$ = nsi(gn($1), "argument_list"); $$->code=$1->code; }
			  | /*empty*/  { $$ = nsi("", "argument_list"); args.push_back(vector<pair<string, string>>()); }
			  ;

arguments : arguments COMMA logic_expression    {
                $$ = nsi(cn({$1, $2, $3}), "arguments");
                args.back().emplace_back(gn($3), $3->declt);
                $$->code = $1->code+$3->code;
                curArgs.push_back($3->varName);
            }
	      | logic_expression   {
                $$ = nsi(gn($1), "arguments");
                args.push_back(vector<pair<string, string>>());
                args.back().emplace_back(gn($1), $1->declt);
                $$->code=$1->code;
                curArgs.push_back($1->varName);
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

	yyin = fin;
	yyparse();

    fprintf(flog, "\nTotal Lines: %d\n\nTotal Errors: %d\n", cntLine, cntErr);

	fclose(flog);

	return 0;
}

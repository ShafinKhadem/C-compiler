#include<bits/stdc++.h>
using namespace std;

class SymbolInfo
{
    string name, type;
public:
    SymbolInfo *nxt;
    string declt, varName, code;
    bool isFunction;
    vector<string> params;
	vector<string> paramNames;

    SymbolInfo(const string &_name, const string &_type) : name(_name), type(_type) { nxt = 0; isFunction = 0; code = ""; }
    SymbolInfo(const string &_name, const string &_type, const string &_declt) : name(_name), type(_type), declt(_declt) { nxt = 0; isFunction = 0; code = ""; }

    string get_name () { return name; }
    string get_type () { return type; }
};

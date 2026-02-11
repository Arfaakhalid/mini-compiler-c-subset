%{
#include <stdio.h>
#include <string>
#include <map>
#include <vector>
#include <iostream>

using namespace std;

extern int yylex();
void yyerror(const char *s);

// Symbol Table
map<string, int> symbolTable;
int tempCount = 0;

// Function to generate unique temporary variables
string newTemp() {
    return "t" + to_string(tempCount++);
}
%}

%union {
    int num;
    char* str;
}

%token <num> NUM
%token <str> ID
%token INT IF ELSE ASSIGN PLUS MINUS SEMI LBRACE RBRACE
%type <str> exp

%%
program:
    statements
    ;

statements:
    statement statements
    | /* empty */
    ;

statement:
    INT ID SEMI { 
        symbolTable[$2] = 0; 
        cout << "; Declared variable: " << $2 << endl;
    }
    | ID ASSIGN exp SEMI {
        if(symbolTable.find($1) == symbolTable.end()) {
            yyerror(("Undefined variable: " + string($1)).c_str());
        } else {
            cout << $1 << " = " << $3 << endl;
        }
    }
    ;

exp:
    NUM { 
        string t = newTemp();
        cout << t << " = " << $1 << endl;
        $$ = strdup(t.c_str());
    }
    | ID {
        if(symbolTable.find($1) == symbolTable.end()) {
            yyerror("Semantic Error: Variable not declared.");
        }
        $$ = $1;
    }
    | exp PLUS exp {
        string t = newTemp();
        cout << t << " = " << $1 << " + " << $3 << endl;
        $$ = strdup(t.c_str());
    }
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}
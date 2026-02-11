
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <iomanip>
#include <cctype>
#include <algorithm>
#include <stack>
#include <memory>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <unordered_map>
#include <functional>

using namespace std;
using namespace chrono;

#define RESET "\033[0m"
#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define BLUE "\033[34m"
#define MAGENTA "\033[35m"
#define CYAN "\033[36m"
#define BOLD "\033[1m"

enum TokenType {
TK_INT, TK_FLOAT, TK_CHAR, TK_VOID, TK_BOOL, TK_STRING,
TK_IF, TK_ELSE, TK_ELSEIF, TK_WHILE, TK_DO, TK_FOR, TK_SWITCH, TK_CASE, TK_DEFAULT,
TK_RETURN, TK_BREAK, TK_CONTINUE,
TK_PRINTF, TK_SCANF, TK_PRINT,
TK_IDENTIFIER, TK_INTEGER, TK_FLOATNUM, TK_STRINGLIT, TK_CHARLIT,
TK_PLUS, TK_MINUS, TK_MULTIPLY, TK_DIVIDE, TK_MOD,
TK_ASSIGN, TK_EQUAL, TK_NOTEQUAL,
TK_LESS, TK_GREATER, TK_LESSEQ, TK_GREATEREQ,
TK_AND, TK_OR, TK_NOT,
TK_INCREMENT, TK_DECREMENT,
TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE,
TK_LBRACKET, TK_RBRACKET,
TK_SEMICOLON, TK_COMMA, TK_DOT, TK_COLON, TK_QUESTION, TK_ARROW,
TK_EOF, TK_ERROR,
TK_TRUE, TK_FALSE, TK_NULL,
TK_INCLUDE, TK_DEFINE, TK_IFDEF, TK_ENDIF, TK_UNDEF
};

struct Token {
TokenType type;
string lexeme;
int line;
int column;

Token(TokenType t = TK_ERROR, string l = "", int ln = 0, int col = 0)
: type(t), lexeme(l), line(ln), column(col) {}

string typeToString() const {
switch(type) {
case TK_INT: return "INT";
case TK_FLOAT: return "FLOAT";
case TK_CHAR: return "CHAR";
case TK_VOID: return "VOID";
case TK_BOOL: return "BOOL";
case TK_STRING: return "STRING";
case TK_IF: return "IF";
case TK_ELSE: return "ELSE";
case TK_ELSEIF: return "ELSEIF";
case TK_WHILE: return "WHILE";
case TK_DO: return "DO";
case TK_FOR: return "FOR";
case TK_SWITCH: return "SWITCH";
case TK_CASE: return "CASE";
case TK_DEFAULT: return "DEFAULT";
case TK_RETURN: return "RETURN";
case TK_BREAK: return "BREAK";
case TK_CONTINUE: return "CONTINUE";
case TK_PRINTF: return "PRINTF";
case TK_SCANF: return "SCANF";
case TK_PRINT: return "PRINT";
case TK_IDENTIFIER: return "IDENTIFIER";
case TK_INTEGER: return "INTEGER";
case TK_FLOATNUM: return "FLOATNUM";
case TK_STRINGLIT: return "STRINGLIT";
case TK_CHARLIT: return "CHARLIT";
case TK_PLUS: return "PLUS";
case TK_MINUS: return "MINUS";
case TK_MULTIPLY: return "MULTIPLY";
case TK_DIVIDE: return "DIVIDE";
case TK_MOD: return "MOD";
case TK_ASSIGN: return "ASSIGN";
case TK_EQUAL: return "EQUAL";
case TK_NOTEQUAL: return "NOTEQUAL";
case TK_LESS: return "LESS";
case TK_GREATER: return "GREATER";
case TK_LESSEQ: return "LESSEQ";
case TK_GREATEREQ: return "GREATEREQ";
case TK_AND: return "AND";
case TK_OR: return "OR";
case TK_NOT: return "NOT";
case TK_INCREMENT: return "INCREMENT";
case TK_DECREMENT: return "DECREMENT";
case TK_LPAREN: return "LPAREN";
case TK_RPAREN: return "RPAREN";
case TK_LBRACE: return "LBRACE";
case TK_RBRACE: return "RBRACE";
case TK_LBRACKET: return "LBRACKET";
case TK_RBRACKET: return "RBRACKET";
case TK_SEMICOLON: return "SEMICOLON";
case TK_COMMA: return "COMMA";
case TK_DOT: return "DOT";
case TK_COLON: return "COLON";
case TK_QUESTION: return "QUESTION";
case TK_ARROW: return "ARROW";
case TK_TRUE: return "TRUE";
case TK_FALSE: return "FALSE";
case TK_NULL: return "NULL";
case TK_INCLUDE: return "INCLUDE";
case TK_DEFINE: return "DEFINE";
case TK_IFDEF: return "IFDEF";
case TK_ENDIF: return "ENDIF";
case TK_UNDEF: return "UNDEF";
case TK_EOF: return "EOF";
default: return "ERROR";
}
}
};

struct CompilerError {
int line;
int column;
string message;
string phase;
string severity;

CompilerError(int l, int c, const string& msg,
const string& p = "Syntax", const string& s = "ERROR")
: line(l), column(c), message(msg), phase(p), severity(s) {}

void display() const {
string color = (severity == "ERROR") ? RED : YELLOW;
cout << color << phase << " " << severity << " at line " << line
<< ", column " << column << ": " << message << RESET << endl;
}

bool operator<(const CompilerError& other) const {
if (line != other.line) return line < other.line;
if (column != other.column) return column < other.column;
return message < other.message;
}
};

enum SymbolType { VAR_SYM, FUNC_SYM, ARRAY_SYM, STRUCT_SYM };
enum DataType { INT_TYPE, FLOAT_TYPE, CHAR_TYPE, VOID_TYPE, BOOL_TYPE, STRING_TYPE, UNKNOWN_TYPE };

struct Symbol {
string name;
SymbolType symType;
DataType dataType;
int line;
int scope;
bool initialized;
bool isUsed;
bool isFunctionDefined;
vector<DataType> paramTypes;
int arraySize;

Symbol(string n = "", SymbolType st = VAR_SYM, DataType dt = INT_TYPE,
int l = 0, int s = 0, bool init = false, bool used = false,
bool funcDefined = false, int arrSize = -1)
: name(n), symType(st), dataType(dt), line(l), scope(s),
initialized(init), isUsed(used), isFunctionDefined(funcDefined),
arraySize(arrSize) {}

string typeToString() const {
switch(symType) {
case VAR_SYM: return "variable";
case FUNC_SYM: return "function";
case ARRAY_SYM: return "array";
case STRUCT_SYM: return "struct";
default: return "unknown";
}
}

string dataTypeToString() const {
switch(dataType) {
case INT_TYPE: return "int";
case FLOAT_TYPE: return "float";
case CHAR_TYPE: return "char";
case VOID_TYPE: return "void";
case BOOL_TYPE: return "bool";
case STRING_TYPE: return "string";
default: return "unknown";
}
}
};

class SymbolTable {
private:
vector<map<string, Symbol>> tables;
vector<CompilerError> errors;
int currentScope;

public:
SymbolTable() : currentScope(0) {
tables.push_back(map<string, Symbol>());
}

void enterScope() {
currentScope++;
tables.push_back(map<string, Symbol>());
}

void exitScope() {
if (currentScope > 0) {
// Check for unused variables in current scope
for (auto& entry : tables.back()) {
Symbol& sym = entry.second;
if (sym.symType == VAR_SYM && !sym.isUsed) {
errors.push_back(CompilerError(
sym.line, 1,
"Unused variable '" + sym.name + "'", "Semantic", "WARNING"
));
}
}
tables.pop_back();
currentScope--;
}
}

bool insert(const string& name, const Symbol& sym) {
// Check for redeclaration in current scope
if (tables.back().find(name) != tables.back().end()) {
Symbol existing = tables.back()[name];
errors.push_back(CompilerError(
sym.line, 1,
"Redeclaration of '" + name + "' (previously declared at line " +
to_string(existing.line) + ")", "Semantic", "ERROR"
));
return false;
}
tables.back()[name] = sym;
return true;
}

Symbol* lookup(const string& name) {
for (int i = tables.size() - 1; i >= 0; i--) {
auto it = tables[i].find(name);
if (it != tables[i].end()) {
return &(it->second);
}
}
return nullptr;
}

void markUsed(const string& name) {
Symbol* sym = lookup(name);
if (sym) sym->isUsed = true;
}

void markInitialized(const string& name) {
Symbol* sym = lookup(name);
if (sym) sym->initialized = true;
}

void markFunctionDefined(const string& name) {
Symbol* sym = lookup(name);
if (sym) sym->isFunctionDefined = true;
}

vector<CompilerError> getErrors() {
// Remove duplicate errors
set<CompilerError> uniqueErrors(errors.begin(), errors.end());
errors.assign(uniqueErrors.begin(), uniqueErrors.end());
return errors;
}

void checkUninitialized() {
for (size_t i = 0; i < tables.size(); i++) {
for (auto& entry : tables[i]) {
Symbol& sym = entry.second;
if (sym.symType == VAR_SYM && sym.isUsed && !sym.initialized) {
errors.push_back(CompilerError(
sym.line, 1,
"Variable '" + sym.name + "' may be used uninitialized", "Semantic", "WARNING"
));
}
if (sym.symType == FUNC_SYM && !sym.isFunctionDefined &&
sym.name != "printf" && sym.name != "scanf" && sym.name != "print") {
errors.push_back(CompilerError(
sym.line, 1,
"Function '" + sym.name + "' declared but not defined", "Semantic", "ERROR"
));
}
}
}
}

void display() {
cout << CYAN << "\nSYMBOL TABLE:" << RESET << endl;
cout << "+------------+------------+------------+------+-------+----------+--------+" << endl;
cout << "| Name | Type | Data Type | Line | Scope | Init | Used |" << endl;
cout << "+------------+------------+------------+------+-------+----------+--------+" << endl;

int totalSymbols = 0;
for (size_t i = 0; i < tables.size(); i++) {
for (auto& entry : tables[i]) {
Symbol& sym = entry.second;
totalSymbols++;

cout << "| " << left << setw(10) << sym.name
<< " | " << setw(10) << sym.typeToString()
<< " | " << setw(10) << sym.dataTypeToString()
<< " | " << setw(4) << sym.line
<< " | " << setw(5) << i
<< " | " << setw(8) << (sym.initialized ? "Yes" : "No")
<< " | " << setw(6) << (sym.isUsed ? "Yes" : "No") << " |" << endl;
}
}
cout << "+------------+------------+------------+------+-------+----------+--------+" << endl;
cout << "Total symbols: " << totalSymbols << " in " << tables.size() << " scopes" << endl;
}
};

struct TACNode {
string op;
string arg1;
string arg2;
string result;
int line;

TACNode(const string& o = "", const string& a1 = "",
const string& a2 = "", const string& r = "", int l = 0)
: op(o), arg1(a1), arg2(a2), result(r), line(l) {}

void display() const {
if (op == "LABEL") {
cout << result << ":" << endl;
} else if (op == "IF") {
cout << "IF " << arg1 << " " << op << " " << arg2 << " GOTO " << result << endl;
} else if (op == "GOTO") {
cout << "GOTO " << result << endl;
} else if (op == "PARAM") {
cout << "PARAM " << arg1 << endl;
} else if (op == "CALL") {
if (!arg2.empty())
cout << result << " = CALL " << arg1 << ", " << arg2 << endl;
else
cout << "CALL " << arg1 << endl;
} else if (op == "RETURN") {
if (!arg1.empty())
cout << "RETURN " << arg1 << endl;
else
cout << "RETURN" << endl;
} else if (op == "PRINT") {
cout << "PRINT " << arg1 << endl;
} else if (arg2.empty()) {
if (op == "=")
cout << result << " = " << arg1 << endl;
else
cout << result << " = " << op << " " << arg1 << endl;
} else {
cout << result << " = " << arg1 << " " << op << " " << arg2 << endl;
}
}
};

struct ASTNode {
string type;
string value;
DataType dataType;
vector<ASTNode*> children;
string tempVar;
int line;
int column;

ASTNode(const string& t, const string& v = "", DataType dt = UNKNOWN_TYPE, int ln = 0, int col = 0)
: type(t), value(v), dataType(dt), tempVar(""), line(ln), column(col) {}

void addChild(ASTNode* child) {
children.push_back(child);
}

void display(int depth = 0) {
string indent(depth * 2, ' ');
cout << indent << type;
if (!value.empty()) cout << " [" << value << "]";
if (dataType != UNKNOWN_TYPE) {
cout << " <";
switch(dataType) {
case INT_TYPE: cout << "int"; break;
case FLOAT_TYPE: cout << "float"; break;
case CHAR_TYPE: cout << "char"; break;
case VOID_TYPE: cout << "void"; break;
case BOOL_TYPE: cout << "bool"; break;
case STRING_TYPE: cout << "string"; break;
default: cout << "unknown";
}
cout << ">";
}
if (!tempVar.empty()) cout << " (temp: " << tempVar << ")";
if (line > 0) cout << " line:" << line;
cout << endl;

for (auto child : children) {
child->display(depth + 1);
}
}

~ASTNode() {
for (auto child : children) {
delete child;
}
}
};

class Lexer {
private:
string source;
vector<Token> tokens;
vector<CompilerError> errors;
int pos;
int line;
int column;

char peek() { return (pos < source.length()) ? source[pos] : '\0'; }
char peekNext() { return (pos + 1 < source.length()) ? source[pos + 1] : '\0'; }

char advance() {
char c = peek();
if (c == '\n') { line++; column = 1; }
else column++;
pos++;
return c;
}

void skipWhitespace() {
while (pos < source.length() && isspace(source[pos])) {
if (source[pos] == '\n') {
line++;
column = 1;
} else {
column++;
}
pos++;
}
}

void skipPreprocessor() {
int startLine = line;
int startCol = column;

advance(); // Skip '#'
skipWhitespace();

// Skip rest of preprocessor line
while (pos < source.length() && source[pos] != '\n') {
advance();
}

tokens.push_back(Token(TK_INCLUDE, "#", startLine, startCol));
}

void skipComment() {
if (match('/')) {
if (match('/')) {
// Single line comment
while (peek() != '\n' && peek() != '\0') advance();
if (peek() == '\n') advance();
} else if (match('*')) {
// Multi-line comment
while (true) {
if (peek() == '\0') {
errors.push_back(CompilerError(line, column,
"Unterminated comment", "Lexical", "ERROR"));
return;
}
if (peek() == '*' && peekNext() == '/') {
advance(); // skip *
advance(); // skip /
break;
}
advance();
}
} else {
pos--;
column--;
}
}
}

bool match(char expected) {
if (peek() == expected) {
advance();
return true;
}
return false;
}

Token identifierOrKeyword() {
int start = pos - 1;
while (isalnum(peek()) || peek() == '_') advance();
string text = source.substr(start, pos - start);

static unordered_map<string, TokenType> keywords = {
{"int", TK_INT}, {"float", TK_FLOAT}, {"char", TK_CHAR},
{"void", TK_VOID}, {"bool", TK_BOOL}, {"string", TK_STRING},
{"true", TK_TRUE}, {"false", TK_FALSE},
{"if", TK_IF}, {"else", TK_ELSE}, {"elseif", TK_ELSEIF},
{"while", TK_WHILE}, {"do", TK_DO}, {"for", TK_FOR},
{"switch", TK_SWITCH}, {"case", TK_CASE}, {"default", TK_DEFAULT},
{"return", TK_RETURN}, {"break", TK_BREAK}, {"continue", TK_CONTINUE},
{"printf", TK_PRINTF}, {"scanf", TK_SCANF}, {"print", TK_PRINT},
{"NULL", TK_NULL}, {"null", TK_NULL}
};

auto it = keywords.find(text);
if (it != keywords.end()) {
return Token(it->second, text, line, column - text.length());
}
return Token(TK_IDENTIFIER, text, line, column - text.length());
}

Token number() {
int start = pos - 1;
bool isFloat = false;
while (isdigit(peek()) || peek() == '.') {
if (peek() == '.') {
if (isFloat) break;
isFloat = true;
}
advance();
}
string text = source.substr(start, pos - start);
return Token(isFloat ? TK_FLOATNUM : TK_INTEGER, text, line, column - text.length());
}

Token stringLiteral() {
int start = pos;
while (peek() != '"' && peek() != '\0') {
if (match('\\')) {
if (peek() != '\0') advance();
} else {
advance();
}
}

if (!match('"')) {
errors.push_back(CompilerError(line, column,
"Unterminated string literal", "Lexical", "ERROR"));
}

string text = source.substr(start, pos - start - 1);
return Token(TK_STRINGLIT, text, line, column - text.length() - 2);
}

Token charLiteral() {
int start = pos;
if (match('\\')) {
if (peek() != '\0') advance();
} else {
advance();
}

if (!match('\'')) {
errors.push_back(CompilerError(line, column,
"Unterminated character literal", "Lexical", "ERROR"));
}

string text = source.substr(start, pos - start - 1);
return Token(TK_CHARLIT, text, line, column - text.length() - 2);
}

public:
Lexer(const string& src) : source(src), pos(0), line(1), column(1) {}

vector<Token> tokenize() {
tokens.clear();
errors.clear();

while (pos < source.length()) {
skipWhitespace();
if (pos >= source.length()) break;

// Skip preprocessor directives
if (source[pos] == '#') {
skipPreprocessor();
continue;
}

// Check for comments
if (source[pos] == '/' && pos + 1 < source.length()) {
if (source[pos+1] == '/') {
skipComment();
continue;
} else if (source[pos+1] == '*') {
skipComment();
continue;
}
}

int tokenLine = line;
int tokenCol = column;

if (isalpha(source[pos]) || source[pos] == '_') {
advance();
tokens.push_back(identifierOrKeyword());
continue;
}

if (isdigit(source[pos])) {
advance();
tokens.push_back(number());
continue;
}

if (source[pos] == '"') {
advance();
tokens.push_back(stringLiteral());
continue;
}

if (source[pos] == '\'') {
advance();
tokens.push_back(charLiteral());
continue;
}

switch (source[pos]) {
case '+':
advance();
if (peek() == '+') {
advance();
tokens.push_back(Token(TK_INCREMENT, "++", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_PLUS, "+", tokenLine, tokenCol));
}
break;

case '-':
advance();
if (peek() == '-') {
advance();
tokens.push_back(Token(TK_DECREMENT, "--", tokenLine, tokenCol));
} else if (peek() == '>') {
advance();
tokens.push_back(Token(TK_ARROW, "->", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_MINUS, "-", tokenLine, tokenCol));
}
break;

case '*': advance(); tokens.push_back(Token(TK_MULTIPLY, "*", tokenLine, tokenCol)); break;
case '/': advance(); tokens.push_back(Token(TK_DIVIDE, "/", tokenLine, tokenCol)); break;
case '%': advance(); tokens.push_back(Token(TK_MOD, "%", tokenLine, tokenCol)); break;
case '.': advance(); tokens.push_back(Token(TK_DOT, ".", tokenLine, tokenCol)); break;
case ':': advance(); tokens.push_back(Token(TK_COLON, ":", tokenLine, tokenCol)); break;
case '?': advance(); tokens.push_back(Token(TK_QUESTION, "?", tokenLine, tokenCol)); break;

case '=':
advance();
if (peek() == '=') {
advance();
tokens.push_back(Token(TK_EQUAL, "==", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_ASSIGN, "=", tokenLine, tokenCol));
}
break;

case '!':
advance();
if (peek() == '=') {
advance();
tokens.push_back(Token(TK_NOTEQUAL, "!=", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_NOT, "!", tokenLine, tokenCol));
}
break;

case '<':
advance();
if (peek() == '=') {
advance();
tokens.push_back(Token(TK_LESSEQ, "<=", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_LESS, "<", tokenLine, tokenCol));
}
break;

case '>':
advance();
if (peek() == '=') {
advance();
tokens.push_back(Token(TK_GREATEREQ, ">=", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_GREATER, ">", tokenLine, tokenCol));
}
break;

case '&':
advance();
if (peek() == '&') {
advance();
tokens.push_back(Token(TK_AND, "&&", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_AND, "&", tokenLine, tokenCol));
}
break;

case '|':
advance();
if (peek() == '|') {
advance();
tokens.push_back(Token(TK_OR, "||", tokenLine, tokenCol));
} else {
tokens.push_back(Token(TK_OR, "|", tokenLine, tokenCol));
}
break;

case '(': advance(); tokens.push_back(Token(TK_LPAREN, "(", tokenLine, tokenCol)); break;
case ')': advance(); tokens.push_back(Token(TK_RPAREN, ")", tokenLine, tokenCol)); break;
case '{': advance(); tokens.push_back(Token(TK_LBRACE, "{", tokenLine, tokenCol)); break;
case '}': advance(); tokens.push_back(Token(TK_RBRACE, "}", tokenLine, tokenCol)); break;
case '[': advance(); tokens.push_back(Token(TK_LBRACKET, "[", tokenLine, tokenCol)); break;
case ']': advance(); tokens.push_back(Token(TK_RBRACKET, "]", tokenLine, tokenCol)); break;
case ';': advance(); tokens.push_back(Token(TK_SEMICOLON, ";", tokenLine, tokenCol)); break;
case ',': advance(); tokens.push_back(Token(TK_COMMA, ",", tokenLine, tokenCol)); break;

default:
errors.push_back(CompilerError(tokenLine, tokenCol,
string("Invalid character '") + source[pos] + "'", "Lexical", "ERROR"));
advance();
break;
}
}

tokens.push_back(Token(TK_EOF, "EOF", line, column));
return tokens;
}

vector<CompilerError> getErrors() {
set<CompilerError> uniqueErrors(errors.begin(), errors.end());
errors.assign(uniqueErrors.begin(), uniqueErrors.end());
return errors;
}

void displayTokens() {
cout << CYAN << "\nTOKEN LIST:" << RESET << endl;
cout << "+----------+------------+------+--------+" << endl;
cout << "| Lexeme | Type | Line | Column |" << endl;
cout << "+----------+------------+------+--------+" << endl;

int count = 0;
for (size_t i = 0; i < tokens.size() && count < 50; i++) {
if (tokens[i].type == TK_EOF) continue;

cout << "| " << left << setw(8) <<
(tokens[i].lexeme.length() > 8 ? tokens[i].lexeme.substr(0,7)+"..." : tokens[i].lexeme)
<< " | " << setw(10) << tokens[i].typeToString()
<< " | " << setw(4) << tokens[i].line
<< " | " << setw(6) << tokens[i].column << " |" << endl;
count++;
}
if (tokens.size() > 50) {
cout << "| ... (showing first 50) ... |" << endl;
}
cout << "+----------+------------+------+--------+" << endl;
cout << "Total tokens: " << tokens.size() - 1 << endl;
}
};

class Parser {
private:
vector<Token> tokens;
SymbolTable& symTable;
int current;
ASTNode* astRoot;
vector<CompilerError> errors;
bool hasMainFunction;
bool hadError;
int tempCounter;
int labelCounter;
bool inFunction;
DataType currentReturnType;
bool hasReturnStatement;
bool inLoop;
int loopDepth;

Token peek() {
if (current >= tokens.size()) return tokens.back();
return tokens[current];
}
Token previous() {
if (current == 0) return tokens[0];
return tokens[current - 1];
}
bool isAtEnd() { return peek().type == TK_EOF; }

bool check(TokenType type) {
if (isAtEnd()) return false;
return peek().type == type;
}

Token advance() {
if (!isAtEnd()) current++;
return previous();
}

bool match(TokenType type) {
if (check(type)) {
advance();
return true;
}
return false;
}

Token consume(TokenType type, const string& message) {
if (check(type)) return advance();

if (!hadError) {
errors.push_back(CompilerError(peek().line, peek().column,
message + " (found '" + peek().lexeme + "' instead)", "Syntax", "ERROR"));
hadError = true;
}

return Token(type, "", peek().line, peek().column);
}

void synchronize() {
hadError = false;
advance();

while (!isAtEnd()) {
if (previous().type == TK_SEMICOLON) return;

switch (peek().type) {
case TK_INT: case TK_FLOAT: case TK_CHAR: case TK_VOID:
case TK_BOOL: case TK_STRING:
case TK_IF: case TK_WHILE: case TK_DO: case TK_FOR:
case TK_PRINTF: case TK_SCANF: case TK_PRINT:
case TK_RETURN: case TK_BREAK: case TK_CONTINUE:
case TK_LBRACE: case TK_RBRACE:
return;
}

advance();
}
}

DataType getDataType(Token token) {
switch(token.type) {
case TK_INT: return INT_TYPE;
case TK_FLOAT: return FLOAT_TYPE;
case TK_CHAR: return CHAR_TYPE;
case TK_VOID: return VOID_TYPE;
case TK_BOOL: return BOOL_TYPE;
case TK_STRING: return STRING_TYPE;
default: return UNKNOWN_TYPE;
}
}

bool isCompatibleType(DataType target, DataType source, bool assignment = false) {
if (target == UNKNOWN_TYPE || source == UNKNOWN_TYPE) return true;
if (target == source) return true;

// Type conversion rules
if (target == FLOAT_TYPE && source == INT_TYPE) return true;
if (target == INT_TYPE && source == BOOL_TYPE) return true;
if (target == BOOL_TYPE && source == INT_TYPE) return true;

// For assignment, be stricter
if (assignment) {
if (target == CHAR_TYPE && source == INT_TYPE) return true;
}

return false;
}

void typeCheck(ASTNode* node, DataType expected, const string& context, bool assignment = false, int line = -1, int col = -1) {
if (node->dataType == UNKNOWN_TYPE) return;

if (!isCompatibleType(expected, node->dataType, assignment)) {
int errLine = (line != -1) ? line : node->line;
int errCol = (col != -1) ? col : node->column;
errors.push_back(CompilerError(errLine, errCol,
"Type mismatch in " + context + ": expected " + dataTypeToString(expected) +
", got " + dataTypeToString(node->dataType), "Semantic", "ERROR"));
}
}

string dataTypeToString(DataType dt) {
switch(dt) {
case INT_TYPE: return "int";
case FLOAT_TYPE: return "float";
case CHAR_TYPE: return "char";
case VOID_TYPE: return "void";
case BOOL_TYPE: return "bool";
case STRING_TYPE: return "string";
default: return "unknown";
}
}

string newTemp() {
return "t" + to_string(tempCounter++);
}

string newLabel() {
return "L" + to_string(labelCounter++);
}

ASTNode* program() {
ASTNode* node = new ASTNode("Program");
astRoot = node;

while (!isAtEnd()) {
// Skip includes and preprocessor
if (match(TK_INCLUDE)) {
continue;
}

if (match(TK_INT) || match(TK_FLOAT) || match(TK_CHAR) ||
match(TK_VOID) || match(TK_BOOL) || match(TK_STRING)) {
Token typeToken = previous();

if (check(TK_IDENTIFIER)) {
Token nameToken = peek();

// Check if it's a function
if (current + 1 < tokens.size() && tokens[current + 1].type == TK_LPAREN) {
node->addChild(function(typeToken));
} else {
current--;
node->addChild(declaration());
}
} else {
errors.push_back(CompilerError(peek().line, peek().column,
"Expected identifier after type", "Syntax", "ERROR"));
synchronize();
}
} else if (check(TK_LBRACE)) {
node->addChild(block());
} else if (check(TK_IF) || check(TK_WHILE) || check(TK_DO) ||
check(TK_FOR) || check(TK_PRINTF) || check(TK_SCANF) ||
check(TK_PRINT) || check(TK_RETURN) || check(TK_BREAK) ||
check(TK_CONTINUE) || check(TK_SEMICOLON)) {
node->addChild(statement());
} else if (check(TK_IDENTIFIER)) {
if (current + 1 < tokens.size() && tokens[current + 1].type == TK_LPAREN) {
node->addChild(functionCallStatement());
} else {
node->addChild(statement());
}
} else {
errors.push_back(CompilerError(peek().line, peek().column,
"Unexpected token '" + peek().lexeme + "'", "Syntax", "ERROR"));
synchronize();
}
}

return node;
}

ASTNode* function(Token typeToken) {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");

if (nameToken.lexeme == "main") hasMainFunction = true;

Symbol funcSym(nameToken.lexeme, FUNC_SYM, getDataType(typeToken), nameToken.line, 0, false, false, false);
symTable.insert(nameToken.lexeme, funcSym);

ASTNode* node = new ASTNode("Function", nameToken.lexeme, getDataType(typeToken), nameToken.line, nameToken.column);

symTable.enterScope();
inFunction = true;
currentReturnType = getDataType(typeToken);
hasReturnStatement = false;
if (nameToken.lexeme == "main" && currentReturnType != INT_TYPE) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"main() function must return int", "Semantic", "ERROR"));
}

consume(TK_LPAREN, "Expected '(' after function name");

if (!check(TK_RPAREN)) {
do {
if (check(TK_INT) || check(TK_FLOAT) || check(TK_CHAR) ||
check(TK_BOOL) || check(TK_STRING)) {
Token paramType = advance();
Token paramName = consume(TK_IDENTIFIER, "Expected parameter name");
Symbol paramSym(paramName.lexeme, VAR_SYM, getDataType(paramType), paramName.line, 1, true, true);
symTable.insert(paramName.lexeme, paramSym);
node->addChild(new ASTNode("Param", paramName.lexeme, getDataType(paramType), paramName.line, paramName.column));
} else {
errors.push_back(CompilerError(peek().line, peek().column,
"Expected parameter type", "Syntax", "ERROR"));
synchronize();
}
} while (match(TK_COMMA));
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_LBRACE, "Expected '{'");

while (!check(TK_RBRACE) && !isAtEnd()) {
node->addChild(statement());
}

consume(TK_RBRACE, "Expected '}'");

// Check if non-void function has return statement
if (currentReturnType != VOID_TYPE && !hasReturnStatement) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"Function '" + nameToken.lexeme + "' must return a value", "Semantic", "ERROR"));
}

symTable.exitScope();
inFunction = false;
symTable.markFunctionDefined(nameToken.lexeme);

return node;
}

ASTNode* declaration() {
Token typeToken = advance();
DataType dtype = getDataType(typeToken);

Token nameToken = consume(TK_IDENTIFIER, "Expected variable name");

Symbol varSym(nameToken.lexeme, VAR_SYM, dtype, nameToken.line,
symTable.lookup("main") ? 1 : 0, false, false);

if (!symTable.insert(nameToken.lexeme, varSym)) {
return new ASTNode("Error", "", UNKNOWN_TYPE, nameToken.line, nameToken.column);
}

ASTNode* node = new ASTNode("Declaration", nameToken.lexeme, dtype, nameToken.line, nameToken.column);

if (match(TK_ASSIGN)) {
symTable.markInitialized(nameToken.lexeme);
ASTNode* expr = expression();
typeCheck(expr, dtype, "declaration initialization", true, nameToken.line, nameToken.column);
node->addChild(expr);
}

consume(TK_SEMICOLON, "Expected ';' after declaration");
return node;
}

ASTNode* statement() {
if (check(TK_IF)) return ifStatement();
else if (check(TK_WHILE)) return whileStatement();
else if (check(TK_DO)) return doWhileStatement();
else if (check(TK_FOR)) return forStatement();
else if (check(TK_PRINTF) || check(TK_SCANF) || check(TK_PRINT))
return ioStatement();
else if (check(TK_RETURN)) return returnStatement();
else if (check(TK_LBRACE)) return block();
else if (check(TK_INT) || check(TK_FLOAT) || check(TK_CHAR) ||
check(TK_BOOL) || check(TK_STRING)) return declaration();
else if (check(TK_BREAK) || check(TK_CONTINUE)) {
return breakContinueStatement();
} else if (check(TK_SEMICOLON)) {
advance();
return new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column);
} else if (check(TK_IDENTIFIER) && current + 1 < tokens.size() &&
tokens[current + 1].type == TK_LPAREN) {
return functionCallStatement();
} else {
return expressionStatement();
}
}

ASTNode* breakContinueStatement() {
if (!inLoop) {
errors.push_back(CompilerError(peek().line, peek().column,
"break/continue statement not within loop", "Semantic", "ERROR"));
}

Token token = advance();
ASTNode* node = new ASTNode(token.type == TK_BREAK ? "Break" : "Continue",
token.lexeme, UNKNOWN_TYPE, token.line, token.column);
consume(TK_SEMICOLON, "Expected ';'");
return node;
}

ASTNode* ifStatement() {
Token ifToken = consume(TK_IF, "Expected 'if'");
ASTNode* node = new ASTNode("If", "", UNKNOWN_TYPE, ifToken.line, ifToken.column);

consume(TK_LPAREN, "Expected '(' after 'if'");
ASTNode* cond = expression();
typeCheck(cond, BOOL_TYPE, "if condition", false, cond->line, cond->column);
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(cond->line, cond->column,
"Condition must be boolean expression", "Semantic", "ERROR"));
}
node->addChild(cond);
consume(TK_RPAREN, "Expected ')' after condition");

node->addChild(statement());

while (match(TK_ELSEIF)) {
Token elseifToken = previous();
consume(TK_LPAREN, "Expected '(' after 'elseif'");
cond = expression();
typeCheck(cond, BOOL_TYPE, "elseif condition", false, cond->line, cond->column);
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(cond->line, cond->column,
"Condition must be boolean expression", "Semantic", "ERROR"));
}
node->addChild(cond);
consume(TK_RPAREN, "Expected ')' after condition");
node->addChild(statement());
}

if (match(TK_ELSE)) {
node->addChild(statement());
}

return node;
}

ASTNode* whileStatement() {
Token whileToken = consume(TK_WHILE, "Expected 'while'");
ASTNode* node = new ASTNode("While", "", UNKNOWN_TYPE, whileToken.line, whileToken.column);

consume(TK_LPAREN, "Expected '(' after 'while'");
ASTNode* cond = expression();
typeCheck(cond, BOOL_TYPE, "while condition", false, cond->line, cond->column);
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(cond->line, cond->column,
"Condition must be boolean expression", "Semantic", "ERROR"));
}
node->addChild(cond);
consume(TK_RPAREN, "Expected ')' after condition");

loopDepth++;
inLoop = true;
node->addChild(statement());
loopDepth--;
if (loopDepth == 0) inLoop = false;

return node;
}

ASTNode* doWhileStatement() {
Token doToken = consume(TK_DO, "Expected 'do'");
ASTNode* node = new ASTNode("DoWhile", "", UNKNOWN_TYPE, doToken.line, doToken.column);

loopDepth++;
inLoop = true;
node->addChild(statement());
loopDepth--;
if (loopDepth == 0) inLoop = false;

consume(TK_WHILE, "Expected 'while' after do block");
consume(TK_LPAREN, "Expected '(' after 'while'");
ASTNode* cond = expression();
typeCheck(cond, BOOL_TYPE, "do-while condition", false, cond->line, cond->column);
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(cond->line, cond->column,
"Condition must be boolean expression", "Semantic", "ERROR"));
}
node->addChild(cond);
consume(TK_RPAREN, "Expected ')' after condition");
consume(TK_SEMICOLON, "Expected ';' after do-while");

return node;
}

ASTNode* forStatement() {
Token forToken = consume(TK_FOR, "Expected 'for'");
ASTNode* node = new ASTNode("For", "", UNKNOWN_TYPE, forToken.line, forToken.column);

consume(TK_LPAREN, "Expected '(' after 'for'");

symTable.enterScope();

if (match(TK_SEMICOLON)) {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column));
} else if (check(TK_INT) || check(TK_FLOAT) || check(TK_CHAR) ||
check(TK_BOOL) || check(TK_STRING)) {
node->addChild(declaration());
} else {
node->addChild(expression());
consume(TK_SEMICOLON, "Expected ';' after for initialization");
}

if (!check(TK_SEMICOLON)) {
ASTNode* cond = expression();
typeCheck(cond, BOOL_TYPE, "for loop condition", false, cond->line, cond->column);
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(cond->line, cond->column,
"Condition must be boolean expression", "Semantic", "ERROR"));
}
node->addChild(cond);
} else {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column));
}
consume(TK_SEMICOLON, "Expected ';' after condition");

if (!check(TK_RPAREN)) {
node->addChild(expression());
} else {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column));
}
consume(TK_RPAREN, "Expected ')' after for clauses");

loopDepth++;
inLoop = true;
node->addChild(statement());
loopDepth--;
if (loopDepth == 0) inLoop = false;

symTable.exitScope();
return node;
}

ASTNode* ioStatement() {
bool isPrintf = match(TK_PRINTF);
bool isPrint = match(TK_PRINT);
bool isScanf = false;
if (!isPrintf && !isPrint) {
isScanf = match(TK_SCANF);
}

Token ioToken = previous();
ASTNode* node = new ASTNode("Print", ioToken.lexeme, UNKNOWN_TYPE, ioToken.line, ioToken.column);

consume(TK_LPAREN, "Expected '(' after '" + ioToken.lexeme + "'");

if (check(TK_STRINGLIT)) {
Token formatToken = advance();
node->addChild(new ASTNode("String", formatToken.lexeme, STRING_TYPE, formatToken.line, formatToken.column));
} else if ((isPrintf || isPrint) && check(TK_IDENTIFIER)) {
Token token = advance();
node->addChild(new ASTNode("Identifier", token.lexeme, UNKNOWN_TYPE, token.line, token.column));
} else if (isPrint) {
// For print(), accept any expression
node->addChild(expression());
} else {
errors.push_back(CompilerError(peek().line, peek().column,
"Expected format string", "Syntax", "ERROR"));
synchronize();
return node;
}

if (isPrintf || isScanf) {
while (match(TK_COMMA)) {
if (isAtEnd() || check(TK_RPAREN)) break;
node->addChild(expression());
}
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_SEMICOLON, "Expected ';' after statement");

return node;
}

ASTNode* functionCallStatement() {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");
ASTNode* node = new ASTNode("FunctionCall", nameToken.lexeme, UNKNOWN_TYPE, nameToken.line, nameToken.column);

symTable.markUsed(nameToken.lexeme);

consume(TK_LPAREN, "Expected '(' after function name");

int paramCount = 0;
if (!check(TK_RPAREN)) {
do {
node->addChild(expression());
paramCount++;
} while (match(TK_COMMA));
}

// Check if function exists
Symbol* sym = symTable.lookup(nameToken.lexeme);
if (!sym) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"Undeclared function '" + nameToken.lexeme + "'", "Semantic", "ERROR"));
} else if (sym->symType != FUNC_SYM) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"'" + nameToken.lexeme + "' is not a function", "Semantic", "ERROR"));
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_SEMICOLON, "Expected ';' after function call");

return node;
}

ASTNode* returnStatement() {
Token returnToken = consume(TK_RETURN, "Expected 'return'");
ASTNode* node = new ASTNode("Return", "", currentReturnType, returnToken.line, returnToken.column);

hasReturnStatement = true;

if (!check(TK_SEMICOLON)) {
ASTNode* expr = expression();
node->addChild(expr);
if (currentReturnType != VOID_TYPE) {
typeCheck(expr, currentReturnType, "return statement", false, returnToken.line, returnToken.column);
} else {
errors.push_back(CompilerError(returnToken.line, returnToken.column,
"Void function cannot return a value", "Semantic", "ERROR"));
}
} else if (currentReturnType != VOID_TYPE) {
errors.push_back(CompilerError(returnToken.line, returnToken.column,
"Function must return a value", "Semantic", "ERROR"));
}

consume(TK_SEMICOLON, "Expected ';' after return");
return node;
}

ASTNode* block() {
Token braceToken = consume(TK_LBRACE, "Expected '{'");

symTable.enterScope();
ASTNode* node = new ASTNode("Block", "", UNKNOWN_TYPE, braceToken.line, braceToken.column);

while (!check(TK_RBRACE) && !isAtEnd()) {
node->addChild(statement());
}

consume(TK_RBRACE, "Expected '}'");
symTable.exitScope();

return node;
}

ASTNode* expressionStatement() {
ASTNode* expr = expression();
if (expr->type == "Identifier") {
symTable.markUsed(expr->value);
}
consume(TK_SEMICOLON, "Expected ';' after expression");
return expr;
}

ASTNode* expression() {
return assignment();
}

ASTNode* assignment() {
ASTNode* expr = conditional();

if (match(TK_ASSIGN)) {
Token assignToken = previous();
ASTNode* assignNode = new ASTNode("Assignment", "=", expr->dataType, assignToken.line, assignToken.column);
assignNode->addChild(expr);
ASTNode* right = assignment();
assignNode->addChild(right);

if (expr->type == "Identifier") {
string varName = expr->value;
Symbol* sym = symTable.lookup(varName);
if (!sym) {
errors.push_back(CompilerError(expr->line, expr->column,
"Undeclared variable '" + varName + "'", "Semantic", "ERROR"));
} else {
symTable.markInitialized(varName);
symTable.markUsed(varName);
// Type checking for assignment
typeCheck(right, sym->dataType, "assignment to '" + varName + "'", true, assignToken.line, assignToken.column);
}
} else if (expr->type == "Literal") {
errors.push_back(CompilerError(expr->line, expr->column,
"Cannot assign to literal", "Semantic", "ERROR"));
}

assignNode->dataType = expr->dataType;
assignNode->tempVar = newTemp();
return assignNode;
}

return expr;
}

ASTNode* conditional() {
ASTNode* expr = logicalOr();

if (match(TK_QUESTION)) {
Token questionToken = previous();
ASTNode* node = new ASTNode("Conditional", "?:", UNKNOWN_TYPE, questionToken.line, questionToken.column);
typeCheck(expr, BOOL_TYPE, "conditional operator", false, expr->line, expr->column);
node->addChild(expr);
ASTNode* trueExpr = expression();
node->addChild(trueExpr);
consume(TK_COLON, "Expected ':' after expression");
ASTNode* falseExpr = conditional();
node->addChild(falseExpr);

// Type checking: true and false expressions should be compatible
if (!isCompatibleType(trueExpr->dataType, falseExpr->dataType)) {
errors.push_back(CompilerError(questionToken.line, questionToken.column,
"Type mismatch in conditional operator: " + dataTypeToString(trueExpr->dataType) +
" vs " + dataTypeToString(falseExpr->dataType), "Semantic", "ERROR"));
}

node->dataType = trueExpr->dataType;
node->tempVar = newTemp();
expr = node;
}

return expr;
}

ASTNode* logicalOr() {
ASTNode* expr = logicalAnd();

while (match(TK_OR)) {
Token orToken = previous();
ASTNode* opNode = new ASTNode("Operator", "||", BOOL_TYPE, orToken.line, orToken.column);
opNode->addChild(expr);
ASTNode* right = logicalAnd();
opNode->addChild(right);

// Both operands should be boolean
typeCheck(expr, BOOL_TYPE, "logical OR", false, expr->line, expr->column);
typeCheck(right, BOOL_TYPE, "logical OR", false, right->line, right->column);
opNode->dataType = BOOL_TYPE;
opNode->tempVar = newTemp();

expr = opNode;
}

return expr;
}

ASTNode* logicalAnd() {
ASTNode* expr = equality();

while (match(TK_AND)) {
Token andToken = previous();
ASTNode* opNode = new ASTNode("Operator", "&&", BOOL_TYPE, andToken.line, andToken.column);
opNode->addChild(expr);
ASTNode* right = equality();
opNode->addChild(right);

// Both operands should be boolean
typeCheck(expr, BOOL_TYPE, "logical AND", false, expr->line, expr->column);
typeCheck(right, BOOL_TYPE, "logical AND", false, right->line, right->column);
opNode->dataType = BOOL_TYPE;
opNode->tempVar = newTemp();

expr = opNode;
}

return expr;
}

ASTNode* equality() {
ASTNode* expr = comparison();

while (match(TK_EQUAL) || match(TK_NOTEQUAL)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* opNode = new ASTNode("Operator", op, BOOL_TYPE, opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = comparison();
opNode->addChild(right);

// Operands should be compatible types
if (!isCompatibleType(expr->dataType, right->dataType)) {
errors.push_back(CompilerError(opToken.line, opToken.column,
"Type mismatch in equality operator: " + dataTypeToString(expr->dataType) +
" vs " + dataTypeToString(right->dataType), "Semantic", "ERROR"));
}

opNode->dataType = BOOL_TYPE;
opNode->tempVar = newTemp();
expr = opNode;
}

return expr;
}

ASTNode* comparison() {
ASTNode* expr = term();

while (match(TK_LESS) || match(TK_GREATER) ||
match(TK_LESSEQ) || match(TK_GREATEREQ)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* opNode = new ASTNode("Operator", op, BOOL_TYPE, opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = term();
opNode->addChild(right);

// Both operands should be numeric or comparable
if (expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != CHAR_TYPE && expr->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(expr->line, expr->column,
"Comparison operator requires numeric or character operands", "Semantic", "ERROR"));
}
if (right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != CHAR_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(right->line, right->column,
"Comparison operator requires numeric or character operands", "Semantic", "ERROR"));
}

opNode->dataType = BOOL_TYPE;
opNode->tempVar = newTemp();
expr = opNode;
}

return expr;
}

ASTNode* term() {
ASTNode* expr = factor();

while (match(TK_PLUS) || match(TK_MINUS)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* opNode = new ASTNode("Operator", op, UNKNOWN_TYPE, opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = factor();
opNode->addChild(right);

// Both operands should be numeric
if (expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != CHAR_TYPE && expr->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(expr->line, expr->column,
"Arithmetic operator requires numeric operands", "Semantic", "ERROR"));
}
if (right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != CHAR_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(right->line, right->column,
"Arithmetic operator requires numeric operands", "Semantic", "ERROR"));
}

// Result type: if either is float, result is float
if (expr->dataType == FLOAT_TYPE || right->dataType == FLOAT_TYPE) {
opNode->dataType = FLOAT_TYPE;
} else if (expr->dataType == INT_TYPE || right->dataType == INT_TYPE) {
opNode->dataType = INT_TYPE;
} else if (expr->dataType == CHAR_TYPE || right->dataType == CHAR_TYPE) {
opNode->dataType = INT_TYPE; // char promotes to int
}

opNode->tempVar = newTemp();
expr = opNode;
}

return expr;
}

ASTNode* factor() {
ASTNode* expr = unary();

while (match(TK_MULTIPLY) || match(TK_DIVIDE) || match(TK_MOD)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* opNode = new ASTNode("Operator", op, UNKNOWN_TYPE, opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = unary();
opNode->addChild(right);

// Both operands should be numeric
if (expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(expr->line, expr->column,
"Arithmetic operator requires numeric operands", "Semantic", "ERROR"));
}
if (right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(right->line, right->column,
"Arithmetic operator requires numeric operands", "Semantic", "ERROR"));
}

// MOD only works with integers
if (op == "%" && (expr->dataType != INT_TYPE || right->dataType != INT_TYPE) &&
expr->dataType != UNKNOWN_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(opToken.line, opToken.column,
"MOD operator requires integer operands", "Semantic", "ERROR"));
}

// Result type: if either is float, result is float
if (expr->dataType == FLOAT_TYPE || right->dataType == FLOAT_TYPE) {
opNode->dataType = FLOAT_TYPE;
} else if (expr->dataType == INT_TYPE || right->dataType == INT_TYPE) {
opNode->dataType = INT_TYPE;
}

opNode->tempVar = newTemp();
expr = opNode;
}

return expr;
}

ASTNode* unary() {
if (match(TK_MINUS) || match(TK_NOT) || match(TK_INCREMENT) || match(TK_DECREMENT)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* node = new ASTNode("Unary", op, UNKNOWN_TYPE, opToken.line, opToken.column);
ASTNode* operand = unary();
node->addChild(operand);

if (op == "-") {
// Should be numeric
if (operand->dataType != INT_TYPE && operand->dataType != FLOAT_TYPE &&
operand->dataType != CHAR_TYPE && operand->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(operand->line, operand->column,
"Unary minus requires numeric operand", "Semantic", "ERROR"));
}
node->dataType = operand->dataType;
} else if (op == "!") {
// Should be boolean
typeCheck(operand, BOOL_TYPE, "logical NOT", false, operand->line, operand->column);
node->dataType = BOOL_TYPE;
} else { // ++ or --
// Should be numeric and lvalue
if (operand->type != "Identifier") {
errors.push_back(CompilerError(operand->line, operand->column,
"Increment/decrement requires lvalue", "Semantic", "ERROR"));
}
if (operand->dataType != INT_TYPE && operand->dataType != FLOAT_TYPE &&
operand->dataType != CHAR_TYPE && operand->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(operand->line, operand->column,
"Increment/decrement requires numeric operand", "Semantic", "ERROR"));
}
node->dataType = operand->dataType;
if (operand->type == "Identifier") {
symTable.markUsed(operand->value);
symTable.markInitialized(operand->value);
}
}

node->tempVar = newTemp();
return node;
}

return postfix();
}

ASTNode* postfix() {
ASTNode* expr = primary();

while (match(TK_INCREMENT) || match(TK_DECREMENT)) {
Token opToken = previous();
string op = opToken.lexeme;
ASTNode* node = new ASTNode("Postfix", op, UNKNOWN_TYPE, opToken.line, opToken.column);
node->addChild(expr);

// Should be numeric and lvalue
if (expr->type != "Identifier") {
errors.push_back(CompilerError(expr->line, expr->column,
"Increment/decrement requires lvalue", "Semantic", "ERROR"));
}
if (expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != CHAR_TYPE && expr->dataType != UNKNOWN_TYPE) {
errors.push_back(CompilerError(expr->line, expr->column,
"Increment/decrement requires numeric operand", "Semantic", "ERROR"));
}

node->dataType = expr->dataType;
node->tempVar = expr->value; // Original value before increment
if (expr->type == "Identifier") {
symTable.markUsed(expr->value);
symTable.markInitialized(expr->value);
}
expr = node;
}

return expr;
}

ASTNode* primary() {
if (match(TK_INTEGER)) {
Token token = previous();
return new ASTNode("Literal", token.lexeme, INT_TYPE, token.line, token.column);
} else if (match(TK_FLOATNUM)) {
Token token = previous();
return new ASTNode("Literal", token.lexeme, FLOAT_TYPE, token.line, token.column);
} else if (match(TK_STRINGLIT)) {
Token token = previous();
return new ASTNode("String", token.lexeme, STRING_TYPE, token.line, token.column);
} else if (match(TK_CHARLIT)) {
Token token = previous();
return new ASTNode("Literal", token.lexeme, CHAR_TYPE, token.line, token.column);
} else if (match(TK_TRUE) || match(TK_FALSE) || match(TK_NULL)) {
Token token = previous();
DataType dt = (token.type == TK_NULL) ? VOID_TYPE : BOOL_TYPE;
return new ASTNode("Literal", token.lexeme, dt, token.line, token.column);
} else if (match(TK_IDENTIFIER)) {
Token token = previous();
string varName = token.lexeme;
Symbol* sym = symTable.lookup(varName);

if (check(TK_LPAREN)) {
current--;
return functionCall();
}

if (!sym) {
errors.push_back(CompilerError(token.line, token.column,
"Undeclared variable '" + varName + "'", "Semantic", "ERROR"));
} else {
symTable.markUsed(varName);
}

ASTNode* node = new ASTNode("Identifier", varName,
sym ? sym->dataType : UNKNOWN_TYPE, token.line, token.column);
node->tempVar = varName;
return node;
} else if (match(TK_LPAREN)) {
Token token = previous();
ASTNode* expr = expression();
consume(TK_RPAREN, "Expected ')' after expression");
expr->tempVar = newTemp();
expr->line = token.line;
expr->column = token.column;
return expr;
} else {
if (!hadError) {
errors.push_back(CompilerError(peek().line, peek().column,
"Expected expression", "Syntax", "ERROR"));
hadError = true;
}
synchronize();
return new ASTNode("Error", peek().lexeme, UNKNOWN_TYPE, peek().line, peek().column);
}
}

ASTNode* functionCall() {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");
ASTNode* node = new ASTNode("FunctionCall", nameToken.lexeme, UNKNOWN_TYPE, nameToken.line, nameToken.column);

symTable.markUsed(nameToken.lexeme);

Symbol* sym = symTable.lookup(nameToken.lexeme);
if (!sym) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"Undeclared function '" + nameToken.lexeme + "'", "Semantic", "ERROR"));
} else if (sym->symType != FUNC_SYM) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"'" + nameToken.lexeme + "' is not a function", "Semantic", "ERROR"));
} else {
node->dataType = sym->dataType;
}

consume(TK_LPAREN, "Expected '(' after function name");

int paramCount = 0;
if (!check(TK_RPAREN)) {
do {
node->addChild(expression());
paramCount++;
} while (match(TK_COMMA));
}

// Check parameter count (simplified - would need to match param types too)
if (sym && sym->paramTypes.size() != paramCount) {
errors.push_back(CompilerError(nameToken.line, nameToken.column,
"Function '" + nameToken.lexeme + "' expects " +
to_string(sym->paramTypes.size()) + " arguments, got " +
to_string(paramCount), "Semantic", "ERROR"));
}

consume(TK_RPAREN, "Expected ')'");
node->tempVar = newTemp();
return node;
}

public:
Parser(vector<Token> t, SymbolTable& st)
: tokens(t), symTable(st), current(0), astRoot(nullptr),
hasMainFunction(false), hadError(false), tempCounter(0), labelCounter(0),
inFunction(false), currentReturnType(VOID_TYPE), hasReturnStatement(false),
inLoop(false), loopDepth(0) {}

ASTNode* parse() {
astRoot = program();

// Add built-in functions
symTable.insert("printf", Symbol("printf", FUNC_SYM, INT_TYPE, 0, 0, false, true, true));
symTable.insert("scanf", Symbol("scanf", FUNC_SYM, INT_TYPE, 0, 0, false, true, true));
symTable.insert("print", Symbol("print", FUNC_SYM, VOID_TYPE, 0, 0, false, true, true));

if (!hasMainFunction && astRoot && astRoot->children.size() > 0) {
bool hasRealContent = false;
for (auto child : astRoot->children) {
if (child->type == "Function" ||
(child->type == "Block" && child->children.size() > 0) ||
child->type != "Block") {
hasRealContent = true;
break;
}
}

if (hasRealContent) {
errors.push_back(CompilerError(1, 1,
"No main() function found - treating as script", "Semantic", "WARNING"));
}
}

// Check for unused variables and functions
symTable.checkUninitialized();

return astRoot;
}

vector<CompilerError> getErrors() {
auto symErrors = symTable.getErrors();
errors.insert(errors.end(), symErrors.begin(), symErrors.end());

// Remove duplicate errors
set<CompilerError> uniqueErrors(errors.begin(), errors.end());
errors.assign(uniqueErrors.begin(), uniqueErrors.end());

return errors;
}

void displayAST() {
if (astRoot) {
cout << CYAN << "\nABSTRACT SYNTAX TREE:" << RESET << endl;
cout << "======================" << endl;
astRoot->display();
}
}

vector<TACNode> generateTAC() {
vector<TACNode> tacList;
if (astRoot) {
generateTACRecursive(astRoot, tacList);
}
return tacList;
}

private:
void generateTACRecursive(ASTNode* node, vector<TACNode>& tacList) {
if (!node) return;

if (node->type == "Program") {
for (auto child : node->children) {
generateTACRecursive(child, tacList);
}
}
else if (node->type == "Function") {
tacList.push_back(TACNode("LABEL", "", "", node->value, node->line));
for (auto child : node->children) {
generateTACRecursive(child, tacList);
}
if (node->value == "main" && node->dataType == INT_TYPE) {
tacList.push_back(TACNode("RETURN", "0", "", "", node->line));
} else if (node->dataType != VOID_TYPE) {
tacList.push_back(TACNode("RETURN", "0", "", "", node->line));
}
}
else if (node->type == "Declaration") {
if (!node->children.empty()) {
generateTACRecursive(node->children[0], tacList);
string arg1 = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
tacList.push_back(TACNode("=", arg1, "", node->value, node->line));
}
}
else if (node->type == "Assignment") {
generateTACRecursive(node->children[1], tacList);
string arg1 = node->children[1]->tempVar.empty() ? node->children[1]->value : node->children[1]->tempVar;
string result = node->children[0]->value;
tacList.push_back(TACNode("=", arg1, "", result, node->line));
}
else if (node->type == "Operator") {
generateTACRecursive(node->children[0], tacList);
generateTACRecursive(node->children[1], tacList);

string arg1 = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
string arg2 = node->children[1]->tempVar.empty() ? node->children[1]->value : node->children[1]->tempVar;
string result = node->tempVar;

if (node->value == "&&") tacList.push_back(TACNode("AND", arg1, arg2, result, node->line));
else if (node->value == "||") tacList.push_back(TACNode("OR", arg1, arg2, result, node->line));
else if (node->value == "<") tacList.push_back(TACNode("<", arg1, arg2, result, node->line));
else if (node->value == ">") tacList.push_back(TACNode(">", arg1, arg2, result, node->line));
else if (node->value == "<=") tacList.push_back(TACNode("<=", arg1, arg2, result, node->line));
else if (node->value == ">=") tacList.push_back(TACNode(">=", arg1, arg2, result, node->line));
else if (node->value == "==") tacList.push_back(TACNode("==", arg1, arg2, result, node->line));
else if (node->value == "!=") tacList.push_back(TACNode("!=", arg1, arg2, result, node->line));
else if (node->value == "+") tacList.push_back(TACNode("+", arg1, arg2, result, node->line));
else if (node->value == "-") tacList.push_back(TACNode("-", arg1, arg2, result, node->line));
else if (node->value == "*") tacList.push_back(TACNode("*", arg1, arg2, result, node->line));
else if (node->value == "/") tacList.push_back(TACNode("/", arg1, arg2, result, node->line));
else if (node->value == "%") tacList.push_back(TACNode("%", arg1, arg2, result, node->line));
}
else if (node->type == "Unary") {
generateTACRecursive(node->children[0], tacList);
string arg1 = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
string result = node->tempVar;

if (node->value == "-") tacList.push_back(TACNode("NEG", arg1, "", result, node->line));
else if (node->value == "!") tacList.push_back(TACNode("NOT", arg1, "", result, node->line));
else if (node->value == "++") {
tacList.push_back(TACNode("+", arg1, "1", result, node->line));
tacList.push_back(TACNode("=", result, "", arg1, node->line));
}
else if (node->value == "--") {
tacList.push_back(TACNode("-", arg1, "1", result, node->line));
tacList.push_back(TACNode("=", result, "", arg1, node->line));
}
}
else if (node->type == "Postfix") {
string arg1 = node->children[0]->value;
string temp = node->tempVar;

if (node->value == "++") {
tacList.push_back(TACNode("=", arg1, "", temp, node->line));
tacList.push_back(TACNode("+", arg1, "1", arg1, node->line));
}
else if (node->value == "--") {
tacList.push_back(TACNode("=", arg1, "", temp, node->line));
tacList.push_back(TACNode("-", arg1, "1", arg1, node->line));
}
}
else if (node->type == "Conditional") {
generateTACRecursive(node->children[0], tacList);
string cond = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
string labelTrue = newLabel();
string labelFalse = newLabel();
string labelEnd = newLabel();

tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", labelTrue, node->line));
tacList.push_back(TACNode("GOTO", "", "", labelFalse, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelTrue, node->line));

generateTACRecursive(node->children[1], tacList);
string trueVal = node->children[1]->tempVar.empty() ? node->children[1]->value : node->children[1]->tempVar;
tacList.push_back(TACNode("=", trueVal, "", node->tempVar, node->line));
tacList.push_back(TACNode("GOTO", "", "", labelEnd, node->line));

tacList.push_back(TACNode("LABEL", "", "", labelFalse, node->line));
generateTACRecursive(node->children[2], tacList);
string falseVal = node->children[2]->tempVar.empty() ? node->children[2]->value : node->children[2]->tempVar;
tacList.push_back(TACNode("=", falseVal, "", node->tempVar, node->line));

tacList.push_back(TACNode("LABEL", "", "", labelEnd, node->line));
}
else if (node->type == "FunctionCall") {
for (auto child : node->children) {
generateTACRecursive(child, tacList);
string arg = child->tempVar.empty() ? child->value : child->tempVar;
tacList.push_back(TACNode("PARAM", arg, "", "", node->line));
}
tacList.push_back(TACNode("CALL", node->value, to_string(node->children.size()), node->tempVar, node->line));
}
else if (node->type == "Print") {
for (auto child : node->children) {
generateTACRecursive(child, tacList);
string arg = child->tempVar.empty() ? child->value : child->tempVar;
tacList.push_back(TACNode("PRINT", arg, "", "", node->line));
}
}
else if (node->type == "If") {
generateTACRecursive(node->children[0], tacList);
string cond = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
string labelTrue = newLabel();
string labelFalse = newLabel();
string labelEnd = newLabel();

tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", labelTrue, node->line));
tacList.push_back(TACNode("GOTO", "", "", labelFalse, node->line));

tacList.push_back(TACNode("LABEL", "", "", labelTrue, node->line));
generateTACRecursive(node->children[1], tacList);

// Handle else if and else
for (size_t i = 2; i < node->children.size(); i += 2) {
if (i < node->children.size() - 1) {
tacList.push_back(TACNode("GOTO", "", "", labelEnd, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelFalse, node->line));
generateTACRecursive(node->children[i], tacList);
cond = node->children[i]->tempVar.empty() ? node->children[i]->value : node->children[i]->tempVar;
labelFalse = newLabel();
tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", labelTrue, node->line));
tacList.push_back(TACNode("GOTO", "", "", labelFalse, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelTrue, node->line));
generateTACRecursive(node->children[i + 1], tacList);
} else {
// Else clause
tacList.push_back(TACNode("GOTO", "", "", labelEnd, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelFalse, node->line));
generateTACRecursive(node->children[i], tacList);
}
}

tacList.push_back(TACNode("GOTO", "", "", labelEnd, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelFalse, node->line));
tacList.push_back(TACNode("LABEL", "", "", labelEnd, node->line));
}
else if (node->type == "While") {
string startLabel = newLabel();
string bodyLabel = newLabel();
string endLabel = newLabel();

tacList.push_back(TACNode("LABEL", "", "", startLabel, node->line));
generateTACRecursive(node->children[0], tacList);
string cond = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", bodyLabel, node->line));
tacList.push_back(TACNode("GOTO", "", "", endLabel, node->line));

tacList.push_back(TACNode("LABEL", "", "", bodyLabel, node->line));
generateTACRecursive(node->children[1], tacList);
tacList.push_back(TACNode("GOTO", "", "", startLabel, node->line));

tacList.push_back(TACNode("LABEL", "", "", endLabel, node->line));
}
else if (node->type == "DoWhile") {
string startLabel = newLabel();
string endLabel = newLabel();

tacList.push_back(TACNode("LABEL", "", "", startLabel, node->line));
generateTACRecursive(node->children[0], tacList);
generateTACRecursive(node->children[1], tacList);
string cond = node->children[1]->tempVar.empty() ? node->children[1]->value : node->children[1]->tempVar;
tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", startLabel, node->line));

tacList.push_back(TACNode("LABEL", "", "", endLabel, node->line));
}
else if (node->type == "For") {
symTable.enterScope();

string startLabel = newLabel();
string bodyLabel = newLabel();
string updateLabel = newLabel();
string endLabel = newLabel();

// Initialization
if (node->children[0]->type != "Empty") {
generateTACRecursive(node->children[0], tacList);
}

tacList.push_back(TACNode("LABEL", "", "", startLabel, node->line));

// Condition
if (node->children[1]->type != "Empty") {
generateTACRecursive(node->children[1], tacList);
string cond = node->children[1]->tempVar.empty() ? node->children[1]->value : node->children[1]->tempVar;
tacList.push_back(TACNode("IF", cond, "!=", "0", node->line));
tacList.push_back(TACNode("GOTO", "", "", bodyLabel, node->line));
tacList.push_back(TACNode("GOTO", "", "", endLabel, node->line));
} else {
tacList.push_back(TACNode("GOTO", "", "", bodyLabel, node->line));
}

// Body
tacList.push_back(TACNode("LABEL", "", "", bodyLabel, node->line));
generateTACRecursive(node->children[3], tacList);

// Update
tacList.push_back(TACNode("LABEL", "", "", updateLabel, node->line));
if (node->children[2]->type != "Empty") {
generateTACRecursive(node->children[2], tacList);
}
tacList.push_back(TACNode("GOTO", "", "", startLabel, node->line));

tacList.push_back(TACNode("LABEL", "", "", endLabel, node->line));

symTable.exitScope();
}
else if (node->type == "Return") {
if (!node->children.empty()) {
generateTACRecursive(node->children[0], tacList);
string arg1 = node->children[0]->tempVar.empty() ? node->children[0]->value : node->children[0]->tempVar;
tacList.push_back(TACNode("RETURN", arg1, "", "", node->line));
} else {
tacList.push_back(TACNode("RETURN", "", "", "", node->line));
}
}
else if (node->type == "Block") {
symTable.enterScope();
for (auto child : node->children) {
generateTACRecursive(child, tacList);
}
symTable.exitScope();
}
else if (node->type == "Break") {
// In a real compiler, would need to track loop exit labels
tacList.push_back(TACNode("GOTO", "", "", "LOOP_EXIT", node->line));
}
else if (node->type == "Continue") {
// In a real compiler, would need to track loop update labels
tacList.push_back(TACNode("GOTO", "", "", "LOOP_UPDATE", node->line));
}
}
};

class CodeGenerator {
private:
vector<CompilerError> errors;
int labelCount;
int tempCount;
ofstream outputFile;
string outputFilename;

public:
CodeGenerator(const string& filename) : labelCount(0), tempCount(0) {
outputFilename = filename.substr(0, filename.find_last_of('.')) + ".cpp";
outputFile.open(outputFilename);
}

~CodeGenerator() {
if (outputFile.is_open()) outputFile.close();
}

string newLabel() { return "L" + to_string(labelCount++); }
string newTemp() { return "t" + to_string(tempCount++); }

void generateFromAST(ASTNode* node) {
if (!outputFile.is_open()) {
errors.push_back(CompilerError(0, 0, "Cannot open output file", "CodeGen", "ERROR"));
return;
}

outputFile << "#include <iostream>\n";
outputFile << "#include <string>\n";
outputFile << "#include <cstdio>\n";
outputFile << "#include <vector>\n";
outputFile << "using namespace std;\n\n";

generateProgram(node);
}

void generateProgram(ASTNode* node) {
if (node->type == "Program") {
bool hasMain = false;
for (auto child : node->children) {
if (child->type == "Function" && child->value == "main") {
hasMain = true;
break;
}
}

if (!hasMain) {
outputFile << "int main() {\n";
for (auto child : node->children) {
generateStatement(child, 1);
}
outputFile << " return 0;\n";
outputFile << "}\n";
} else {
for (auto child : node->children) {
if (child->type == "Function") {
generateFunction(child);
} else {
// Global declarations
generateStatement(child, 0);
}
}
}
}
}

void generateFunction(ASTNode* node) {
string returnType = getTypeString(node->dataType);
outputFile << returnType << " " << node->value << "(";

// Parameters
bool firstParam = true;
for (auto child : node->children) {
if (child->type == "Param") {
if (!firstParam) outputFile << ", ";
outputFile << getTypeString(child->dataType) << " " << child->value;
firstParam = false;
}
}

outputFile << ") {\n";

// Function body
for (auto child : node->children) {
if (child->type != "Param") {
generateStatement(child, 1);
}
}

if (node->value == "main") {
outputFile << " return 0;\n";
} else if (node->dataType != VOID_TYPE) {
outputFile << " return 0; // Default return\n";
}

outputFile << "}\n\n";
}

void generateStatement(ASTNode* node, int indentLevel) {
string indent(indentLevel * 4, ' ');

if (node->type == "Declaration") {
outputFile << indent << getTypeString(node->dataType) << " " << node->value;
if (!node->children.empty()) {
outputFile << " = ";
generateExpression(node->children[0], outputFile);
}
outputFile << ";\n";
}
else if (node->type == "Assignment") {
outputFile << indent;
generateExpression(node->children[0], outputFile);
outputFile << " = ";
generateExpression(node->children[1], outputFile);
outputFile << ";\n";
}
else if (node->type == "Print") {
outputFile << indent;
if (node->value == "printf") {
outputFile << "printf(";
} else if (node->value == "scanf") {
outputFile << "scanf(";
} else {
outputFile << "cout << ";
}

if (!node->children.empty()) {
bool first = true;
for (auto child : node->children) {
if (!first) outputFile << " << ";
if (child->type == "String") {
outputFile << "\"" << child->value << "\"";
} else {
generateExpression(child, outputFile);
}
first = false;
}
}

if (node->value == "printf" || node->value == "scanf") {
outputFile << ")";
}
outputFile << ";\n";
}
else if (node->type == "If") {
outputFile << indent << "if (";
generateExpression(node->children[0], outputFile);
outputFile << ") {\n";
generateStatement(node->children[1], indentLevel + 1);
outputFile << indent << "}\n";

// Handle else if and else
for (size_t i = 2; i < node->children.size(); i++) {
if (i + 1 < node->children.size()) {
outputFile << indent << "else if (";
generateExpression(node->children[i], outputFile);
outputFile << ") {\n";
generateStatement(node->children[i + 1], indentLevel + 1);
outputFile << indent << "}\n";
i++;
} else {
outputFile << indent << "else {\n";
generateStatement(node->children[i], indentLevel + 1);
outputFile << indent << "}\n";
}
}
}
else if (node->type == "While") {
outputFile << indent << "while (";
generateExpression(node->children[0], outputFile);
outputFile << ") {\n";
generateStatement(node->children[1], indentLevel + 1);
outputFile << indent << "}\n";
}
else if (node->type == "DoWhile") {
outputFile << indent << "do {\n";
generateStatement(node->children[0], indentLevel + 1);
outputFile << indent << "} while (";
generateExpression(node->children[1], outputFile);
outputFile << ");\n";
}
else if (node->type == "For") {
outputFile << indent << "for (";

// Initialization
if (node->children[0]->type != "Empty") {
generateStatement(node->children[0], 0);
outputFile.seekp(-2, ios_base::cur); // Remove semicolon
}
outputFile << "; ";

// Condition
if (node->children[1]->type != "Empty") {
generateExpression(node->children[1], outputFile);
}
outputFile << "; ";

// Increment
if (node->children[2]->type != "Empty") {
generateExpression(node->children[2], outputFile);
}
outputFile << ") {\n";

// Body
generateStatement(node->children[3], indentLevel + 1);
outputFile << indent << "}\n";
}
else if (node->type == "Return") {
outputFile << indent << "return";
if (!node->children.empty()) {
outputFile << " ";
generateExpression(node->children[0], outputFile);
}
outputFile << ";\n";
}
else if (node->type == "Block") {
outputFile << indent << "{\n";
for (auto child : node->children) {
generateStatement(child, indentLevel + 1);
}
outputFile << indent << "}\n";
}
else if (node->type == "FunctionCall") {
outputFile << indent << node->value << "(";
for (size_t i = 0; i < node->children.size(); i++) {
if (i > 0) outputFile << ", ";
generateExpression(node->children[i], outputFile);
}
outputFile << ");\n";
}
else if (node->type == "Break") {
outputFile << indent << "break;\n";
}
else if (node->type == "Continue") {
outputFile << indent << "continue;\n";
}
else if (node->type == "Empty") {
outputFile << indent << ";\n";
}
}

void generateExpression(ASTNode* node, ostream& out) {
if (node->type == "Identifier") {
out << node->value;
}
else if (node->type == "Literal") {
out << node->value;
}
else if (node->type == "String") {
out << "\"" << node->value << "\"";
}
else if (node->type == "Operator") {
out << "(";
generateExpression(node->children[0], out);
out << " " << node->value << " ";
generateExpression(node->children[1], out);
out << ")";
}
else if (node->type == "Unary") {
out << node->value;
generateExpression(node->children[0], out);
}
else if (node->type == "Postfix") {
generateExpression(node->children[0], out);
out << node->value;
}
else if (node->type == "FunctionCall") {
out << node->value << "(";
for (size_t i = 0; i < node->children.size(); i++) {
if (i > 0) out << ", ";
generateExpression(node->children[i], out);
}
out << ")";
}
else if (node->type == "Conditional") {
generateExpression(node->children[0], out);
out << " ? ";
generateExpression(node->children[1], out);
out << " : ";
generateExpression(node->children[2], out);
}
}

string getTypeString(DataType type) {
switch(type) {
case INT_TYPE: return "int";
case FLOAT_TYPE: return "float";
case CHAR_TYPE: return "char";
case VOID_TYPE: return "void";
case BOOL_TYPE: return "bool";
case STRING_TYPE: return "string";
default: return "auto";
}
}

bool compileAndExecute() {
outputFile.close();

// Compile the generated C++ code
string compileCmd = "g++ -std=c++11 -o " + outputFilename.substr(0, outputFilename.find_last_of('.')) +
" " + outputFilename + " 2>&1";

cout << CYAN << "\nCOMPILING GENERATED CODE:" << RESET << endl;
cout << "============================\n" << endl;

FILE* pipe = popen(compileCmd.c_str(), "r");
if (!pipe) {
cout << RED << "Failed to compile generated code" << RESET << endl;
return false;
}

char buffer[128];
string result = "";
while (!feof(pipe)) {
if (fgets(buffer, 128, pipe) != NULL)
result += buffer;
}
pclose(pipe);

if (!result.empty()) {
cout << result << endl;
if (result.find("error") != string::npos) {
cout << RED << "✗ Compilation failed" << RESET << endl;
return false;
}
}

cout << GREEN << "✓ Code compiled successfully" << RESET << endl;

// Execute the compiled program
cout << CYAN << "\nEXECUTING PROGRAM:" << RESET << endl;
cout << "==================\n" << endl;

string execCmd = "./" + outputFilename.substr(0, outputFilename.find_last_of('.'));
system(execCmd.c_str());

return true;
}

string getOutputFilename() { return outputFilename; }

vector<CompilerError> getErrors() { return errors; }
};

class CCompiler {
private:
string source;
string filename;
vector<CompilerError> allErrors;
vector<CompilerError> allWarnings;

public:
CCompiler(const string& src, const string& fname)
: source(src), filename(fname) {}

bool compile() {
auto startTime = high_resolution_clock::now();
allErrors.clear();
allWarnings.clear();

cout << BOLD << CYAN << "\n╔══════════════════════════════════════════════════════════╗" << RESET << endl;
cout << BOLD << CYAN << "║ UNIVERSAL C COMPILER - COMPLETE IMPLEMENTATION ║" << RESET << endl;
cout << BOLD << CYAN << "║ Handles any C code with/without main() function ║" << RESET << endl;
cout << BOLD << CYAN << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;

cout << "Compiling: " << filename << endl;
cout << "Source size: " << source.size() << " bytes" << endl;

// PHASE 1: LEXICAL ANALYSIS
cout << BOLD << YELLOW << "\nPHASE 1: LEXICAL ANALYSIS" << RESET << endl;
cout << YELLOW << "=========================\n" << RESET << endl;

Lexer lexer(source);
auto lexStart = high_resolution_clock::now();
vector<Token> tokens = lexer.tokenize();
auto lexEnd = high_resolution_clock::now();
vector<CompilerError> lexErrors = lexer.getErrors();

for (auto& err : lexErrors) {
if (err.severity == "ERROR") allErrors.push_back(err);
else allWarnings.push_back(err);
}

if (!allErrors.empty()) {
cout << RED << "✗ Lexical analysis found " << allErrors.size() << " errors" << RESET << endl;
for (auto& err : lexErrors) if (err.severity == "ERROR") err.display();
} else {
cout << GREEN << "✓ Lexical analysis successful ("
<< duration_cast<microseconds>(lexEnd - lexStart).count() << " μs)" << RESET << endl;
}

lexer.displayTokens();

// PHASE 2: SYNTAX ANALYSIS
cout << BOLD << YELLOW << "\nPHASE 2: SYNTAX ANALYSIS" << RESET << endl;
cout << YELLOW << "========================\n" << RESET << endl;

SymbolTable symTable;
Parser parser(tokens, symTable);
auto parseStart = high_resolution_clock::now();
ASTNode* ast = parser.parse();
auto parseEnd = high_resolution_clock::now();
vector<CompilerError> parseErrors = parser.getErrors();

for (auto& err : parseErrors) {
if (err.severity == "ERROR") allErrors.push_back(err);
else allWarnings.push_back(err);
}

if (!allErrors.empty()) {
cout << RED << "✗ Syntax/Semantic analysis found " <<
count_if(parseErrors.begin(), parseErrors.end(),
[](const CompilerError& e) { return e.severity == "ERROR"; })
<< " errors" << RESET << endl;
for (auto& err : parseErrors) {
if (err.severity == "ERROR") {
err.display();
}
}
} else {
cout << GREEN << "✓ Syntax analysis successful ("
<< duration_cast<microseconds>(parseEnd - parseStart).count() << " μs)" << RESET << endl;
}

// Display warnings
for (auto& err : parseErrors) {
if (err.severity == "WARNING") {
err.display();
}
}

if (allErrors.empty()) {
parser.displayAST();
}

// PHASE 3: SEMANTIC ANALYSIS
cout << BOLD << YELLOW << "\nPHASE 3: SEMANTIC ANALYSIS" << RESET << endl;
cout << YELLOW << "=========================\n" << RESET << endl;

symTable.display();

if (allErrors.empty()) {
cout << GREEN << "✓ Semantic analysis complete" << RESET << endl;
} else {
cout << RED << "✗ Semantic analysis failed" << RESET << endl;
}

// PHASE 4: THREE-ADDRESS CODE GENERATION
cout << BOLD << YELLOW << "\nPHASE 4: THREE-ADDRESS CODE GENERATION" << RESET << endl;
cout << YELLOW << "====================================\n" << RESET << endl;

if (allErrors.empty() && ast) {
vector<TACNode> tacList = parser.generateTAC();

if (tacList.empty()) {
cout << YELLOW << "⚠ No TAC generated (empty program or no expressions)" << RESET << endl;
} else {
cout << CYAN << "THREE-ADDRESS CODE:" << RESET << endl;
cout << "===================" << endl;
for (auto& tac : tacList) {
tac.display();
}
cout << "Total TAC instructions: " << tacList.size() << endl;
}
}

// PHASE 5: CODE GENERATION AND EXECUTION
cout << BOLD << YELLOW << "\nPHASE 5: CODE GENERATION AND EXECUTION" << RESET << endl;
cout << YELLOW << "======================================\n" << RESET << endl;

if (allErrors.empty() && ast) {
CodeGenerator codeGen(filename);
auto codegenStart = high_resolution_clock::now();
codeGen.generateFromAST(ast);
auto codegenEnd = high_resolution_clock::now();

if (codeGen.getErrors().empty()) {
cout << GREEN << "✓ C++ code generated: " << codeGen.getOutputFilename()
<< " (" << duration_cast<microseconds>(codegenEnd - codegenStart).count() << " μs)" << RESET << endl;

// Try to compile and execute
if (codeGen.compileAndExecute()) {
cout << GREEN << "\n✓ Program executed successfully!" << RESET << endl;
}
} else {
for (auto& err : codeGen.getErrors()) {
err.display();
}
}
} else {
cout << YELLOW << "⚠ Skipping code generation due to errors" << RESET << endl;
}

// SUMMARY
auto endTime = high_resolution_clock::now();
auto duration = duration_cast<milliseconds>(endTime - startTime);

cout << BOLD << MAGENTA << "\n╔══════════════════════════════════════════════════════════╗" << RESET << endl;

if (allErrors.empty()) {
cout << BOLD << GREEN << "║ COMPILATION SUCCESSFUL - NO ERRORS ║" << RESET << endl;
} else {
cout << BOLD << RED << "║ COMPILATION FAILED - ERRORS DETECTED ║" << RESET << endl;
}

cout << BOLD << MAGENTA << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;

cout << CYAN << "+------------------------+--------------------------------+" << RESET << endl;
cout << CYAN << "| Metric | Value |" << endl;
cout << CYAN << "+------------------------+--------------------------------+" << RESET << endl;
cout << "| Source size | " << setw(30) << source.size() << " bytes |" << endl;
cout << "| Tokens generated | " << setw(30) << tokens.size() << " |" << endl;
cout << "| Errors | " << setw(30) << allErrors.size() << " |" << endl;
cout << "| Warnings | " << setw(30) << allWarnings.size() << " |" << endl;
cout << "| Total time | " << setw(30) << (to_string(duration.count()) + " ms") << " |" << endl;
cout << CYAN << "+------------------------+--------------------------------+\n" << RESET << endl;

saveResults();
if (ast) delete ast;

return allErrors.empty();
}

void saveResults() {
string baseName = filename.substr(0, filename.find_last_of('.'));
if (baseName.empty()) baseName = "output";

ofstream file(baseName + "_report.txt");
if (file.is_open()) {
file << "Compilation Report for: " << filename << "\n";
file << "Status: " << (allErrors.empty() ? "SUCCESS" : "FAILED") << "\n";
file << "Total errors: " << allErrors.size() << "\n";
file << "Total warnings: " << allWarnings.size() << "\n";
file << "\nERROR DETAILS:\n";
for (auto& err : allErrors) {
file << err.phase << " " << err.severity << " at line " << err.line
<< ", column " << err.column << ": " << err.message << "\n";
}
file << "\nWARNING DETAILS:\n";
for (auto& err : allWarnings) {
file << err.phase << " " << err.severity << " at line " << err.line
<< ", column " << err.column << ": " << err.message << "\n";
}
file.close();
cout << GREEN << "✓ Report saved: " << baseName << "_report.txt" << RESET << endl;
}
}
};

class CompilerMenu {
public:
void run() {
while (true) {
displayMenu();
int choice;
cin >> choice;
cin.ignore();

switch (choice) {
case 1: compileFile(); break;
case 2: interactiveMode(); break;
case 3: runSamples(); break;
case 4: return;
default:
cout << RED << "Invalid choice!" << RESET << endl;
}

cout << YELLOW << "\nPress Enter to continue..." << RESET;
cin.get();
}
}

private:
void displayMenu() {
system("clear || cls");
cout << BOLD << CYAN << "\n╔══════════════════════════════════════════════════════════╗" << RESET << endl;
cout << BOLD << CYAN << "║ UNIVERSAL C COMPILER - MAIN MENU ║" << RESET << endl;
cout << BOLD << CYAN << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;
cout << " 1. Compile a file\n";
cout << " 2. Enter code interactively\n";
cout << " 3. Run sample programs\n";
cout << " 4. Exit\n";
cout << BOLD << "\nSelect option (1-4): " << RESET;
}

void compileFile() {
string filename;
cout << "\nEnter C file name: ";
getline(cin, filename);

ifstream file(filename);
if (!file) {
cout << RED << "Error: Cannot open file!" << RESET << endl;
return;
}

stringstream buffer;
buffer << file.rdbuf();
string code = buffer.str();
file.close();

CCompiler compiler(code, filename);
compiler.compile();
}

void interactiveMode() {
cout << "\nEnter C code line by line (type END on a separate line to finish):\n";
cout << "Note: You can enter code with or without main() function\n" << endl;

string code, line;
int lineNum = 1;
while (true) {
cout << setw(3) << lineNum++ << "> ";
getline(cin, line);
if (line == "END") break;
code += line + "\n";
}

if (code.empty()) {
cout << RED << "No code entered!" << RESET << endl;
return;
}

CCompiler compiler(code, "interactive.c");
compiler.compile();
}

void runSamples() {
vector<string> samples = {

R"(int x;
int y;
int z;

x = 8;
y = 4;

z = x + y * (x - y);

print(z);
)",
// Test without main
R"(x = 10; // Undeclared variable
y = "hello"; // Type error

if (5) { // Non-boolean condition
printf("Hello");
}

for (i = 0; i < 10; i++) { // Undeclared i
printf("%d", i);
}

break; // Not in loop)",
// Test with function errors
R"(int main() {
int x = 10;
float y = 3.14;

// Type mismatch
x = y;

// Invalid operation
bool b = x + y;

// Undeclared function
undefined_function();

// Missing return
})"
};

vector<string> descriptions = {
"Your example: int a = true; bool flag = 5;",
"Various errors: undeclared vars, type errors, invalid conditions",
"Function errors and type mismatches"
};

for (size_t i = 0; i < samples.size(); i++) {
cout << BOLD << "\nSAMPLE " << (i+1) << ": " << descriptions[i] << RESET << endl;
cout << CYAN << "=============================================" << RESET << endl;
cout << samples[i] << endl << endl;

CCompiler compiler(samples[i], "sample_" + to_string(i+1) + ".c");
compiler.compile();

if (i < samples.size() - 1) {
cout << YELLOW << "\nPress Enter for next sample..." << RESET;
cin.get();
}
}
}
};

int main() {
CompilerMenu menu;
menu.run();
return 0;
}



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

// Fast token type enum
enum TokenType {
TK_INT, TK_FLOAT, TK_CHAR, TK_VOID, TK_BOOL, TK_STRING,
TK_IF, TK_ELSE, TK_ELSEIF, TK_WHILE, TK_DO, TK_FOR,
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
TK_SEMICOLON, TK_COMMA, TK_DOT, TK_COLON, TK_QUESTION,
TK_EOF, TK_ERROR,
TK_TRUE, TK_FALSE, TK_NULL,
TK_INCLUDE
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
case TK_TRUE: return "TRUE";
case TK_FALSE: return "FALSE";
case TK_NULL: return "NULL";
case TK_INCLUDE: return "INCLUDE";
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
};

enum SymbolType { VAR_SYM, FUNC_SYM, ARRAY_SYM };
enum DataType { INT_TYPE, FLOAT_TYPE, CHAR_TYPE, VOID_TYPE, BOOL_TYPE, STRING_TYPE, UNKNOWN_TYPE };

struct Symbol {
string name;
SymbolType symType;
DataType dataType;
int line;
int scope;
bool initialized;
bool isFunction;
vector<DataType> paramTypes; // For functions

Symbol(string n = "", SymbolType st = VAR_SYM, DataType dt = INT_TYPE,
int l = 0, int s = 0, bool init = false, bool isFunc = false)
: name(n), symType(st), dataType(dt), line(l), scope(s),
initialized(init), isFunction(isFunc) {}

string typeToString() const {
switch(symType) {
case VAR_SYM: return "variable";
case FUNC_SYM: return "function";
case ARRAY_SYM: return "array";
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
vector<unordered_map<string, Symbol>> tables;
vector<CompilerError> errors;
int currentScope;

public:
SymbolTable() : currentScope(0) {
tables.emplace_back();
}

void enterScope() {
currentScope++;
tables.emplace_back();
}

void exitScope() {
if (currentScope > 0) {
tables.pop_back();
currentScope--;
}
}

bool insert(const string& name, Symbol sym) {
// Check for redeclaration in current scope
if (tables.back().find(name) != tables.back().end()) {
Symbol existing = tables.back()[name];
errors.emplace_back(
sym.line, 1,
"Redeclaration of '" + name + "' (previously declared at line " +
to_string(existing.line) + ")", "Semantic", "ERROR"
);
return false;
}
tables.back()[name] = move(sym);
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

void markInitialized(const string& name) {
Symbol* sym = lookup(name);
if (sym) sym->initialized = true;
}

vector<CompilerError> getErrors() { return errors; }

void display() {
cout << CYAN << "\nSYMBOL TABLE:" << RESET << endl;
cout << "+------------+------------+------------+------+-------+----------+" << endl;
cout << "| Name | Type | Data Type | Line | Scope | Init |" << endl;
cout << "+------------+------------+------------+------+-------+----------+" << endl;

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
<< " | " << setw(8) << (sym.initialized ? "Yes" : "No") << " |" << endl;
}
}
cout << "+------------+------------+------------+------+-------+----------+" << endl;
cout << "Total symbols: " << totalSymbols << " in " << tables.size() << " scopes" << endl;
}
};

struct ASTNode {
string type;
string value;
DataType dataType;
int line;
int column;
vector<ASTNode*> children;

ASTNode(string t, string v = "", DataType dt = UNKNOWN_TYPE, int ln = 0, int col = 0)
: type(move(t)), value(move(v)), dataType(dt), line(ln), column(col) {}

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
cout << " (line " << line << ")" << endl;

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

class FastLexer {
private:
string source;
vector<Token> tokens;
vector<CompilerError> errors;
int pos;
int line;
int column;

char peek() { return (pos < (int)source.length()) ? source[pos] : '\0'; }
char peekNext() { return (pos + 1 < (int)source.length()) ? source[pos + 1] : '\0'; }

char advance() {
char c = peek();
if (c == '\n') { line++; column = 1; }
else column++;
pos++;
return c;
}

void skipWhitespace() {
while (pos < (int)source.length() && isspace(source[pos])) {
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
pos++; // Skip '#'
while (pos < (int)source.length() && source[pos] != '\n') {
if (source[pos] == '\n') {
line++;
column = 1;
} else {
column++;
}
pos++;
}
if (pos < (int)source.length() && source[pos] == '\n') {
line++;
column = 1;
pos++;
}
}

void skipComment() {
if (peekNext() == '/') {
// Single line comment
pos += 2;
column += 2;
while (pos < (int)source.length() && source[pos] != '\n') {
if (source[pos] == '\n') {
line++;
column = 1;
} else {
column++;
}
pos++;
}
} else if (peekNext() == '*') {
// Multi-line comment
pos += 2;
column += 2;
while (pos < (int)source.length()) {
if (source[pos] == '*' && pos + 1 < (int)source.length() && source[pos + 1] == '/') {
pos += 2;
column += 2;
break;
}
if (source[pos] == '\n') {
line++;
column = 1;
} else {
column++;
}
pos++;
}
}
}

Token identifierOrKeyword(int start) {
int begin = start;
while (pos < (int)source.length() && (isalnum(source[pos]) || source[pos] == '_')) {
pos++;
column++;
}

string text = source.substr(begin, pos - begin);

static unordered_map<string, TokenType> keywords = {
{"int", TK_INT}, {"float", TK_FLOAT}, {"char", TK_CHAR},
{"void", TK_VOID}, {"bool", TK_BOOL}, {"string", TK_STRING},
{"true", TK_TRUE}, {"false", TK_FALSE},
{"if", TK_IF}, {"else", TK_ELSE}, {"elseif", TK_ELSEIF},
{"while", TK_WHILE}, {"do", TK_DO}, {"for", TK_FOR},
{"return", TK_RETURN}, {"break", TK_BREAK}, {"continue", TK_CONTINUE},
{"printf", TK_PRINTF}, {"scanf", TK_SCANF}, {"print", TK_PRINT},
{"NULL", TK_NULL}, {"null", TK_NULL}
};

auto it = keywords.find(text);
if (it != keywords.end()) {
return Token(it->second, text, line, column - (int)text.length());
}
return Token(TK_IDENTIFIER, text, line, column - (int)text.length());
}

Token number(int start) {
int begin = start;
bool isFloat = false;
bool hasError = false;
while (pos < (int)source.length() && (isdigit(source[pos]) || source[pos] == '.' || source[pos] == 'e' || source[pos] == 'E')) {
if (source[pos] == '.') {
if (isFloat) {
hasError = true;
}
isFloat = true;
}
pos++;
column++;
}

string text = source.substr(begin, pos - begin);

if (hasError) {
errors.emplace_back(line, column, "Invalid number format: " + text, "Lexical", "ERROR");
}

return Token(isFloat ? TK_FLOATNUM : TK_INTEGER, text, line, column - (int)text.length());
}

Token stringLiteral() {
int startLine = line;
int startCol = column;
pos++;
column++;

while (pos < (int)source.length() && source[pos] != '"') {
if (source[pos] == '\\' && pos + 1 < (int)source.length()) {
pos += 2;
column += 2;
} else {
if (source[pos] == '\n') {
errors.emplace_back(line, column, "Unterminated string literal", "Lexical", "ERROR");
line++;
column = 1;
}
pos++;
column++;
}
}

if (pos >= (int)source.length()) {
errors.emplace_back(startLine, startCol, "Unterminated string literal", "Lexical", "ERROR");
return Token(TK_STRINGLIT, "", startLine, startCol);
}

string text = source.substr(startCol, pos - startCol);
pos++;
column++;

return Token(TK_STRINGLIT, text, startLine, startCol);
}

Token charLiteral() {
int startLine = line;
int startCol = column;
pos++;
column++;

if (pos >= (int)source.length() || source[pos] == '\'') {
errors.emplace_back(startLine, startCol, "Empty character literal", "Lexical", "ERROR");
if (pos < (int)source.length() && source[pos] == '\'') {
pos++;
column++;
}
return Token(TK_CHARLIT, "", startLine, startCol);
}

if (source[pos] == '\\') {
if (pos + 1 < (int)source.length()) {
pos += 2;
column += 2;
} else {
errors.emplace_back(startLine, startCol, "Invalid escape sequence", "Lexical", "ERROR");
return Token(TK_CHARLIT, "", startLine, startCol);
}
} else {
pos++;
column++;
}

if (pos >= (int)source.length() || source[pos] != '\'') {
errors.emplace_back(startLine, startCol, "Unterminated character literal", "Lexical", "ERROR");
return Token(TK_CHARLIT, "", startLine, startCol);
}

pos++;
column++;
return Token(TK_CHARLIT, source.substr(startCol + 1, 1), startLine, startCol);
}

public:
FastLexer(const string& src) : source(src), pos(0), line(1), column(1) {}

vector<Token> tokenize() {
tokens.clear();
errors.clear();

while (pos < (int)source.length()) {
skipWhitespace();
if (pos >= (int)source.length()) break;

// Skip preprocessor
if (source[pos] == '#') {
skipPreprocessor();
continue;
}

// Check for comments
if (source[pos] == '/' && pos + 1 < (int)source.length()) {
if (source[pos+1] == '/' || source[pos+1] == '*') {
skipComment();
continue;
}
}

int tokenLine = line;
int tokenCol = column;

if (isalpha(source[pos]) || source[pos] == '_') {
int start = pos;
advance();
tokens.push_back(identifierOrKeyword(start));
continue;
}

if (isdigit(source[pos])) {
int start = pos;
advance();
tokens.push_back(number(start));
continue;
}

if (source[pos] == '"') {
tokens.push_back(stringLiteral());
continue;
}

if (source[pos] == '\'') {
tokens.push_back(charLiteral());
continue;
}

// Fast switch for operators
switch (source[pos]) {
case '+':
if (peekNext() == '+') {
pos += 2; column += 2;
tokens.emplace_back(TK_INCREMENT, "++", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_PLUS, "+", tokenLine, tokenCol);
}
break;

case '-':
if (peekNext() == '-') {
pos += 2; column += 2;
tokens.emplace_back(TK_DECREMENT, "--", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_MINUS, "-", tokenLine, tokenCol);
}
break;

case '*': pos++; column++; tokens.emplace_back(TK_MULTIPLY, "*", tokenLine, tokenCol); break;
case '/': pos++; column++; tokens.emplace_back(TK_DIVIDE, "/", tokenLine, tokenCol); break;
case '%': pos++; column++; tokens.emplace_back(TK_MOD, "%", tokenLine, tokenCol); break;
case '.': pos++; column++; tokens.emplace_back(TK_DOT, ".", tokenLine, tokenCol); break;
case ':': pos++; column++; tokens.emplace_back(TK_COLON, ":", tokenLine, tokenCol); break;
case '?': pos++; column++; tokens.emplace_back(TK_QUESTION, "?", tokenLine, tokenCol); break;

case '=':
if (peekNext() == '=') {
pos += 2; column += 2;
tokens.emplace_back(TK_EQUAL, "==", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_ASSIGN, "=", tokenLine, tokenCol);
}
break;

case '!':
if (peekNext() == '=') {
pos += 2; column += 2;
tokens.emplace_back(TK_NOTEQUAL, "!=", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_NOT, "!", tokenLine, tokenCol);
}
break;

case '<':
if (peekNext() == '=') {
pos += 2; column += 2;
tokens.emplace_back(TK_LESSEQ, "<=", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_LESS, "<", tokenLine, tokenCol);
}
break;

case '>':
if (peekNext() == '=') {
pos += 2; column += 2;
tokens.emplace_back(TK_GREATEREQ, ">=", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_GREATER, ">", tokenLine, tokenCol);
}
break;

case '&':
if (peekNext() == '&') {
pos += 2; column += 2;
tokens.emplace_back(TK_AND, "&&", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_AND, "&", tokenLine, tokenCol);
}
break;

case '|':
if (peekNext() == '|') {
pos += 2; column += 2;
tokens.emplace_back(TK_OR, "||", tokenLine, tokenCol);
} else {
pos++; column++;
tokens.emplace_back(TK_OR, "|", tokenLine, tokenCol);
}
break;

case '(': pos++; column++; tokens.emplace_back(TK_LPAREN, "(", tokenLine, tokenCol); break;
case ')': pos++; column++; tokens.emplace_back(TK_RPAREN, ")", tokenLine, tokenCol); break;
case '{': pos++; column++; tokens.emplace_back(TK_LBRACE, "{", tokenLine, tokenCol); break;
case '}': pos++; column++; tokens.emplace_back(TK_RBRACE, "}", tokenLine, tokenCol); break;
case '[': pos++; column++; tokens.emplace_back(TK_LBRACKET, "[", tokenLine, tokenCol); break;
case ']': pos++; column++; tokens.emplace_back(TK_RBRACKET, "]", tokenLine, tokenCol); break;
case ';': pos++; column++; tokens.emplace_back(TK_SEMICOLON, ";", tokenLine, tokenCol); break;
case ',': pos++; column++; tokens.emplace_back(TK_COMMA, ",", tokenLine, tokenCol); break;

default:
errors.emplace_back(line, column,
string("Invalid character '") + source[pos] + "'", "Lexical", "ERROR");
pos++; column++;
break;
}
}

tokens.emplace_back(TK_EOF, "EOF", line, column);
return tokens;
}

vector<CompilerError> getErrors() { return errors; }

void displayTokens() {
cout << CYAN << "\nTOKEN LIST:" << RESET << endl;
cout << "+----------+------------+------+--------+" << endl;
cout << "| Lexeme | Type | Line | Column |" << endl;
cout << "+----------+------------+------+--------+" << endl;

int count = 0;
for (size_t i = 0; i < tokens.size() && count < 50; i++) {
if (tokens[i].type == TK_EOF) continue;

string lexeme = tokens[i].lexeme;
if (lexeme.length() > 8) lexeme = lexeme.substr(0,7) + "...";

cout << "| " << left << setw(8) << lexeme
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

class FastParser {
private:
const vector<Token>& tokens;
SymbolTable& symTable;
size_t current;
ASTNode* astRoot;
vector<CompilerError> errors;
bool hasMainFunction;
bool hadError;
bool recoveryMode;

const Token& peek() { return tokens[current]; }
const Token& previous() { return tokens[current - 1]; }
bool isAtEnd() { return peek().type == TK_EOF; }

bool check(TokenType type) { return peek().type == type; }

const Token& advance() {
if (!isAtEnd()) current++;
return previous();
}

bool match(TokenType type) {
if (check(type)) {
current++;
return true;
}
return false;
}

const Token& consume(TokenType type, const string& message) {
if (check(type)) return advance();

if (!recoveryMode) {
errors.emplace_back(peek().line, peek().column,
message + " (found '" + peek().lexeme + "' instead)", "Syntax", "ERROR");
hadError = true;
recoveryMode = true;
}

static Token errorToken(type, "", peek().line, peek().column);
return errorToken;
}

void synchronize() {
recoveryMode = false;
hadError = false;

while (!isAtEnd()) {
if (previous().type == TK_SEMICOLON) {
// Found statement boundary
return;
}

switch (peek().type) {
case TK_INT: case TK_FLOAT: case TK_CHAR: case TK_VOID:
case TK_BOOL: case TK_STRING:
case TK_IF: case TK_WHILE: case TK_DO: case TK_FOR:
case TK_PRINTF: case TK_SCANF: case TK_PRINT:
case TK_RETURN: case TK_BREAK: case TK_CONTINUE:
case TK_LBRACE: case TK_RBRACE:
return;
default:
advance();
break;
}
}
}

void reportError(const string& message, const Token& token) {
if (!hadError) {
errors.emplace_back(token.line, token.column, message, "Syntax", "ERROR");
hadError = true;
}
}

DataType getDataType(const Token& token) {
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

bool isTypeToken(TokenType type) {
return type == TK_INT || type == TK_FLOAT || type == TK_CHAR ||
type == TK_VOID || type == TK_BOOL || type == TK_STRING;
}

ASTNode* program() {
auto* node = new ASTNode("Program");
astRoot = node;

while (!isAtEnd()) {
if (match(TK_INCLUDE)) {
continue;
}

if (isTypeToken(peek().type)) {
Token typeToken = advance();

if (check(TK_IDENTIFIER)) {
// Look ahead for function
size_t lookahead = current + 1;
if (lookahead < tokens.size() && tokens[lookahead].type == TK_LPAREN) {
node->addChild(function(typeToken));
} else {
current--;
ASTNode* decl = declaration();
if (decl) node->addChild(decl);
}
} else {
reportError("Expected identifier after type", peek());
synchronize();
}
} else if (check(TK_LBRACE)) {
node->addChild(block());
} else {
ASTNode* stmt = statement();
if (stmt) node->addChild(stmt);
}
}

return node;
}

ASTNode* function(const Token& typeToken) {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");

if (nameToken.lexeme == "main") {
hasMainFunction = true;
if (typeToken.type != TK_INT && typeToken.type != TK_VOID) {
errors.emplace_back(nameToken.line, nameToken.column,
"main() must return int or void", "Semantic", "ERROR");
}
}

// Insert function symbol
Symbol funcSym(nameToken.lexeme, FUNC_SYM, getDataType(typeToken),
nameToken.line, 0, true, true);
symTable.insert(nameToken.lexeme, funcSym);

auto* node = new ASTNode("Function", nameToken.lexeme, getDataType(typeToken),
nameToken.line, nameToken.column);

symTable.enterScope();

// Parse parameters
vector<ASTNode*> params;
consume(TK_LPAREN, "Expected '(' after function name");

if (!check(TK_RPAREN)) {
do {
if (isTypeToken(peek().type)) {
Token paramType = advance();
Token paramName = consume(TK_IDENTIFIER, "Expected parameter name");

// Add parameter to symbol table
symTable.insert(paramName.lexeme,
Symbol(paramName.lexeme, VAR_SYM, getDataType(paramType),
paramName.line, symTable.lookup("main") ? 1 : 0, true));

auto* paramNode = new ASTNode("Param", paramName.lexeme,
getDataType(paramType),
paramName.line, paramName.column);
params.push_back(paramNode);
node->addChild(paramNode);

funcSym.paramTypes.push_back(getDataType(paramType));
} else {
reportError("Expected type in parameter list", peek());
synchronize();
break;
}
} while (match(TK_COMMA));
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_LBRACE, "Expected '{'");

// Parse function body
while (!check(TK_RBRACE) && !isAtEnd()) {
ASTNode* stmt = statement();
if (stmt) node->addChild(stmt);
}

consume(TK_RBRACE, "Expected '}'");

// Check for return statement in non-void functions
if (getDataType(typeToken) != VOID_TYPE && nameToken.lexeme != "main") {
// Simple check - in real compiler, do control flow analysis
bool hasReturn = false;
for (auto child : node->children) {
if (child->type == "Return") {
hasReturn = true;
break;
}
}
if (!hasReturn) {
errors.emplace_back(nameToken.line, nameToken.column,
"Non-void function '" + nameToken.lexeme + "' lacks return statement",
"Semantic", "WARNING");
}
}

symTable.exitScope();

return node;
}

ASTNode* declaration() {
Token typeToken = advance();
DataType dtype = getDataType(typeToken);

Token nameToken = consume(TK_IDENTIFIER, "Expected variable name");

// Check for array declaration
bool isArray = false;
int arraySize = 0;
if (match(TK_LBRACKET)) {
isArray = true;
if (check(TK_INTEGER)) {
Token sizeToken = advance();
arraySize = stoi(sizeToken.lexeme);
if (arraySize <= 0) {
errors.emplace_back(sizeToken.line, sizeToken.column,
"Array size must be positive", "Semantic", "ERROR");
}
} else {
reportError("Expected array size", peek());
}
consume(TK_RBRACKET, "Expected ']'");
}

Symbol sym(nameToken.lexeme, isArray ? ARRAY_SYM : VAR_SYM, dtype,
nameToken.line, symTable.lookup("main") ? 1 : 0, false);
if (!symTable.insert(nameToken.lexeme, sym)) {
// Redeclaration error already reported
}

auto* node = new ASTNode(isArray ? "ArrayDeclaration" : "Declaration",
nameToken.lexeme, dtype, nameToken.line, nameToken.column);

if (match(TK_ASSIGN)) {
symTable.markInitialized(nameToken.lexeme);
ASTNode* expr = expression();
if (expr) node->addChild(expr);

// Type checking
if (expr && expr->dataType != dtype && expr->dataType != UNKNOWN_TYPE) {
errors.emplace_back(nameToken.line, nameToken.column,
"Type mismatch in initialization of '" + nameToken.lexeme + "'",
"Semantic", "ERROR");
}
}

consume(TK_SEMICOLON, "Expected ';' after declaration");
return node;
}

ASTNode* statement() {
if (hadError) {
synchronize();
hadError = false;
}

if (check(TK_IF)) return ifStatement();
if (check(TK_WHILE)) return whileStatement();
if (check(TK_DO)) return doWhileStatement();
if (check(TK_FOR)) return forStatement();
if (check(TK_PRINTF) || check(TK_SCANF) || check(TK_PRINT)) return ioStatement();
if (check(TK_RETURN)) return returnStatement();
if (check(TK_LBRACE)) return block();
if (isTypeToken(peek().type)) return declaration();
if (check(TK_BREAK) || check(TK_CONTINUE)) {
Token token = advance();
auto* node = new ASTNode(token.type == TK_BREAK ? "Break" : "Continue",
"", UNKNOWN_TYPE, token.line, token.column);
consume(TK_SEMICOLON, "Expected ';'");
return node;
}
if (check(TK_SEMICOLON)) {
advance();
return new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column);
}

// Check for function call
if (check(TK_IDENTIFIER) && current + 1 < tokens.size() &&
tokens[current + 1].type == TK_LPAREN) {
return functionCallStatement();
}

return expressionStatement();
}

ASTNode* ifStatement() {
Token ifToken = consume(TK_IF, "Expected 'if'");
auto* node = new ASTNode("If", "", UNKNOWN_TYPE, ifToken.line, ifToken.column);

consume(TK_LPAREN, "Expected '(' after 'if'");
ASTNode* cond = expression();
if (cond) {
node->addChild(cond);
// Check if condition is boolean
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.emplace_back(ifToken.line, ifToken.column,
"Condition in if statement must be boolean", "Semantic", "ERROR");
}
}
consume(TK_RPAREN, "Expected ')' after condition");

ASTNode* thenStmt = statement();
if (thenStmt) node->addChild(thenStmt);

while (match(TK_ELSEIF)) {
Token elseifToken = previous();
consume(TK_LPAREN, "Expected '(' after 'elseif'");
ASTNode* elseifCond = expression();
if (elseifCond) {
node->addChild(elseifCond);
// Type check
if (elseifCond->dataType != BOOL_TYPE && elseifCond->dataType != UNKNOWN_TYPE) {
errors.emplace_back(elseifToken.line, elseifToken.column,
"Condition in elseif must be boolean", "Semantic", "ERROR");
}
}
consume(TK_RPAREN, "Expected ')' after condition");
ASTNode* elseifStmt = statement();
if (elseifStmt) node->addChild(elseifStmt);
}

if (match(TK_ELSE)) {
ASTNode* elseStmt = statement();
if (elseStmt) node->addChild(elseStmt);
}

return node;
}

ASTNode* whileStatement() {
Token whileToken = consume(TK_WHILE, "Expected 'while'");
auto* node = new ASTNode("While", "", UNKNOWN_TYPE, whileToken.line, whileToken.column);

consume(TK_LPAREN, "Expected '(' after 'while'");
ASTNode* cond = expression();
if (cond) {
node->addChild(cond);
// Type check
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.emplace_back(whileToken.line, whileToken.column,
"Condition in while statement must be boolean", "Semantic", "ERROR");
}
}
consume(TK_RPAREN, "Expected ')' after condition");

ASTNode* body = statement();
if (body) node->addChild(body);

return node;
}

ASTNode* doWhileStatement() {
Token doToken = consume(TK_DO, "Expected 'do'");
auto* node = new ASTNode("DoWhile", "", UNKNOWN_TYPE, doToken.line, doToken.column);

ASTNode* body = statement();
if (body) node->addChild(body);

consume(TK_WHILE, "Expected 'while' after do block");
consume(TK_LPAREN, "Expected '(' after 'while'");
ASTNode* cond = expression();
if (cond) {
node->addChild(cond);
// Type check
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.emplace_back(doToken.line, doToken.column,
"Condition in do-while must be boolean", "Semantic", "ERROR");
}
}
consume(TK_RPAREN, "Expected ')' after condition");
consume(TK_SEMICOLON, "Expected ';' after do-while");

return node;
}

ASTNode* forStatement() {
Token forToken = consume(TK_FOR, "Expected 'for'");
auto* node = new ASTNode("For", "", UNKNOWN_TYPE, forToken.line, forToken.column);

consume(TK_LPAREN, "Expected '(' after 'for'");

symTable.enterScope();

// Initialization
if (match(TK_SEMICOLON)) {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, forToken.line, forToken.column));
} else if (isTypeToken(peek().type)) {
ASTNode* init = declaration();
if (init) node->addChild(init);
} else {
ASTNode* initExpr = expression();
if (initExpr) node->addChild(initExpr);
consume(TK_SEMICOLON, "Expected ';' after for initialization");
}

// Condition
if (!check(TK_SEMICOLON)) {
ASTNode* cond = expression();
if (cond) {
node->addChild(cond);
// Type check
if (cond->dataType != BOOL_TYPE && cond->dataType != UNKNOWN_TYPE) {
errors.emplace_back(forToken.line, forToken.column,
"Condition in for statement must be boolean", "Semantic", "ERROR");
}
}
} else {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, forToken.line, forToken.column));
}
consume(TK_SEMICOLON, "Expected ';' after condition");

// Update
if (!check(TK_RPAREN)) {
ASTNode* update = expression();
if (update) node->addChild(update);
} else {
node->addChild(new ASTNode("Empty", "", UNKNOWN_TYPE, forToken.line, forToken.column));
}
consume(TK_RPAREN, "Expected ')' after for clauses");

// Body
ASTNode* body = statement();
if (body) node->addChild(body);

symTable.exitScope();
return node;
}

ASTNode* ioStatement() {
bool isPrintf = match(TK_PRINTF);
bool isPrint = match(TK_PRINT);
if (!isPrintf && !isPrint) match(TK_SCANF);

Token ioToken = previous();
auto* node = new ASTNode(isPrintf ? "Printf" : (isPrint ? "Print" : "Scanf"),
ioToken.lexeme, UNKNOWN_TYPE, ioToken.line, ioToken.column);

consume(TK_LPAREN, "Expected '(' after '" + ioToken.lexeme + "'");

if (isPrintf || isPrint) {
if (check(TK_STRINGLIT)) {
Token formatToken = advance();
node->addChild(new ASTNode("String", formatToken.lexeme, STRING_TYPE,
formatToken.line, formatToken.column));
} else {
reportError("Expected format string or expression", peek());
}

while (match(TK_COMMA)) {
if (check(TK_RPAREN)) break;
ASTNode* expr = expression();
if (expr) node->addChild(expr);
}
} else {
// scanf
if (check(TK_STRINGLIT)) {
Token formatToken = advance();
node->addChild(new ASTNode("String", formatToken.lexeme, STRING_TYPE,
formatToken.line, formatToken.column));

while (match(TK_COMMA)) {
if (check(TK_RPAREN)) break;
if (check(TK_AND)) {
advance(); // Skip &
}
if (check(TK_IDENTIFIER)) {
Token varToken = advance();
node->addChild(new ASTNode("Identifier", varToken.lexeme, UNKNOWN_TYPE,
varToken.line, varToken.column));

// Check if variable exists
Symbol* sym = symTable.lookup(varToken.lexeme);
if (!sym) {
errors.emplace_back(varToken.line, varToken.column,
"Undeclared variable '" + varToken.lexeme + "' in scanf",
"Semantic", "ERROR");
}
} else {
reportError("Expected variable name in scanf", peek());
}
}
} else {
reportError("Expected format string in scanf", peek());
}
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_SEMICOLON, "Expected ';' after statement");

return node;
}

ASTNode* functionCallStatement() {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");
auto* node = new ASTNode("FunctionCall", nameToken.lexeme, UNKNOWN_TYPE,
nameToken.line, nameToken.column);

// Check if function exists
Symbol* sym = symTable.lookup(nameToken.lexeme);
if (!sym || !sym->isFunction) {
errors.emplace_back(nameToken.line, nameToken.column,
"Undeclared function '" + nameToken.lexeme + "'", "Semantic", "ERROR");
}

consume(TK_LPAREN, "Expected '(' after function name");

int argCount = 0;
if (!check(TK_RPAREN)) {
do {
ASTNode* arg = expression();
if (arg) {
node->addChild(arg);
argCount++;
}
} while (match(TK_COMMA));
}

// Check argument count if function exists
if (sym && sym->isFunction) {
if (argCount != sym->paramTypes.size()) {
errors.emplace_back(nameToken.line, nameToken.column,
"Function '" + nameToken.lexeme + "' expects " +
to_string(sym->paramTypes.size()) + " arguments, got " +
to_string(argCount), "Semantic", "ERROR");
}
}

consume(TK_RPAREN, "Expected ')'");
consume(TK_SEMICOLON, "Expected ';' after function call");

return node;
}

ASTNode* returnStatement() {
Token returnToken = consume(TK_RETURN, "Expected 'return'");
auto* node = new ASTNode("Return", "", UNKNOWN_TYPE, returnToken.line, returnToken.column);

if (!check(TK_SEMICOLON)) {
ASTNode* expr = expression();
if (expr) node->addChild(expr);
}

consume(TK_SEMICOLON, "Expected ';' after return");
return node;
}

ASTNode* block() {
Token braceToken = consume(TK_LBRACE, "Expected '{'");

symTable.enterScope();
auto* node = new ASTNode("Block", "", UNKNOWN_TYPE, braceToken.line, braceToken.column);

while (!check(TK_RBRACE) && !isAtEnd()) {
ASTNode* stmt = statement();
if (stmt) node->addChild(stmt);
}

consume(TK_RBRACE, "Expected '}'");
symTable.exitScope();

return node;
}

ASTNode* expressionStatement() {
ASTNode* expr = expression();
if (expr) {
consume(TK_SEMICOLON, "Expected ';' after expression");
} else {
// If we couldn't parse an expression, try to recover
if (!check(TK_SEMICOLON)) {
reportError("Expected expression", peek());
synchronize();
} else {
advance();
}
}
return expr ? expr : new ASTNode("Empty", "", UNKNOWN_TYPE, peek().line, peek().column);
}

ASTNode* expression() { return assignment(); }

ASTNode* assignment() {
ASTNode* expr = logicalOr();

if (match(TK_ASSIGN)) {
Token assignToken = previous();
auto* assignNode = new ASTNode("Assignment", "=", UNKNOWN_TYPE,
assignToken.line, assignToken.column);
assignNode->addChild(expr);
ASTNode* value = assignment();
if (value) assignNode->addChild(value);

if (expr && expr->type == "Identifier") {
string varName = expr->value;
Symbol* sym = symTable.lookup(varName);
if (!sym) {
errors.emplace_back(expr->line, expr->column,
"Undeclared variable '" + varName + "'", "Semantic", "ERROR");
} else {
symTable.markInitialized(varName);

// Type checking
if (value && value->dataType != sym->dataType && value->dataType != UNKNOWN_TYPE) {
errors.emplace_back(assignToken.line, assignToken.column,
"Type mismatch in assignment to '" + varName + "'",
"Semantic", "ERROR");
}
}
}

return assignNode;
}

return expr;
}

ASTNode* logicalOr() {
ASTNode* expr = logicalAnd();

while (match(TK_OR)) {
Token opToken = previous();
auto* opNode = new ASTNode("Operator", "||", BOOL_TYPE,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = logicalAnd();
if (right) {
opNode->addChild(right);

// Type checking
if (expr->dataType != BOOL_TYPE && expr->dataType != UNKNOWN_TYPE) {
errors.emplace_back(expr->line, expr->column,
"Left operand of '||' must be boolean", "Semantic", "ERROR");
}
if (right->dataType != BOOL_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.emplace_back(right->line, right->column,
"Right operand of '||' must be boolean", "Semantic", "ERROR");
}
}
expr = opNode;
}

return expr;
}

ASTNode* logicalAnd() {
ASTNode* expr = equality();

while (match(TK_AND)) {
Token opToken = previous();
auto* opNode = new ASTNode("Operator", "&&", BOOL_TYPE,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = equality();
if (right) {
opNode->addChild(right);

// Type checking
if (expr->dataType != BOOL_TYPE && expr->dataType != UNKNOWN_TYPE) {
errors.emplace_back(expr->line, expr->column,
"Left operand of '&&' must be boolean", "Semantic", "ERROR");
}
if (right->dataType != BOOL_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.emplace_back(right->line, right->column,
"Right operand of '&&' must be boolean", "Semantic", "ERROR");
}
}
expr = opNode;
}

return expr;
}

ASTNode* equality() {
ASTNode* expr = comparison();

while (match(TK_EQUAL) || match(TK_NOTEQUAL)) {
Token opToken = previous();
auto* opNode = new ASTNode("Operator", opToken.lexeme, BOOL_TYPE,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = comparison();
if (right) {
opNode->addChild(right);

// Type checking
if (expr->dataType != right->dataType &&
expr->dataType != UNKNOWN_TYPE && right->dataType != UNKNOWN_TYPE) {
errors.emplace_back(opToken.line, opToken.column,
"Type mismatch in comparison", "Semantic", "ERROR");
}
}
expr = opNode;
}

return expr;
}

ASTNode* comparison() {
ASTNode* expr = term();

while (match(TK_LESS) || match(TK_GREATER) ||
match(TK_LESSEQ) || match(TK_GREATEREQ)) {
Token opToken = previous();
auto* opNode = new ASTNode("Operator", opToken.lexeme, BOOL_TYPE,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = term();
if (right) {
opNode->addChild(right);

// Type checking - comparisons work on numeric types
if ((expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != UNKNOWN_TYPE) ||
(right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != UNKNOWN_TYPE)) {
errors.emplace_back(opToken.line, opToken.column,
"Comparison operators require numeric types", "Semantic", "ERROR");
}
}
expr = opNode;
}

return expr;
}

ASTNode* term() {
ASTNode* expr = factor();

while (match(TK_PLUS) || match(TK_MINUS)) {
Token opToken = previous();
DataType resultType = UNKNOWN_TYPE;
if (expr && expr->dataType == FLOAT_TYPE) resultType = FLOAT_TYPE;
else resultType = INT_TYPE;

auto* opNode = new ASTNode("Operator", opToken.lexeme, resultType,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = factor();
if (right) {
opNode->addChild(right);

// Type checking
if ((expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != UNKNOWN_TYPE) ||
(right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != UNKNOWN_TYPE)) {
errors.emplace_back(opToken.line, opToken.column,
"Arithmetic operators require numeric types", "Semantic", "ERROR");
}

// Determine result type
if (expr->dataType == FLOAT_TYPE || right->dataType == FLOAT_TYPE) {
opNode->dataType = FLOAT_TYPE;
} else if (expr->dataType == INT_TYPE && right->dataType == INT_TYPE) {
opNode->dataType = INT_TYPE;
}
}
expr = opNode;
}

return expr;
}

ASTNode* factor() {
ASTNode* expr = unary();

while (match(TK_MULTIPLY) || match(TK_DIVIDE) || match(TK_MOD)) {
Token opToken = previous();
DataType resultType = UNKNOWN_TYPE;
if (expr && expr->dataType == FLOAT_TYPE) resultType = FLOAT_TYPE;
else resultType = INT_TYPE;

auto* opNode = new ASTNode("Operator", opToken.lexeme, resultType,
opToken.line, opToken.column);
opNode->addChild(expr);
ASTNode* right = unary();
if (right) {
opNode->addChild(right);

// Type checking
if ((expr->dataType != INT_TYPE && expr->dataType != FLOAT_TYPE &&
expr->dataType != UNKNOWN_TYPE) ||
(right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE &&
right->dataType != UNKNOWN_TYPE)) {
errors.emplace_back(opToken.line, opToken.column,
"Arithmetic operators require numeric types", "Semantic", "ERROR");
}

// For MOD, require integer types
if (opToken.type == TK_MOD &&
(expr->dataType != INT_TYPE && expr->dataType != UNKNOWN_TYPE ||
right->dataType != INT_TYPE && right->dataType != UNKNOWN_TYPE)) {
errors.emplace_back(opToken.line, opToken.column,
"Modulo operator requires integer types", "Semantic", "ERROR");
}

// Determine result type
if (expr->dataType == FLOAT_TYPE || right->dataType == FLOAT_TYPE) {
if (opToken.type != TK_MOD) {
opNode->dataType = FLOAT_TYPE;
}
} else if (expr->dataType == INT_TYPE && right->dataType == INT_TYPE) {
opNode->dataType = INT_TYPE;
}
}
expr = opNode;
}

return expr;
}

ASTNode* unary() {
if (match(TK_MINUS) || match(TK_NOT) || match(TK_INCREMENT) || match(TK_DECREMENT)) {
Token opToken = previous();
DataType resultType = UNKNOWN_TYPE;

if (opToken.type == TK_NOT) resultType = BOOL_TYPE;
else if (opToken.type == TK_MINUS) resultType = INT_TYPE; // Default

auto* node = new ASTNode("Unary", opToken.lexeme, resultType,
opToken.line, opToken.column);
ASTNode* operand = unary();
if (operand) {
node->addChild(operand);

// Type checking
if (opToken.type == TK_NOT) {
if (operand->dataType != BOOL_TYPE && operand->dataType != UNKNOWN_TYPE) {
errors.emplace_back(opToken.line, opToken.column,
"Logical NOT requires boolean operand", "Semantic", "ERROR");
}
} else {
// Arithmetic unary operators
if (operand->dataType != INT_TYPE && operand->dataType != FLOAT_TYPE &&
operand->dataType != UNKNOWN_TYPE) {
errors.emplace_back(opToken.line, opToken.column,
"Unary minus requires numeric operand", "Semantic", "ERROR");
}
node->dataType = operand->dataType;
}
}
return node;
}

return postfix();
}

ASTNode* postfix() {
ASTNode* expr = primary();

while (match(TK_INCREMENT) || match(TK_DECREMENT)) {
Token opToken = previous();
auto* node = new ASTNode("Postfix", opToken.lexeme, expr->dataType,
opToken.line, opToken.column);
node->addChild(expr);

// Check if operand is a variable
if (expr->type != "Identifier") {
errors.emplace_back(opToken.line, opToken.column,
"Increment/decrement requires variable operand", "Semantic", "ERROR");
} else {
// Check if variable exists
Symbol* sym = symTable.lookup(expr->value);
if (!sym) {
errors.emplace_back(expr->line, expr->column,
"Undeclared variable '" + expr->value + "'", "Semantic", "ERROR");
} else if (sym->dataType != INT_TYPE && sym->dataType != FLOAT_TYPE) {
errors.emplace_back(expr->line, expr->column,
"Increment/decrement requires numeric variable", "Semantic", "ERROR");
}
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

// Check if this is a function call
if (check(TK_LPAREN)) {
current--;
return functionCall();
}

DataType dt = UNKNOWN_TYPE;
if (sym) {
dt = sym->dataType;
if (!sym->initialized && sym->symType == VAR_SYM) {
errors.emplace_back(token.line, token.column,
"Variable '" + varName + "' used before initialization",
"Semantic", "WARNING");
}
} else {
errors.emplace_back(token.line, token.column,
"Undeclared variable '" + varName + "'", "Semantic", "ERROR");
}

return new ASTNode("Identifier", varName, dt, token.line, token.column);
} else if (match(TK_LPAREN)) {
Token parenToken = previous();
ASTNode* expr = expression();
if (!expr) {
reportError("Expected expression after '('", peek());
synchronize();
return new ASTNode("Error", "", UNKNOWN_TYPE, parenToken.line, parenToken.column);
}
consume(TK_RPAREN, "Expected ')' after expression");
return expr;
} else {
if (!recoveryMode) {
reportError("Expected expression", peek());
hadError = true;
}
// Try to recover
synchronize();
return new ASTNode("Error", peek().lexeme, UNKNOWN_TYPE, peek().line, peek().column);
}
}

ASTNode* functionCall() {
Token nameToken = consume(TK_IDENTIFIER, "Expected function name");
auto* node = new ASTNode("FunctionCall", nameToken.lexeme, UNKNOWN_TYPE,
nameToken.line, nameToken.column);

// Check if function exists
Symbol* sym = symTable.lookup(nameToken.lexeme);
if (!sym || !sym->isFunction) {
errors.emplace_back(nameToken.line, nameToken.column,
"Undeclared function '" + nameToken.lexeme + "'", "Semantic", "ERROR");
} else {
node->dataType = sym->dataType;
}

consume(TK_LPAREN, "Expected '(' after function name");

int argCount = 0;
if (!check(TK_RPAREN)) {
do {
ASTNode* arg = expression();
if (arg) {
node->addChild(arg);
argCount++;

// Type checking for arguments
if (sym && sym->isFunction && argCount <= sym->paramTypes.size()) {
if (arg->dataType != sym->paramTypes[argCount-1] &&
arg->dataType != UNKNOWN_TYPE &&
sym->paramTypes[argCount-1] != UNKNOWN_TYPE) {
errors.emplace_back(nameToken.line, nameToken.column,
"Type mismatch in argument " + to_string(argCount) +
" of function '" + nameToken.lexeme + "'",
"Semantic", "ERROR");
}
}
}
} while (match(TK_COMMA));
}

// Check argument count
if (sym && sym->isFunction) {
if (argCount != sym->paramTypes.size()) {
errors.emplace_back(nameToken.line, nameToken.column,
"Function '" + nameToken.lexeme + "' expects " +
to_string(sym->paramTypes.size()) + " arguments, got " +
to_string(argCount), "Semantic", "ERROR");
}
}

consume(TK_RPAREN, "Expected ')'");
return node;
}

public:
FastParser(const vector<Token>& t, SymbolTable& st)
: tokens(t), symTable(st), current(0), astRoot(nullptr),
hasMainFunction(false), hadError(false), recoveryMode(false) {}

ASTNode* parse() {
astRoot = program();

if (!hasMainFunction && astRoot && !astRoot->children.empty()) {
bool hasRealContent = false;
for (auto child : astRoot->children) {
if (child->type == "Function" ||
(child->type == "Block" && !child->children.empty()) ||
child->type != "Block") {
hasRealContent = true;
break;
}
}

if (hasRealContent) {
errors.emplace_back(1, 1,
"No main() function found - treating as script", "Semantic", "WARNING");
}
}

return astRoot;
}

vector<CompilerError> getErrors() {
auto symErrors = symTable.getErrors();
errors.insert(errors.end(), symErrors.begin(), symErrors.end());
return errors;
}

void displayAST() {
if (astRoot) {
cout << CYAN << "\nABSTRACT SYNTAX TREE:" << RESET << endl;
cout << "======================" << endl;
astRoot->display();
}
}
};

struct TACInstruction {
string op;
string arg1;
string arg2;
string result;

TACInstruction(string o = "", string a1 = "", string a2 = "", string r = "")
: op(move(o)), arg1(move(a1)), arg2(move(a2)), result(move(r)) {}

string toString() const {
if (op == "LABEL") return arg1 + ":";
if (op == "GOTO") return "goto " + arg1;
if (op == "IF") return "if " + arg1 + " goto " + arg2;
if (op == "IF_FALSE") return "if_false " + arg1 + " goto " + arg2;
if (op == "PRINT") return "print " + arg1;
if (op == "SCAN") return "scan " + arg1;
if (op == "RETURN") return "return " + arg1;
if (op == "CALL") return result + " = call " + arg1;
if (op == "PARAM") return "param " + arg1;
if (op == "FUNC") return "function " + arg1;

if (arg2.empty()) {
if (op == "=") return result + " = " + arg1;
return result + " = " + op + " " + arg1;
}
return result + " = " + arg1 + " " + op + " " + arg2;
}
};

class TACGenerator {
private:
vector<TACInstruction> instructions;
int tempCount;
int labelCount;
unordered_map<string, string> currentFunction;

public:
TACGenerator() : tempCount(1), labelCount(1) {}

string newTemp() { return "t" + to_string(tempCount++); }
string newLabel() { return "L" + to_string(labelCount++); }

void generateFromAST(ASTNode* node) {
if (!node) return;

if (node->type == "Program") {
bool hasMain = false;
bool hasContent = false;

for (auto child : node->children) {
if (child->type == "Function" && child->value == "main") {
hasMain = true;
}
if (child->type != "Block" || !child->children.empty()) {
hasContent = true;
}
if (child->type == "Function") hasContent = true;
}

if (!hasMain && hasContent) {
instructions.emplace_back("FUNC", "main");
for (auto child : node->children) {
generateStatement(child);
}
instructions.emplace_back("RETURN", "0");
} else {
for (auto child : node->children) {
if (child->type == "Function") {
generateFunction(child);
} else {
generateStatement(child);
}
}
}
}
}

void generateFunction(ASTNode* node) {
instructions.emplace_back("FUNC", node->value);
currentFunction[node->value] = node->value;

for (auto child : node->children) {
if (child->type == "Param") {
instructions.emplace_back("PARAM", child->value);
} else {
generateStatement(child);
}
}

if (node->value == "main") {
instructions.emplace_back("RETURN", "0");
}
currentFunction.clear();
}

void generateStatement(ASTNode* node) {
if (!node) return;

if (node->type == "Declaration") {
if (!node->children.empty()) {
string temp = generateExpression(node->children[0]);
instructions.emplace_back("=", temp, "", node->value);
}
}
else if (node->type == "Assignment") {
if (node->children.size() == 2) {
string rhsTemp = generateExpression(node->children[1]);
string lhs = node->children[0]->value;
instructions.emplace_back("=", rhsTemp, "", lhs);
}
}
else if (node->type == "Print" || node->type == "Printf") {
if (!node->children.empty()) {
for (size_t i = 0; i < node->children.size(); i++) {
string temp = generateExpression(node->children[i]);
instructions.emplace_back("PRINT", temp);
}
}
}
else if (node->type == "If") {
string elseLabel = newLabel();
string endLabel = newLabel();

// If condition
if (node->children.size() > 0) {
string condTemp = generateExpression(node->children[0]);
instructions.emplace_back("IF_FALSE", condTemp, elseLabel);
}

// Then block
if (node->children.size() > 1) {
generateStatement(node->children[1]);
}
instructions.emplace_back("GOTO", endLabel);
instructions.emplace_back("LABEL", elseLabel);

// Else/elseif blocks
for (size_t i = 2; i < node->children.size(); i++) {
if (i + 1 < node->children.size()) {
// elseif
string elseifCond = generateExpression(node->children[i]);
string nextLabel = newLabel();
instructions.emplace_back("IF_FALSE", elseifCond, nextLabel);
generateStatement(node->children[i + 1]);
instructions.emplace_back("GOTO", endLabel);
instructions.emplace_back("LABEL", nextLabel);
i++;
} else {
// else
generateStatement(node->children[i]);
}
}

instructions.emplace_back("LABEL", endLabel);
}
else if (node->type == "While") {
string startLabel = newLabel();
string endLabel = newLabel();

instructions.emplace_back("LABEL", startLabel);
if (node->children.size() > 0) {
string condTemp = generateExpression(node->children[0]);
instructions.emplace_back("IF_FALSE", condTemp, endLabel);
}
if (node->children.size() > 1) {
generateStatement(node->children[1]);
}
instructions.emplace_back("GOTO", startLabel);
instructions.emplace_back("LABEL", endLabel);
}
else if (node->type == "DoWhile") {
string startLabel = newLabel();
string condLabel = newLabel();

instructions.emplace_back("LABEL", startLabel);
if (node->children.size() > 0) {
generateStatement(node->children[0]);
}
instructions.emplace_back("LABEL", condLabel);
if (node->children.size() > 1) {
string condTemp = generateExpression(node->children[1]);
instructions.emplace_back("IF", condTemp, startLabel);
}
}
else if (node->type == "For") {
string startLabel = newLabel();
string incLabel = newLabel();
string endLabel = newLabel();

// Initialization
if (node->children.size() > 0 && node->children[0]->type != "Empty") {
generateStatement(node->children[0]);
}

instructions.emplace_back("LABEL", startLabel);

// Condition
if (node->children.size() > 1 && node->children[1]->type != "Empty") {
string condTemp = generateExpression(node->children[1]);
instructions.emplace_back("IF_FALSE", condTemp, endLabel);
}

// Body
if (node->children.size() > 3) {
generateStatement(node->children[3]);
}

// Increment
if (node->children.size() > 2 && node->children[2]->type != "Empty") {
generateExpression(node->children[2]);
}

instructions.emplace_back("GOTO", startLabel);
instructions.emplace_back("LABEL", endLabel);
}
else if (node->type == "Return") {
if (!node->children.empty()) {
string temp = generateExpression(node->children[0]);
instructions.emplace_back("RETURN", temp);
} else {
instructions.emplace_back("RETURN", "0");
}
}
else if (node->type == "FunctionCall") {
string args;
for (auto child : node->children) {
string temp = generateExpression(child);
if (!args.empty()) args += ", ";
args += temp;
}
string result = newTemp();
instructions.emplace_back("CALL", node->value + "(" + args + ")", "", result);
}
else if (node->type == "Break") {
instructions.emplace_back("GOTO", "break_target");
}
else if (node->type == "Continue") {
instructions.emplace_back("GOTO", "continue_target");
}
else if (node->type == "Block") {
for (auto child : node->children) {
generateStatement(child);
}
}
else if (node->type == "Empty") {
// Do nothing
}
}

string generateExpression(ASTNode* node) {
if (!node) return "t0";

if (node->type == "Identifier") {
string temp = newTemp();
instructions.emplace_back("=", node->value, "", temp);
return temp;
}
else if (node->type == "Literal" || node->type == "String") {
string temp = newTemp();
instructions.emplace_back("=", node->value, "", temp);
return temp;
}
else if (node->type == "Operator") {
string left = generateExpression(node->children[0]);
string right = generateExpression(node->children[1]);
string result = newTemp();
instructions.emplace_back(node->value, left, right, result);
return result;
}
else if (node->type == "Unary") {
string operand = generateExpression(node->children[0]);
string result = newTemp();
instructions.emplace_back(node->value, operand, "", result);
return result;
}
else if (node->type == "Postfix") {
string operand = node->children[0]->value;
string result = newTemp();
instructions.emplace_back("=", operand, "", result);
string incResult = newTemp();
instructions.emplace_back(node->value, operand, "1", incResult);
instructions.emplace_back("=", incResult, "", operand);
return result;
}
else if (node->type == "FunctionCall") {
string args;
for (auto child : node->children) {
string temp = generateExpression(child);
if (!args.empty()) args += ", ";
args += temp;
}
string result = newTemp();
instructions.emplace_back("CALL", node->value + "(" + args + ")", "", result);
return result;
}

return "t0";
}

void display() {
cout << CYAN << "\nTHREE ADDRESS CODE:" << RESET << endl;
cout << "====================" << endl;

if (instructions.empty()) {
cout << "No TAC generated" << endl;
return;
}

for (size_t i = 0; i < instructions.size(); i++) {
cout << setw(3) << (i+1) << ": " << instructions[i].toString() << endl;
}

cout << "\nTotal TAC instructions: " << instructions.size() << endl;
}

vector<TACInstruction> getInstructions() { return instructions; }
};

class CodeGenerator {
private:
ofstream outputFile;
string outputFilename;

public:
CodeGenerator(const string& filename) {
outputFilename = filename.substr(0, filename.find_last_of('.')) + ".cpp";
outputFile.open(outputFilename);
}

~CodeGenerator() {
if (outputFile.is_open()) outputFile.close();
}

void generateFromAST(ASTNode* node) {
if (!outputFile.is_open()) return;

outputFile << "#include <iostream>\n";
outputFile << "#include <string>\n";
outputFile << "#include <cstdio>\n";
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
}
}
}
}
}

void generateFunction(ASTNode* node) {
string returnType = getTypeString(node->dataType);
outputFile << returnType << " " << node->value << "(";

// Generate parameters
bool first = true;
for (auto child : node->children) {
if (child->type == "Param") {
if (!first) outputFile << ", ";
outputFile << getTypeString(child->dataType) << " " << child->value;
first = false;
}
}
outputFile << ") {\n";

for (auto child : node->children) {
if (child->type != "Param") {
generateStatement(child, 1);
}
}

if (node->value == "main" && node->dataType == INT_TYPE) {
outputFile << " return 0;\n";
} else if (node->dataType != VOID_TYPE && node->value != "main") {
// Add default return for non-void functions
outputFile << " return " << getDefaultValue(node->dataType) << ";\n";
}

outputFile << "}\n\n";
}

void generateStatement(ASTNode* node, int indent) {
string spaces(indent * 2, ' ');

if (node->type == "Declaration") {
outputFile << spaces << getTypeString(node->dataType) << " " << node->value;
if (!node->children.empty()) {
outputFile << " = ";
generateExpression(node->children[0], outputFile);
}
outputFile << ";\n";
}
else if (node->type == "Assignment") {
outputFile << spaces;
generateExpression(node->children[0], outputFile);
outputFile << " = ";
generateExpression(node->children[1], outputFile);
outputFile << ";\n";
}
else if (node->type == "Print" || node->type == "Printf") {
outputFile << spaces;
if (node->value == "printf" || node->type == "Printf") {
outputFile << "printf(";
if (!node->children.empty()) {
generateExpression(node->children[0], outputFile);
for (size_t i = 1; i < node->children.size(); i++) {
outputFile << ", ";
generateExpression(node->children[i], outputFile);
}
}
outputFile << ");\n";
} else {
outputFile << "cout << ";
if (!node->children.empty()) {
generateExpression(node->children[0], outputFile);
for (size_t i = 1; i < node->children.size(); i++) {
outputFile << " << ";
generateExpression(node->children[i], outputFile);
}
}
outputFile << ";\n";
}
}
else if (node->type == "If") {
outputFile << spaces << "if (";
if (node->children.size() > 0) {
generateExpression(node->children[0], outputFile);
}
outputFile << ") {\n";
if (node->children.size() > 1) {
generateStatement(node->children[1], indent + 1);
}
outputFile << spaces << "}\n";

for (size_t i = 2; i < node->children.size(); i++) {
if (i + 1 < node->children.size()) {
outputFile << spaces << "else if (";
generateExpression(node->children[i], outputFile);
outputFile << ") {\n";
generateStatement(node->children[i + 1], indent + 1);
outputFile << spaces << "}\n";
i++;
} else {
outputFile << spaces << "else {\n";
generateStatement(node->children[i], indent + 1);
outputFile << spaces << "}\n";
}
}
}
else if (node->type == "While") {
outputFile << spaces << "while (";
if (node->children.size() > 0) {
generateExpression(node->children[0], outputFile);
}
outputFile << ") {\n";
if (node->children.size() > 1) {
generateStatement(node->children[1], indent + 1);
}
outputFile << spaces << "}\n";
}
else if (node->type == "DoWhile") {
outputFile << spaces << "do {\n";
if (node->children.size() > 0) {
generateStatement(node->children[0], indent + 1);
}
outputFile << spaces << "} while (";
if (node->children.size() > 1) {
generateExpression(node->children[1], outputFile);
}
outputFile << ");\n";
}
else if (node->type == "For") {
outputFile << spaces << "for (";
if (node->children.size() > 0 && node->children[0]->type != "Empty") {
generateStatement(node->children[0], 0);
outputFile.seekp(-2, ios_base::cur);
}
outputFile << "; ";
if (node->children.size() > 1 && node->children[1]->type != "Empty") {
generateExpression(node->children[1], outputFile);
}
outputFile << "; ";
if (node->children.size() > 2 && node->children[2]->type != "Empty") {
generateExpression(node->children[2], outputFile);
}
outputFile << ") {\n";
if (node->children.size() > 3) {
generateStatement(node->children[3], indent + 1);
}
outputFile << spaces << "}\n";
}
else if (node->type == "Return") {
outputFile << spaces << "return";
if (!node->children.empty()) {
outputFile << " ";
generateExpression(node->children[0], outputFile);
}
outputFile << ";\n";
}
else if (node->type == "Block") {
outputFile << spaces << "{\n";
for (auto child : node->children) {
generateStatement(child, indent + 1);
}
outputFile << spaces << "}\n";
}
else if (node->type == "FunctionCall") {
outputFile << spaces << node->value << "(";
for (size_t i = 0; i < node->children.size(); i++) {
if (i > 0) outputFile << ", ";
generateExpression(node->children[i], outputFile);
}
outputFile << ");\n";
}
}

void generateExpression(ASTNode* node, ostream& out) {
if (!node) return;

if (node->type == "Identifier") {
out << node->value;
}
else if (node->type == "Literal" || node->type == "String") {
if (node->dataType == STRING_TYPE) {
out << "\"" << node->value << "\"";
} else {
out << node->value;
}
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
else if (node->type == "FunctionCall") {
out << node->value << "(";
for (size_t i = 0; i < node->children.size(); i++) {
if (i > 0) out << ", ";
generateExpression(node->children[i], out);
}
out << ")";
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

string getDefaultValue(DataType type) {
switch(type) {
case INT_TYPE: return "0";
case FLOAT_TYPE: return "0.0";
case CHAR_TYPE: return "'\\0'";
case BOOL_TYPE: return "false";
case STRING_TYPE: return "\"\"";
default: return "0";
}
}

bool compileAndExecute() {
outputFile.close();

string exeName = outputFilename.substr(0, outputFilename.find_last_of('.'));
string compileCmd = "g++ -std=c++11 -o " + exeName + " " + outputFilename + " 2>&1";

cout << CYAN << "Compiling: " << compileCmd << RESET << endl;

FILE* pipe = popen(compileCmd.c_str(), "r");
if (!pipe) return false;

char buffer[128];
string result;
while (fgets(buffer, sizeof(buffer), pipe)) {
result += buffer;
}
pclose(pipe);

if (!result.empty()) {
cout << RED << "Compilation errors:\n" << result << RESET << endl;
return false;
}

cout << GREEN << "✓ Compilation successful!" << RESET << endl;
cout << CYAN << "\nProgram output:" << RESET << endl;
cout << "--------------\n";

string execCmd = "./" + exeName;
system(execCmd.c_str());

return true;
}

string getOutputFilename() { return outputFilename; }
};

class OptimizedCompiler {
private:
string source;
string filename;
vector<CompilerError> allErrors;
vector<CompilerError> allWarnings;

public:
OptimizedCompiler(const string& src, const string& fname)
: source(src), filename(fname) {}

bool compile() {
auto totalStart = high_resolution_clock::now();
allErrors.clear();
allWarnings.clear();

cout << BOLD << CYAN << "\n╔══════════════════════════════════════════════════════════╗" << RESET << endl;
cout << BOLD << CYAN << "║ ULTRA-EFFICIENT C COMPILER ║" << RESET << endl;
cout << BOLD << CYAN << "║ Full compilation with TAC generation ║" << RESET << endl;
cout << BOLD << CYAN << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;

cout << "Source: " << filename << " (" << source.size() << " bytes)" << endl;

// PHASE 1: LEXICAL ANALYSIS
auto lexStart = high_resolution_clock::now();
cout << BOLD << YELLOW << "\n[1] LEXICAL ANALYSIS" << RESET << endl;
cout << YELLOW << "===================\n" << RESET << endl;

FastLexer lexer(source);
vector<Token> tokens = lexer.tokenize();
auto lexTime = duration_cast<microseconds>(high_resolution_clock::now() - lexStart);

for (auto& err : lexer.getErrors()) {
if (err.severity == "ERROR") allErrors.push_back(err);
else allWarnings.push_back(err);
}

if (!allErrors.empty()) {
cout << RED << "✗ Lexical errors: " << allErrors.size() << RESET << endl;
for (auto& err : lexer.getErrors()) {
err.display();
}
} else {
cout << GREEN << "✓ Lexical OK (" << lexTime.count() << " μs)" << RESET << endl;
}

lexer.displayTokens();

// Stop if lexical errors
if (!allErrors.empty()) {
goto summary;
}

// PHASE 2: SYNTAX ANALYSIS
auto parseStart = high_resolution_clock::now();
cout << BOLD << YELLOW << "\n[2] SYNTAX ANALYSIS" << RESET << endl;
cout << YELLOW << "==================\n" << RESET << endl;

SymbolTable symTable;
FastParser parser(tokens, symTable);
ASTNode* ast = parser.parse();
auto parseTime = duration_cast<microseconds>(high_resolution_clock::now() - parseStart);

for (auto& err : parser.getErrors()) {
if (err.severity == "ERROR") allErrors.push_back(err);
else allWarnings.push_back(err);
}

if (!allErrors.empty()) {
cout << RED << "✗ Syntax errors: " << allErrors.size() << RESET << endl;
for (auto& err : parser.getErrors()) {
if (err.severity == "ERROR") err.display();
}
} else {
cout << GREEN << "✓ Syntax OK (" << parseTime.count() << " μs)" << RESET << endl;
}

// Display warnings
for (auto& warn : allWarnings) {
warn.display();
}

symTable.display();
if (ast && allErrors.empty()) {
parser.displayAST();
}

// PHASE 3: TAC GENERATION
auto tacStart = high_resolution_clock::now();
cout << BOLD << YELLOW << "\n[3] TAC GENERATION" << RESET << endl;
cout << YELLOW << "==================\n" << RESET << endl;

TACGenerator tacGen;
if (allErrors.empty() && ast) {
tacGen.generateFromAST(ast);
tacGen.display();
auto tacTime = duration_cast<microseconds>(high_resolution_clock::now() - tacStart);
cout << GREEN << "✓ TAC generated (" << tacTime.count() << " μs)" << RESET << endl;
}

// PHASE 4: CODE GENERATION & EXECUTION
if (allErrors.empty() && ast) {
auto codegenStart = high_resolution_clock::now();
cout << BOLD << YELLOW << "\n[4] CODE GENERATION & EXECUTION" << RESET << endl;
cout << YELLOW << "===============================\n" << RESET << endl;

CodeGenerator codeGen(filename);
codeGen.generateFromAST(ast);

cout << GREEN << "✓ C++ code generated: " << codeGen.getOutputFilename() << RESET << endl;

cout << CYAN << "\nExecuting program..." << RESET << endl;
cout << CYAN << "-------------------\n" << RESET << endl;

if (codeGen.compileAndExecute()) {
cout << GREEN << "\n✓ Program executed successfully!" << RESET << endl;
} else {
cout << RED << "\n✗ Program execution failed" << RESET << endl;
}

auto codegenTime = duration_cast<milliseconds>(high_resolution_clock::now() - codegenStart);
cout << CYAN << "\nCode generation time: " << codegenTime.count() << " ms" << RESET << endl;
}

summary:
// SUMMARY
auto totalTime = duration_cast<milliseconds>(high_resolution_clock::now() - totalStart);

cout << BOLD << MAGENTA << "\n╔══════════════════════════════════════════════════════════╗" << RESET << endl;

if (allErrors.empty()) {
cout << BOLD << GREEN << "║ COMPILATION SUCCESSFUL ║" << RESET << endl;
} else {
cout << BOLD << RED << "║ COMPILATION FAILED - " << setw(3) << allErrors.size()
<< " ERRORS ║" << RESET << endl;
}

cout << BOLD << MAGENTA << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;

// Fast summary table
cout << CYAN << "+----------------------+---------------------+" << RESET << endl;
cout << CYAN << "| Metric | Value |" << endl;
cout << CYAN << "+----------------------+---------------------+" << RESET << endl;
cout << "| Source size | " << setw(19) << source.size() << " bytes |" << endl;
cout << "| Tokens | " << setw(19) << tokens.size() << " |" << endl;
cout << "| Lexical time | " << setw(19) << lexTime.count() << " μs |" << endl;
cout << "| Parse time | " << setw(19) << parseTime.count() << " μs |" << endl;
cout << "| Errors | " << setw(19) << allErrors.size() << " |" << endl;
cout << "| Warnings | " << setw(19) << allWarnings.size() << " |" << endl;
cout << "| Total time | " << setw(19) << totalTime.count() << " ms |" << endl;
cout << CYAN << "+----------------------+---------------------+\n" << RESET << endl;

if (ast) delete ast;
return allErrors.empty();
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
case 4: testErrorCases(); break;
case 5: return;
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
cout << BOLD << CYAN << "║ ULTRA-EFFICIENT C COMPILER - MAIN MENU ║" << RESET << endl;
cout << BOLD << CYAN << "╚══════════════════════════════════════════════════════════╝\n" << RESET << endl;
cout << " 1. Compile a file\n";
cout << " 2. Enter code interactively\n";
cout << " 3. Run sample programs\n";
cout << " 4. Test error cases\n";
cout << " 5. Exit\n";
cout << BOLD << "\nSelect option (1-5): " << RESET;
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

string source((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
file.close();

OptimizedCompiler compiler(source, filename);
compiler.compile();
}

void interactiveMode() {
cout << "\nEnter C code line by line (type END on a separate line to finish):\n" << endl;

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

OptimizedCompiler compiler(code, "interactive.c");
compiler.compile();
}

void runSamples() {
vector<string> samples = {
// Sample 1: Basic program
R"(int main() {
int x = 10;
int y = 20;
int sum = x + y;

printf("Sum: %d\n", sum);

for (int i = 0; i < 5; i++) {
printf("i = %d\n", i);
}

if (x > y) {
printf("x is greater\n");
} else {
printf("y is greater or equal\n");
}

return 0;
})",
// Sample 2: Without main()
R"(int a = 5;
int b = 10;

printf("a + b = %d\n", a + b);

if (a > b) {
printf("a is greater\n");
} else {
printf("b is greater\n");
})",
// Sample 3: Function with parameters
R"(int add(int x, int y) {
return x + y;
}

int main() {
int result = add(10, 20);
printf("Result: %d\n", result);
return 0;
})"
};

for (size_t i = 0; i < samples.size(); i++) {
cout << BOLD << "\nSAMPLE " << (i+1) << ":" << RESET << endl;
cout << CYAN << "===========" << RESET << endl;
cout << samples[i] << endl << endl;

OptimizedCompiler compiler(samples[i], "sample_" + to_string(i+1) + ".c");
compiler.compile();

if (i < samples.size() - 1) {
cout << YELLOW << "\nPress Enter for next sample..." << RESET;
cin.get();
}
}
}

void testErrorCases() {
vector<pair<string, string>> errorCases = {
{"Undeclared Variable",
"int main() {\n x = 10;\n return 0;\n}"},
{"Type Mismatch",
"int main() {\n int x = \"hello\";\n return 0;\n}"},
{"Invalid If Condition",
"int main() {\n int x = 5;\n if (x) {\n printf(\"OK\");\n }\n return 0;\n}"},
{"While Syntax Error",
"int main() {\n while {\n printf(\"Loop\");\n }\n return 0;\n}"},
{"Function Call Argument Error",
"void foo(int x) {}\nint main() {\n foo();\n return 0;\n}"},
{"Missing Return Statement",
"int main() {\n printf(\"Hello\");\n}"},
{"For Loop Update Error",
"int main() {\n for (int i = 0; i < 10; i+++) {\n printf(\"%d\", i);\n }\n return 0;\n}"},
{"Invalid Character",
"int main() {\n int x = 10@;\n return 0;\n}"}
};

for (size_t i = 0; i < errorCases.size(); i++) {
cout << BOLD << "\nERROR TEST " << (i+1) << ": " << errorCases[i].first << RESET << endl;
cout << RED << "===========" << RESET << endl;
cout << errorCases[i].second << endl << endl;

OptimizedCompiler compiler(errorCases[i].second, "error_test_" + to_string(i+1) + ".c");
compiler.compile();

if (i < errorCases.size() - 1) {
cout << YELLOW << "\nPress Enter for next error test..." << RESET;
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


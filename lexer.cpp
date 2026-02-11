#include "lexer.h"
#include "error_handler.h"
#include <cctype>
#include <iostream>

Lexer::Lexer(const std::string& src) 
    : source(src), position(0), line(1), column(1) {
    
    // Initialize keywords
    keywords = {
        {"int", TOK_INT},
        {"float", TOK_FLOAT},
        {"char", TOK_CHAR},
        {"void", TOK_VOID},
        {"if", TOK_IF},
        {"else", TOK_ELSE},
        {"while", TOK_WHILE},
        {"for", TOK_FOR},
        {"return", TOK_RETURN}
    };
}

char Lexer::peek() {
    if (position >= source.length()) return '\0';
    return source[position];
}

char Lexer::advance() {
    if (position >= source.length()) return '\0';
    
    char c = source[position++];
    if (c == '\n') {
        line++;
        column = 1;
    } else {
        column++;
    }
    return c;
}

void Lexer::skipWhitespace() {
    while (true) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance();
        } else if (c == '/') {
            if (position + 1 < source.length() && source[position + 1] == '/') {
                skipComment();
            } else {
                break;
            }
        } else {
            break;
        }
    }
}

void Lexer::skipComment() {
    while (peek() != '\n' && peek() != '\0') {
        advance();
    }
}

Token Lexer::scanNumber() {
    std::string value;
    bool isReal = false;
    int startCol = column;
    
    while (isdigit(peek())) {
        value += advance();
    }
    
    if (peek() == '.') {
        isReal = true;
        value += advance();
        
        while (isdigit(peek())) {
            value += advance();
        }
    }
    
    return isReal ? Token(TOK_REAL, value, line, startCol) 
                  : Token(TOK_NUMBER, value, line, startCol);
}

Token Lexer::scanIdentifier() {
    std::string ident;
    int startCol = column;
    
    while (isalnum(peek()) || peek() == '_') {
        ident += advance();
    }
    
    auto it = keywords.find(ident);
    if (it != keywords.end()) {
        return Token(it->second, ident, line, startCol);
    }
    
    return Token(TOK_IDENTIFIER, ident, line, startCol);
}

Token Lexer::scanCharacter() {
    std::string value;
    int startCol = column;
    
    advance();  // Skip opening quote
    
    if (peek() == '\\') {
        value += advance();  // Escape character
        value += advance();
    } else if (peek() != '\'') {
        value += advance();
    }
    
    if (peek() == '\'') {
        advance();  // Skip closing quote
        return Token(TOK_CHARACTER, value, line, startCol);
    } else {
        ErrorHandler::getInstance()->addError(
            LEXICAL_ERROR, "Unterminated character constant", line, startCol
        );
        return Token(TOK_ERROR, value, line, startCol);
    }
}

Token Lexer::scanString() {
    std::string value;
    int startCol = column;
    
    advance();  // Skip opening quote
    
    while (peek() != '"' && peek() != '\0') {
        if (peek() == '\\') {
            value += advance();  // Escape character
        }
        value += advance();
    }
    
    if (peek() == '"') {
        advance();  // Skip closing quote
        return Token(TOK_STRING, value, line, startCol);
    } else {
        ErrorHandler::getInstance()->addError(
            LEXICAL_ERROR, "Unterminated string literal", line, startCol
        );
        return Token(TOK_ERROR, value, line, startCol);
    }
}

Token Lexer::makeToken(TokenType type, const std::string& lexeme) {
    int startCol = column;
    if (!lexeme.empty()) {
        for (char c : lexeme) {
            if (c == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            position++;
        }
    }
    return Token(type, lexeme, line, startCol);
}

Token Lexer::getNextToken() {
    skipWhitespace();
    
    if (position >= source.length()) {
        return Token(TOK_EOF, "", line, column);
    }
    
    char current = peek();
    int startCol = column;
    
    // Identifiers
    if (isalpha(current) || current == '_') {
        return scanIdentifier();
    }
    
    // Numbers
    if (isdigit(current)) {
        return scanNumber();
    }
    
    // Characters
    if (current == '\'') {
        return scanCharacter();
    }
    
    // Strings
    if (current == '"') {
        return scanString();
    }
    
    // Operators and separators
    switch (current) {
        case ';': advance(); return Token(TOK_SEMICOLON, ";", line, startCol);
        case ',': advance(); return Token(TOK_COMMA, ",", line, startCol);
        case '(': advance(); return Token(TOK_LPAREN, "(", line, startCol);
        case ')': advance(); return Token(TOK_RPAREN, ")", line, startCol);
        case '{': advance(); return Token(TOK_LBRACE, "{", line, startCol);
        case '}': advance(); return Token(TOK_RBRACE, "}", line, startCol);
        case '[': advance(); return Token(TOK_LBRACKET, "[", line, startCol);
        case ']': advance(); return Token(TOK_RBRACKET, "]", line, startCol);
        
        case '+': advance(); return Token(TOK_PLUS, "+", line, startCol);
        case '-': advance(); return Token(TOK_MINUS, "-", line, startCol);
        case '*': advance(); return Token(TOK_MULTIPLY, "*", line, startCol);
        case '/': advance(); return Token(TOK_DIVIDE, "/", line, startCol);
        case '%': advance(); return Token(TOK_MOD, "%", line, startCol);
        
        case '=':
            advance();
            if (peek() == '=') {
                advance();
                return Token(TOK_EQUAL, "==", line, startCol);
            }
            return Token(TOK_ASSIGN, "=", line, startCol);
            
        case '!':
            advance();
            if (peek() == '=') {
                advance();
                return Token(TOK_NOT_EQUAL, "!=", line, startCol);
            }
            return Token(TOK_NOT, "!", line, startCol);
            
        case '<':
            advance();
            if (peek() == '=') {
                advance();
                return Token(TOK_LESS_EQUAL, "<=", line, startCol);
            }
            return Token(TOK_LESS, "<", line, startCol);
            
        case '>':
            advance();
            if (peek() == '=') {
                advance();
                return Token(TOK_GREATER_EQUAL, ">=", line, startCol);
            }
            return Token(TOK_GREATER, ">", line, startCol);
            
        case '&':
            advance();
            if (peek() == '&') {
                advance();
                return Token(TOK_AND, "&&", line, startCol);
            }
            break;
            
        case '|':
            advance();
            if (peek() == '|') {
                advance();
                return Token(TOK_OR, "||", line, startCol);
            }
            break;
    }
    
    // Unknown character
    std::string unknown(1, advance());
    ErrorHandler::getInstance()->addError(
        LEXICAL_ERROR, "Unknown character: " + unknown, line, startCol
    );
    return Token(TOK_ERROR, unknown, line, startCol);
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    Token token;
    
    do {
        token = getNextToken();
        tokens.push_back(token);
    } while (token.type != TOK_EOF && token.type != TOK_ERROR);
    
    return tokens;
}

std::string Lexer::tokenTypeToString(TokenType type) {
    switch (type) {
        case TOK_INT: return "int";
        case TOK_FLOAT: return "float";
        case TOK_CHAR: return "char";
        case TOK_VOID: return "void";
        case TOK_IF: return "if";
        case TOK_ELSE: return "else";
        case TOK_WHILE: return "while";
        case TOK_FOR: return "for";
        case TOK_RETURN: return "return";
        case TOK_IDENTIFIER: return "identifier";
        case TOK_NUMBER: return "number";
        case TOK_REAL: return "real";
        case TOK_CHARACTER: return "character";
        case TOK_STRING: return "string";
        case TOK_PLUS: return "+";
        case TOK_MINUS: return "-";
        case TOK_MULTIPLY: return "*";
        case TOK_DIVIDE: return "/";
        case TOK_MOD: return "%";
        case TOK_ASSIGN: return "=";
        case TOK_EQUAL: return "==";
        case TOK_NOT_EQUAL: return "!=";
        case TOK_LESS: return "<";
        case TOK_GREATER: return ">";
        case TOK_LESS_EQUAL: return "<=";
        case TOK_GREATER_EQUAL: return ">=";
        case TOK_AND: return "&&";
        case TOK_OR: return "||";
        case TOK_NOT: return "!";
        case TOK_SEMICOLON: return ";";
        case TOK_COMMA: return ",";
        case TOK_LPAREN: return "(";
        case TOK_RPAREN: return ")";
        case TOK_LBRACE: return "{";
        case TOK_RBRACE: return "}";
        case TOK_LBRACKET: return "[";
        case TOK_RBRACKET: return "]";
        case TOK_EOF: return "EOF";
        case TOK_ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}
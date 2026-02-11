#include "error_handler.h"
#include <iomanip>

ErrorHandler* ErrorHandler::instance = nullptr;

ErrorHandler* ErrorHandler::getInstance() {
    if (!instance) {
        instance = new ErrorHandler();
    }
    return instance;
}

void ErrorHandler::addError(ErrorType type, const std::string& message, int line, int column) {
    errors.emplace_back(type, message, line, column);
}

void ErrorHandler::clearErrors() {
    errors.clear();
}

bool ErrorHandler::hasErrors() const {
    return !errors.empty();
}

void ErrorHandler::printErrors() const {
    if (errors.empty()) {
        std::cout << "No errors found.\n";
        return;
    }
    
    std::cout << "\n=== COMPILATION ERRORS ===\n";
    for (const auto& error : errors) {
        std::cout << std::setw(10) << getErrorTypeString(error.type) 
                  << " at line " << error.line 
                  << ", column " << error.column 
                  << ": " << error.message << "\n";
    }
    std::cout << "=========================\n";
}

const std::vector<Error>& ErrorHandler::getErrors() const {
    return errors;
}

std::string ErrorHandler::getErrorTypeString(ErrorType type) const {
    switch (type) {
        case LEXICAL_ERROR: return "LEXICAL";
        case SYNTAX_ERROR: return "SYNTAX";
        case SEMANTIC_ERROR: return "SEMANTIC";
        case TYPE_MISMATCH: return "TYPE_MISMATCH";
        case UNDECLARED_VARIABLE: return "UNDECLARED";
        case REDECLARATION: return "REDECLARATION";
        case INVALID_OPERATION: return "INVALID_OP";
        default: return "UNKNOWN";
    }
}
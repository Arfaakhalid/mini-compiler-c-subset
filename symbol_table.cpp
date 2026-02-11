#include "symbol_table.h"
#include <iostream>
#include <iomanip>

SymbolTable::SymbolTable() : currentScope(0) {
    tables.push_back(std::unordered_map<std::string, std::shared_ptr<Symbol>>());
}

void SymbolTable::enterScope() {
    currentScope++;
    tables.push_back(std::unordered_map<std::string, std::shared_ptr<Symbol>>());
}

void SymbolTable::exitScope() {
    if (currentScope > 0) {
        tables.pop_back();
        currentScope--;
    }
}

bool SymbolTable::addSymbol(const std::string& name, SymbolType type, DataType dataType, int line) {
    if (lookupCurrentScope(name)) {
        return false;  // Redeclaration in same scope
    }
    
    auto symbol = std::make_shared<Symbol>(name, type, dataType, currentScope, line);
    tables[currentScope][name] = symbol;
    return true;
}

bool SymbolTable::addArray(const std::string& name, DataType dataType, int size, int line) {
    if (lookupCurrentScope(name)) {
        return false;
    }
    
    auto symbol = std::make_shared<Symbol>(name, SYM_ARRAY, dataType, currentScope, line);
    symbol->arraySize = size;
    tables[currentScope][name] = symbol;
    return true;
}

bool SymbolTable::addFunction(const std::string& name, DataType returnType, 
                             const std::vector<DataType>& params, int line) {
    if (lookupCurrentScope(name)) {
        return false;
    }
    
    auto symbol = std::make_shared<Symbol>(name, SYM_FUNCTION, returnType, currentScope, line);
    symbol->paramTypes = params;
    tables[currentScope][name] = symbol;
    return true;
}

std::shared_ptr<Symbol> SymbolTable::lookup(const std::string& name) {
    for (int i = currentScope; i >= 0; i--) {
        auto it = tables[i].find(name);
        if (it != tables[i].end()) {
            return it->second;
        }
    }
    return nullptr;
}

std::shared_ptr<Symbol> SymbolTable::lookupCurrentScope(const std::string& name) {
    auto it = tables[currentScope].find(name);
    if (it != tables[currentScope].end()) {
        return it->second;
    }
    return nullptr;
}

void SymbolTable::printTable() const {
    std::cout << "\n=== SYMBOL TABLE ===\n";
    std::cout << std::setw(5) << "Scope" 
              << std::setw(15) << "Name" 
              << std::setw(15) << "Type" 
              << std::setw(15) << "Data Type" 
              << std::setw(10) << "Size"
              << std::setw(10) << "Line\n";
    
    for (int i = 0; i <= currentScope; i++) {
        for (const auto& pair : tables[i]) {
            const auto& sym = pair.second;
            std::cout << std::setw(5) << i
                      << std::setw(15) << sym->name
                      << std::setw(15) << getSymbolTypeString(sym->symType)
                      << std::setw(15) << getTypeString(sym->dataType)
                      << std::setw(10) << (sym->symType == SYM_ARRAY ? std::to_string(sym->arraySize) : "-")
                      << std::setw(10) << sym->lineDeclared << "\n";
        }
    }
    std::cout << "===================\n";
}

int SymbolTable::getCurrentScope() const {
    return currentScope;
}

std::string SymbolTable::getTypeString(DataType type) const {
    switch (type) {
        case TYPE_INT: return "int";
        case TYPE_FLOAT: return "float";
        case TYPE_CHAR: return "char";
        case TYPE_VOID: return "void";
        default: return "unknown";
    }
}

std::string SymbolTable::getSymbolTypeString(SymbolType type) const {
    switch (type) {
        case SYM_VARIABLE: return "variable";
        case SYM_FUNCTION: return "function";
        case SYM_ARRAY: return "array";
        case SYM_PARAMETER: return "parameter";
        default: return "unknown";
    }
}
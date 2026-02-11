#include "tac_generator.h"
#include <iostream>
#include <iomanip>

void TACInstruction::print() const {
    std::cout << std::setw(10) << result 
              << std::setw(5) << "=" 
              << std::setw(10) << arg1
              << std::setw(5) << op
              << std::setw(10) << arg2 << "\n";
}

TACGenerator::TACGenerator() : tempCounter(0), labelCounter(0) {}

std::string TACGenerator::newTemp() {
    return "t" + std::to_string(tempCounter++);
}

std::string TACGenerator::newLabel() {
    return "L" + std::to_string(labelCounter++);
}

std::vector<TACInstruction> TACGenerator::generate(std::shared_ptr<ProgramNode> ast) {
    instructions.clear();
    tempCounter = 0;
    labelCounter = 0;
    
    if (ast) {
        for (const auto& decl : ast->declarations) {
            generateDeclaration(decl);
        }
    }
    
    return instructions;
}

void TACGenerator::generateDeclaration(std::shared_ptr<ASTNode> node) {
    if (!node) return;
    
    if (node->type == ASTNodeType::VARIABLE_DECL) {
        auto varDecl = std::dynamic_pointer_cast<VariableDeclNode>(node);
        if (varDecl && varDecl->initialValue) {
            std::string temp = generateExpression(varDecl->initialValue);
            instructions.emplace_back("=", temp, "", varDecl->name);
        }
    }
}

std::string TACGenerator::generateExpression(std::shared_ptr<ASTNode> node) {
    if (!node) return "";
    
    if (node->type == ASTNodeType::BINARY_EXPR) {
        auto binExpr = std::dynamic_pointer_cast<BinaryExprNode>(node);
        if (binExpr) {
            std::string left = generateExpression(binExpr->left);
            std::string right = generateExpression(binExpr->right);
            std::string temp = newTemp();
            
            std::string op;
            switch (binExpr->op) {
                case TOK_PLUS: op = "+"; break;
                case TOK_MINUS: op = "-"; break;
                case TOK_MULTIPLY: op = "*"; break;
                case TOK_DIVIDE: op = "/"; break;
                case TOK_EQUAL: op = "=="; break;
                case TOK_NOT_EQUAL: op = "!="; break;
                case TOK_LESS: op = "<"; break;
                case TOK_GREATER: op = ">"; break;
                case TOK_LESS_EQUAL: op = "<="; break;
                case TOK_GREATER_EQUAL: op = ">="; break;
                case TOK_AND: op = "&&"; break;
                case TOK_OR: op = "||"; break;
                default: op = "?";
            }
            
            instructions.emplace_back(op, left, right, temp);
            return temp;
        }
    }
    else if (node->type == ASTNodeType::VARIABLE_DECL) {
        auto varNode = std::dynamic_pointer_cast<VariableDeclNode>(node);
        if (varNode) {
            return varNode->name;
        }
    }
    
    return "";
}

void TACGenerator::printTAC() const {
    std::cout << "\n=== THREE-ADDRESS CODE ===\n";
    std::cout << std::setw(10) << "Result" 
              << std::setw(5) << "" 
              << std::setw(10) << "Arg1"
              << std::setw(5) << "Op"
              << std::setw(10) << "Arg2" << "\n";
    std::cout << std::string(40, '-') << "\n";
    
    for (const auto& instr : instructions) {
        instr.print();
    }
    std::cout << "==========================\n";
}

const std::vector<TACInstruction>& TACGenerator::getInstructions() const {
    return instructions;
}
# Mini Compiler: C-Like Subset in C++
It's for a simplified version of C – think basic types, loops, functions, and I/O – but we built it all from the ground up in C++ on Visual Studio Code using the WSL Linux extension. No cheating with Flex or Bison; everything's manual to really get how compilers tick. It's a solid project to learn the ropes, but it handles real code: parses it, checks for errors, generates intermediate code, and even runs the output. Great for anyone curious about compilers, whether you're a dev or just starting out.
Keywords for the curious (or search engines): C compiler project, compiler construction in C++, lexical analyzer, syntax parser, semantic analysis, symbol table management, three-address code generation, error handling in compilers, recursive descent parsing.
Licensed under MIT.

Menu-Driven Interface: Fire it up, and you get a clean menu to compile files, type code on the fly, or test samples. No command-line hassle.
Phased Compilation: Shows each step – lexical (tokens table), syntax (parse tree), semantic (symbol table with init/used flags), TAC (intermediate instructions), code gen (makes output.cpp, compiles, runs).
Error Smarts: Catches lexical (bad chars), syntax (missing ;), semantic (wrong types). Uses panic-mode recovery – skips junk to report more errors. Warnings for unused vars.
Interactive and File-Based: Write code live or load .c files like test_valid.c (works) or test_invalid.c (errors out gracefully).
Performance Perks: Times each phase in microseconds, color-coded output for readability, generates reports (tokens.txt, errors.txt, etc.).
Execution: For valid code, it runs and shows output right there – e.g., print(z) actually prints the value.

It's advanced enough to feel like a real compiler (full pipeline to execution), but simple: just a tutorial-style project we scratched together in VS Code on WSL to understand basics.
Setup and Run (Step-by-Step)
You'll need g++ (C++11 or later). I built/tested on Ubuntu via WSL in VS Code – super straightforward.

Clone the repo: git clone https://github.com/arfaakhalid/mini-compiler-c-subset.git
Hop in: cd mini-compiler-c-subset
Compile: g++ -std=c++11 -o mini_compiler src/*.cpp
Run: ./mini_compiler

##  Screenshots

| ---------------------- Preview ----------------------------- |
|--------------------------------------------------------------|
 ![](Screenshot%20(624).png)
 ![](Screenshot%20(625).png) 
 ![](Screenshot%20(626).png)
 ![](Screenshot%20(627).png)
![](Screenshot%20(628).png) 
![](Screenshot%20(629).png)
![](Screenshot%20(630).png)
![](Screenshot%20(631).png)
![](Screenshot%20(632).png) 

|--------------------------------------------------------------|


Supports:
- Types: int, float, char, void, bool, string
- Vars, functions, params, returns
- Ops: + - * / % < > && || ! ++ --
- Controls: if/else/elseif, while/do-while, for, switch/case/default
- I/O: printf, scanf, print
- Preproc: basic #include, #define
- Errors: lexical/syntax/semantic with recovery (panic mode, skip to ; or })
- Extras: Color output, timing per phase, interactive menu

## Grammar Snippet (EBNF-ish)
<program> → <declaration>* <function>*  
<declaration> → <type> <id> ('[' <num> ']')? ('=' <expr>)? ';'  
<type> → int | float | char | void  
<function> → <type> <id> '(' <params>? ')' '{' <stmts>* '}'  
(Full rules in the report PDF – precedence via recursion.)

## Setup & Run
Need g++ (11+). Tested on Linux, should work on Windows with MinGW.

1. Clone: `git clone https://github.com/yourusername/mini-compiler-c-subset.git`  
2. cd in: `cd mini-compiler-c-subset`  
3. Build: `g++ -std=c++11 -o mini_compiler src/*.cpp`  
4. Fire it up: `./mini_compiler`

Menu lets you compile files (.c or .txt), type code interactively, or run samples.

Example valid code:
int x, y, z;
x = 8;
y = 4;
z = x + y * (x - y);
print(z);

Spits out 36. For crap code (undeclared var, type screwup), it reports errors but keeps going.

Outputs: tokens.txt, parse_tree.txt, tac.txt, symbols.txt, errors.txt, output.cpp (which it compiles and runs).

## Tests
Check /tests/ for valid/invalid examples. Valid: arith, functions, arrays. Invalid: redeclares, missing semis, type fails. All caught, under 100ms compile time.

## Folder Setup
- src/: lexer.cpp, parser.cpp, semantic.cpp, symboltable.cpp, tac.cpp, codegen.cpp, main.cpp
- tests/: valid/ and invalid/ with .c files and expected outs
- docs/: Mini_Compiler_Report.pdf (full deets)
- README.md, LICENSE, .gitignore

## Wanna Contribute?
Fork it, branch like `fix/lexer-bug`, commit, PR. Stick to C++11, add tests, update docs. Bugs? Open an issue.

## License
MIT – free to use/modify. Copyright 2026 Arfa Khalid.
Questions? shanerumman4@gmail.com. This was built from scratch to learn, not copied from tutorials – pure sweat on VS Code WSL.

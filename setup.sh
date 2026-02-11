#!/bin/bash

echo "Setting up C-Compiler project structure..."

# Create directories
mkdir -p src include output

echo "Directories created."

# Create test file
cat > test.c << 'TESTEOF'
#include <stdio.h>

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    int num = 5;
    int result = factorial(num);
    printf("Factorial of %d is %d\n", num, result);
    
    // Array example
    int arr[5] = {1, 2, 3, 4, 5};
    int sum = 0;
    
    for (int i = 0; i < 5; i++) {
        sum += arr[i];
    }
    
    printf("Sum of array: %d\n", sum);
    
    if (sum > 10) {
        printf("Sum is greater than 10\n");
    } else {
        printf("Sum is 10 or less\n");
    }
    
    return 0;
}
TESTEOF

echo "Test file created: test.c"
echo "Setup complete!"
echo ""
echo "To compile the compiler: make"
echo "To run the compiler: ./ccompiler test.c"
echo "To run interactive mode: ./ccompiler"

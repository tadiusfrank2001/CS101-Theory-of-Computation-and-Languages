# CS101-Theory-of-Computation-and-Languages


The course on the theory of computation involves coding in Haskell to investigate models of computation such as finite-state automata, Turing machines, and pushdown automata. It explores formal languages, including context-free grammars, and delves into key topics in computability, such as the halting problem and decidability. The course also examines the connections to real-world applications such as lexical analysis, parsing, writing interpreters, and compiling, bridging theoretical concepts with practical implementation. Through hands-on programming assignments and theoretical analysis, the course emphasizes the deep relationship between theory and practice in computation.

## Technologies Used

`Languages`: Haskell, Bash Shell

## Projects 


### 1. Arithmetic Expression Evaluator & Stack-Based Interpreter

This Haskell module provides:

Arithmetic Expression Evaluation – Supports unary `(-)` and binary `(+, -, *, /, ^)` operations on integer expressions.
Stack-Based Interpreter – Evaluates a list of operations `(Push, Add, Mult, Sub, Div, Swap)` using a stack-based approach.

1a. Arithmetic Expression Evaluation
Expression Types:

+`AST_NUM Int` – Numeric literals
+`AST_UNARY UOp ArithExp` – Unary operations
+`AST_BINARY ArithExp BOp ArithExp` – Binary operations

Supported Operations:
+Unary: `NEG (-x)`
+Binary: `ADD (+)`, `MINUS (-)`, `MUL (*)`, `DIV (/)`, `EXP (^)`

1b. Stack Interpreter

Stack Operations:

+`Push Float` – Pushes a number onto the stack
+`Add` – Pops two values and pushes their sum
+`Sub` – Pops two values and pushes their difference
+`Mult` – Pops two values and pushes their product
+`Div` – Pops two values and pushes their quotient
+`Swap` – Swaps the top two elements on the stack


### 2. PCF Interpreter with Let and Recursion

file: `PCFEnvInterpLetMonadStarter.hs`

This Haskell program implements an interpreter for PCF (Programming Computable Functions) with support for let bindings, recursion, and basic arithmetic operations. It evaluates PCF expressions using an environment-based approach with closures and thunks for function application and recursion. It supports:

+ Arithmetic Operations: `succ`, `pred`, `isZero`
+ Boolean Logic: `if-then-else` expressions
+ Function Definition & Application: First-class functions with closures
+ Let Bindings: Local variable assignments
+ Recursion: Implemented using thunks for recursive function calls


### 3. Type Checker

file: `TypeCheckerLetMonadStarter.hs`

This Haskell program implements an interpreter which includes a type checker to ensure expressions are well-typed before evaluation. It supports:

+ Basic types: `TYPE_INT`, `TYPE_BOOL`
+ Function types: `TYPE_FUN`
+ Recursive types: `AST_REC` for recursive function checking
+ Conditional expressions: `AST_IF` for type-safe `if-then-else` statements
+ Let-bindings with types: `ASF_IF` for scoped variable type checking

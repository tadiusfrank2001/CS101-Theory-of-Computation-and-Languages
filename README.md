# CS101-Theory-of-Computation-and-Languages


The course on the theory of computation involves coding in Haskell to investigate models of computation such as finite-state automata, Turing machines, and pushdown automata. It explores formal languages, including context-free grammars, and delves into key topics in computability, such as the halting problem and decidability. The course also examines the connections to real-world applications such as lexical analysis, parsing, writing interpreters, and compiling, bridging theoretical concepts with practical implementation. Through hands-on programming assignments and theoretical analysis, the course emphasizes the deep relationship between theory and practice in computation.

## Technologies Used

`Languages`: Haskell, Bash Shell

## Projects

### PCF Interpreter with Let and Recursion

file: `PCFEnvInterpLetMonadStarter.hs`

This Haskell program implements an interpreter for PCF (Programming Computable Functions) with support for let bindings, recursion, and basic arithmetic operations. It evaluates PCF expressions using an environment-based approach with closures and thunks for function application and recursion. It supports:

+ Arithmetic Operations: `succ`, `pred`, `isZero`
+ Boolean Logic: `if-then-else` expressions
+ Function Definition & Application: First-class functions with closures
+ Let Bindings: Local variable assignments
+ Recursion: Implemented using thunks for recursive function calls


### Type Checker

file: `TypeCheckerLetMonadStarter.hs`

This Haskell program implements an interpreter which includes a type checker to ensure expressions are well-typed before evaluation. It supports:

+ Basic types: `TYPE_INT`, `TYPE_BOOL`
+ Function types: `TYPE_FUN`
+ Recursive types: `AST_REC` for recursive function checking
+ Conditional expressions: `AST_IF` for type-safe `if-then-else` statements
+ Let-bindings with types: `ASF_IF` for scoped variable type checking

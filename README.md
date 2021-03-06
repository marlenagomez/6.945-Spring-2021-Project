# 6.945-Spring-2021-Project
## Program Refactoring
### Gabrielle Ecanow, Katherine Liew, Marlena Gomez

Scheme Program Refactoring, our final project for MIT 6.945: Large-Scale Symbolic Systems. 

We wanted to design a system which could re-factor/compress programs by identifying repeated code portions in the program and abstracting it into modular, reusable functions. The identified functions are given a name by the user, based on the principle that “giving something a name gives you power over it.” The growing library of reusable functions can then support program compression. This sort of compression of programs is primarily helpful in large 100+ page systems, where concision and understandability increase in importance and it becomes harder for a single user to identify where code is repeated.

We broke program refactoring into four main subproblems: common subexpression elimination, implementation of the library, library subexpression matching, and compression/refactoring via replacement. At a high level, the common subexpression eliminator (CSE) identifies repeated subexpressions within a block of inputted code. The library accepts an expression (in the form of a list to be matched against) and a corresponding name, which is then defined in the Scheme environment. The library matcher identifies subexpressions of code that are stored in the library (and can hence be compressed). Finally, the replacer does the job of replacing repeated expressions with named functions to compress large blocks of inputted code.

###  Function Library

The function library is a library of “memoized” procedures that have a name. The function library supports multiple procedures, such:

(add-to library name proc) Adds procedure proc to the library with name name. name is now defined to be proc in the Scheme working environment. The proc parameter must be a list formatted to be matched against, and can contain variables to match to (or not).

(find-in library proc) Attempts to find a procedure stored in the library that matches proc. If one is found, a procedure call with the appropriate parameters is returned; otherwise, an empty list is returned.

#### function-library/library-utils.scm
Holds utilities that support the library useful for any underlying implementation of the library itself.

#### function-library/basic-function-library.scm
Implements the library as an association list (alist) mapping expanded functions to the pair (defined function name .  parameter list).

#### function-library/trie-function-library.scm
In order to optimize the search for a matching procedure, an alternative implementation of the library isas a trie. The edges of the trie are predicates that test for matching the elements that make up procedures.

#### function-library/trie-matcher.scm
A custom trie matcher compiler, for use in matching to the library when implemented as a trie.

#### library.scm
Holds generic handlers for interfacing with the library. It also supports a *redundant* library implementation which takes advange of the power offered by degenerate implementations of the library. 

### Program-Library Matching

#### library-matcher.scm
The library matcher searches for segments within inputted Scheme code that already exist in the function library. It first “chunks” the inputted code recursively, such that every list (i.e. every possible standalone procedure nested in the larger procedure) can be matched against the library. The library matcher then compiles the list of pairs (original-procedure . library-condensed-procedure). The library returned condensed procedures are formatted with the correct operands from the original procedure as well. Futhermore, the library-matcher supports a compress procedure which actually compresses the inputted code.

#### library-matcher-tests.scm
Test cases for the library matcher.

### Common Subexpression Elimination

#### cse.scm
The common subexpression eliminator identifies instances of identical algebraic expressions and replaces them with a single variable. Any identified common subexpressions in the analyzed code are added to the function library. The implementation uses Professor Sussman’s gjs-cselim.scm code.

#### global-cse.scm
The global cse is a WIP that identifies instances of *similar* expressions to use for compression. The global cse uses the matching functionality of the function library to support finding similar subexpression that can be abstracted out into named procedures that take in the differences as parameters. 

### Testing

#### run.scm
Holds procedures for reading in code from a file and writing out code to a file, as well as importing all the necessary dependencies for compressing code using the gjs/cse, global cse, or library matching. See the tests directory for examples.

### Additional Replacement

#### match-replace.scm
The matcher takes an entry from the function library which is known to be within the inputted Scheme code being analyzed. 
The apply-run-matcher function then tries to match the function’s name, unknowns and body using run-matcher (from SDF), checking each possible sub-expression starting from largest to smallest. 
If unknowns are successfully identified with run-matcher, resolved-unknowns are returned in the format (unknown-variable resolved-value ?)
Apply-run-matcher will substitute a new list for the sub-expression to match the function described by the function library, ie. (list ‘define (list lib-function-name (map (lambda (x) (car x)) resolved-unknowns))). The list is created using the helper function resolved-function.
If run-matcher returns #f, there is no match to the sub-expression and apply-run-matcher keeps the original.



# Dana Compiler
## Εισαγωγικά

Ο compiler είναι για την γλώσσα Dana (λεπτομέρειες [εδώ](https://courses.softlab.ntua.gr/compilers/2025a/dana2025.pdf)). Ο Compiler γράφτηκε σε
C με αρκετές επιρροές από τεχνικές Data Oriented Design (tagged union, struct of 
arrays) και functional languges (Zig, Rust).

## Γενικές παρατηρήσεις

- **tagged union**: όλες οι δομές αξιοποιούν tagged union για πολυμορφισμό. Για 
  αποφυγή code bloat, πολλαπλά tags χρησιμοποιούν ίδιο union field (βλ.  
  `ast_node_t`)
- **pointers**: οι pointers χρησιμοποιούνται μόνο σε περιπτώσεις ownwership ή 
  για pass by reference.
- **indices**: οποιοδήποτε άλλο access γίνεται με την χρήση indices στους 
  εκάστοτε πίνακες. Τα indices είναι μέσα σε κατάλληλο struct για type safety.
- **getters**: οι πίνακες προορίζονται για πρόσβαση μέσω getter functions, έτσι 
  ώστε να υπάρχει bounds checking

## Lexer

Ο lexer είναι γραμμένος στο χέρι και δε χρησιμοποιεί State Machine. Κατασκευάζει 
tokens που περιέχουν την αρχή τους· για να βρεις το τέλος κάνει tokenize δεύτερη 
φορά.

Για την διαφοροποίηση keyword / operator από identifier, αξιοποιείται ένα symbol 
hash table.

### Δομές

- `struct lexer_t`{.c}:
    - `buffer`: buffer με ολόκληρο το αρχείο
    - `alloc`: allocator που χρησιμοποιήθηκε για τον `buffer`
    - `fname`: όνομα αρχείου προς tokenization
- `LEXER_CLEANUP`: `lexer_t` με αυτόματο destruction
-  `struct lex_token_t`{.c}:
    - `type`: tag του token
    - `pos`: index στο `lexer_t::buffer` όπου ξεκινάει

### Συναρτήσεις

- `lexer_create`
- `lexer_destroy`
- `lex_get_type_str(tok)`: επιστρέφει το string που περιγράφει το tag του `tok`
- `lex_next_token(lex, prev)`: επιστρέφει το επόμενο token μετά το `prev` στον 
  `lex`
- `lex_get_token(lex, tok)`: επιστρέφει το slice του `lex::buffer` που 
  αντιστοιχεί στο `tok`

## Parser

Ο parser είναι υπεύθυνος για την κατασκευή του Abstract Syntax Tree. Κατά την 
διάρκεια, κατασκευάζει το text area. Δέχεται τον `lexer` προς χρήση στην 
αρχικοποίση.

### Δομές

- `struct parser_t`{.c}:
    - `lexer`: ο lexer προς χρήση
    - `tokens`: dynamic array με τα tokens που κατασκευάζει ο lexer. Δεν 
      χρησιμοποιούνται μετά την κατασκευή του AST
    - `last`: reference στο τελευταίο token που έχει καταναλώσει ο parser. 
      Επιτρέπει peeking
    - `ast`: dynamic array με τα AST nodes
    - `extra`: dynamic array με AST node indices. Αξιοποιούνται από node tags 
      που έχουν μεταβλητό πλήθος παραμέτρων
    - `text`: dynamic array για την αντιγραφή των string literals από τον 
      `lexer::bufer`. Επιτρέπει την διαγραφή του lexer πριν προχωρήσουμε σε 
      semantic analysis
    - `names`: Hash masp που αντιστοιχεί ID σε string, επιτρέπει χρήση ονομάτων 
      για το debugging των identifiers. Δεν χρειάζεται σε production
- `struct ast_node_t`{.c}:
    - `type`: tag του node (αντίστοιχα types μεταξύ `lex_token_t` και 
      `ast_node_t` έχουν ίδιο value)
    - `union`{.c}:
        - `pl_data`: αξιά σε περίπτωση literal node (αντιγραμμένη από τον lexer)
        - `op_data`: unary / binary operator. Περιέχει 2 δείκτες σε άλλα nodes
        - `name_data`: περιέχει 1 identifier και 1 δείκτη σε node (body σε named 
          statement κ.λπ.)
        - `extra_data`: slice από το `parser_t::extra` που αντιστοιχεί στον 
          κόμβο
        - `var_data`: πληροφορία για variable definition.

### Αντιστοίχηση types με union fields:
```
AST_ARGS         extra_data : argument values (expressions)
AST_ARR_*        var_data.name : array ID         var_data.dim : διαστάσεις
                 var_data.next ?: επόμενο var
AST_ARR_AT       op_data.lhs : array lvalue       op_data.rhs : index
AST_ASSIGN       op_data.lhs : lvalue             op_data.rhs : rvalue
AST_BIN_AND      op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_BIN_NOT      op_data.lhs : operand
AST_BIN_OR,      op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_BLOCK        extra_data : σύνολο των εντολών
AST_BLOCK_SIMPLE op_data.lhs : μοναδική εντολή του block
AST_BOOL         pl_data.num : value
AST_BOOL_AND     op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_BOOL_NOT     op_data.lhs : operand
AST_BOOL_OR      op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_BREAK        name_data.name ?: target ID
AST_BYTE         var_data.name : variable ID      var_data.next ?: επόμενο var
AST_CHAR         pl_data.char : value
AST_CMP_*        op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_COND         extra_data : ζεύγη μορφής [condition, block]. Αν περιττού
                 μήκους, το τελευταίο είναι else block
AST_COND_SIMPLE  op_data.lhs : condition          op_data.rhs : block
AST_CONT         name_data.name ?: target ID
AST_DECL_*       name_data.data : function ID     name_data.body : arg-list
AST_DEF_*        op_data.lhs : function decl      op_data.rhs : local defs
AST_DIV          op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_EXIT
AST_FUNC         name_data.name : function ID     name_data.body : argument list
AST_INT          var_data.name : variable ID      var_data.next ?: επόμενο var
AST_LOCAL_DEF    extra_data : σύνολο από func-decl, func-def, var-def. Τελευταίο
                 στοιχείο είναι function implementation
AST_LOOP         name_data.name ?: loop ID        name_data.body : block
AST_MINUS        op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_MINUS        op_data.lhs : operand
AST_MULT         op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_NAME         pl_data.name : ID
AST_NUMBER       pl_data.num : value
AST_PLUS         op_data.lhs : LHS operand        op_data.rhs : RHS operand
AST_PROC         name_data.name : function ID     name_data.body : argument list
AST_REF_*        var_data.name : refernce ID      var_data.next ?: επόμενο var
AST_RETURN       op_data.lhs : return value
AST_SKIP
AST_STRING       pl_data.str : position στο parser_t::text
ST_MOD           op_data.lhs : LHS operand        op_data.rhs : RHS operand

```

#pragma once

#ifdef CGEN_IMPLEMENT
    #define DA_IMPLEMENT
    #define STACK_IMPLEMENT
    #define STR_IMPLEMENT
#endif

#include "parser.h"
#include "util.h"
#include "util/dynamic_array.h"
#include "util/stack.h"
#include "util/string.h"

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Utils.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

#pragma region TYPES

typedef struct {
    LLVMTypeRef type;
    LLVMValueRef alloca;
    LLVMTypeRef ptr_type;
    LLVMTypeRef element_type;
} cgen_var_t;

typedef struct { uint16_t key; cgen_var_t value; } cg_cell_t;
typedef cg_cell_t  *cg_scope_t;
typedef struct {cg_scope_t *scopes;} cg_table_t;

typedef struct {
    LLVMValueRef function;
    LLVMBasicBlockRef current_block;
    int block_counter;
} BlockContext;

typedef struct {
    int label;
    LLVMBasicBlockRef LoopBlock;
    LLVMBasicBlockRef AfterBlock;
} LoopInfo;

typedef struct {
    LLVMModuleRef Module;
    LLVMBuilderRef IRBuilder;
    LLVMContextRef Context;

    LLVMTypeRef i1, i8, i32, i64;

    cg_table_t cg_st;
    string_list scope;

    BlockContext *block_stack;
    LoopInfo *loop_stack;

    enum termination {NO_TERM = 0, CG_RET, CG_BREAK, CG_CONT} block_term;
    int loop_to_check;

} cgen_t;


#pragma endregion

#pragma region DEBUGGING
#ifdef CGEN_DEBUG
	#define log_c(fmt,...) \
		printf("CGEN: " fmt "\n" __VA_OPT__(,) __VA_ARGS__)
#else
	#define log_c(...)
#endif
#pragma endregion

#pragma region SYMBOL TABLE DECLARATIONS
#define CG_CLEANUP __attribute__((cleanup(cg_table_destroy)))
extern cg_table_t cg_table_create(void);
extern void        cg_table_destroy(cg_table_t *this);
#define cg_emplace_symbol(this, name, decl...) \
	cg_put_symbol(this, name, (cgen_var_t{ decl }))
extern cgen_var_t cg_get_symbol(const cg_table_t *this, uint16_t name);
extern void        cg_pop_scope(cg_table_t *this);
extern void        cg_push_scope(cg_table_t *this);
extern bool        cg_put_symbol(cg_table_t *this, uint16_t name, cgen_var_t info);
extern bool        cg_scope_has(const cg_table_t *this, uint16_t name);
#pragma endregion

#pragma region CODEGEN DECLARATIONS

extern LLVMValueRef c8(char c);
extern LLVMValueRef c32(int32_t i);

#define CGEN_CLEANUP __attribute__((cleanup(cgen_destroy)))
extern void cgen_create(cgen_t *cgen);
extern void cgen_destroy(cgen_t *cgen);

extern void cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);
extern LLVMValueRef _cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);

extern cgen_var_t _cgen_get_var_types(cgen_t *cgen, const parser_t *parser, ast_node_pos pos, bool is_arg);
extern cgen_var_t _cgen_get_array_type(cgen_t *cgen, const parser_t *parser, ast_node_t node, dtype_t type, uint32_t dim);
extern LLVMValueRef _cgen_get_var_value(cgen_t *cgen, const parser_t *parser, ast_node_pos pos, bool is_lvalue);
extern LLVMValueRef _cgen_get_array_value(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);
extern LLVMValueRef _cgen_get_array_at_ptr(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);
extern LLVMTypeRef _cgen_get_array_base_type(cgen_t *cgen, int name); 


extern slice_char_t _get_current_function(cgen_t *cgen);
extern slice_char_t _cgen_push_function(cgen_t *cgen, uint32_t name);
extern slice_char_t _cgen_pop_function(cgen_t *cgen);
extern slice_char_t _cgen_push_function_name(cgen_t *cgen, slice_char_t name);

extern void block_push(BlockContext **h, LLVMValueRef f, LLVMBasicBlockRef b, int c );
extern void loop_push (LoopInfo **h, LLVMBasicBlockRef loop, LLVMBasicBlockRef after, int label);
extern LoopInfo find_loop(LoopInfo **h, int label);

extern void cgen_verify_module(cgen_t *cgen);

#pragma endregion

#ifdef CGEN_IMPLEMENT
#pragma region STACKS IMPLEMENTATIONS
/*Symbol Table*/
cg_table_t
cg_table_create (void)
{
	cg_table_t ret = {0};
	stack_push(ret.scopes, (cg_scope_t){0});
	return ret;
}

void
cg_table_destroy (cg_table_t *this)
{
	stack_free(this->scopes);
}

cgen_var_t
cg_get_symbol (const cg_table_t *this, uint16_t name)
{
    ptrdiff_t idx;
	for (cg_scope_t *u = this->scopes; u; u = stack_next(u))
		if ((idx = hm_geti(*u, name)) != -1)
			return (*u)[idx].value;
	return (cgen_var_t){0};
}

inline void
cg_pop_scope (cg_table_t *this)
{
	stack_pop(this->scopes);
}

inline void
cg_push_scope (cg_table_t *this)
{
	stack_push(this->scopes, (cg_scope_t){0});
}

inline bool
cg_put_symbol (cg_table_t *this, uint16_t name, cgen_var_t info)
{
	if (cg_scope_has(this, name)) return true;
	hm_put(stack_top(this->scopes), name, info);
	return false;
}

inline bool
cg_scope_has (const cg_table_t *this, uint16_t name)
{   
    return hm_geti(stack_top(this->scopes), name) != -1;
}

/*Function Scopes*/
inline slice_char_t 
_get_current_function(cgen_t *cgen) {
    return cgen->scope.first ? cgen->scope.last->str : StrLit("");
}

slice_char_t 
_cgen_push_function(cgen_t *cgen, uint32_t name) {
    slice_char_t temp = _get_current_function(cgen);
    slice_char_t new_scope = str_append(temp, str_from_number(name), .sep = StrLit("_"));
    string_node* new_node = malloc(sizeof(string_node));
    *new_node = (string_node){ .str = new_scope, .next = NULL, .prev = NULL };
    ListPushBack(cgen->scope, new_node);
    return new_scope;
}

slice_char_t 
_cgen_pop_function(cgen_t *cgen) {
    slice_char_t temp = _get_current_function(cgen);
    ListPopBack(cgen->scope);
    return temp;
}

slice_char_t 
_cgen_push_function_name(cgen_t *cgen, slice_char_t name) {
    string_node* new_node = malloc(sizeof(string_node));
    *new_node = (string_node){ .str = name, .next = NULL, .prev = NULL };
    ListPushBack(cgen->scope, new_node);
    return name;
}

/*Blocks*/
inline void
block_push (BlockContext **h, LLVMValueRef f, LLVMBasicBlockRef b, int c ) {
    BlockContext new_block = { .function = f, .current_block = b, .block_counter = c};
    stack_push(*h, new_block);
    log_c("Pushed block with counter %d", stack_top(*h).block_counter);
}

/*Loop Stack*/
inline void
loop_push (LoopInfo **h, LLVMBasicBlockRef loop, LLVMBasicBlockRef after, int label) {
    LoopInfo new_loop = {.label = label, .AfterBlock = after, .LoopBlock = loop};
    stack_push(*h, new_loop);
}

LoopInfo
find_loop (LoopInfo **h, int label) {
    LoopInfo* loop_stack_temp = *h;
    LoopInfo loop = stack_top(*h);

    while(loop.label != label && !stack_empty(loop_stack_temp)) {
        loop_stack_temp = stack_next(loop_stack_temp);
        loop = stack_top(loop_stack_temp);
    }

    if(stack_empty(loop_stack_temp)) {
        throw(LoopInfo, "Label of loop not found");
    } else {
        return loop;
    }
}

#pragma endregion


#pragma region CODEGEN IMPLEMENTATIONS

#define c8(c) LLVMConstInt(LLVMInt8TypeInContext(cgen->Context), (uint64_t)(c), false)
#define c32(i) LLVMConstInt(LLVMInt32TypeInContext(cgen->Context), (uint64_t)(i), false)

void cgen_create(cgen_t *cgen) {
    log_c("INITIALIZING CODEGEN");
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    log_c("Creating context and module");
    cgen->Context = LLVMContextCreate();
    cgen->Module = LLVMModuleCreateWithNameInContext("my_module", cgen->Context);
    cgen->IRBuilder = LLVMCreateBuilderInContext(cgen->Context);

    cgen->i1 = LLVMInt1TypeInContext(cgen->Context);
    cgen->i8 = LLVMInt8TypeInContext(cgen->Context);
    cgen->i32 = LLVMInt32TypeInContext(cgen->Context);
    cgen->i64 = LLVMInt64TypeInContext(cgen->Context);

    log_c("Creating symbol table");
    cg_table_t CG_CLEANUP temp = cg_table_create();
    cgen->cg_st = temp;
    log_c("Creating global scope");
    cgen->scope = (string_list){0};
    stack_push(cgen->block_stack, (BlockContext){0});
    stack_push(cgen->loop_stack, (LoopInfo){0});

    cgen->block_term = NO_TERM;
    cgen->loop_to_check = 0;
}

void cgen_destroy(cgen_t *cgen) {
    LLVMDisposeBuilder(cgen->IRBuilder);
    LLVMDisposeModule(cgen->Module);
    LLVMContextDispose(cgen->Context);  

    cg_table_destroy(&(cgen->cg_st));
    //string_list_free(&cgen->scope);
    stack_free(cgen->block_stack);
    stack_free(cgen->loop_stack);
}

void cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos) {
    log_c("BEGIN CODEGEN");

    log_c("Type of root node: %s", ast_get_type_str(node_at(parser, pos)));

    assert(node_at(parser, pos).type == AST_DEF_PROC);

    LLVMTypeRef main_func_type = LLVMFunctionType(cgen->i32, NULL, 0, false);
    LLVMValueRef main_func = LLVMAddFunction(cgen->Module, "main", main_func_type);
    LLVMSetFunctionCallConv(main_func, LLVMCCallConv);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cgen->Context, main_func, "entry");
    LLVMPositionBuilderAtEnd(cgen->IRBuilder, entry);
    block_push(&(cgen->block_stack), main_func, entry, 0);
    _cgen_push_function_name(cgen, StrLit("main"));
    cg_push_scope(&(cgen->cg_st));

    _cgen_generate_code(cgen, parser, pos);

    log_c("Finalizing main function");

    while(stack_top(cgen->block_stack).block_counter > 0) {
        stack_pop(cgen->block_stack);
    }

    log_c("Reached correct block");
    LLVMPositionBuilderAtEnd(cgen->IRBuilder, stack_top(cgen->block_stack).current_block);

    log_c("Changed block");
    ast_node_t node = node_at(parser, pos);

    log_c("Getting initial function");

    slice_char_t func_name =  str_append(_get_current_function(cgen), str_from_number(node.name), .sep = StrLit("_"));
    log_c("Initial function name: %.*s", UNSLICE(func_name));
    LLVMValueRef func = LLVMGetNamedFunction(cgen->Module, func_name.ptr);
    if(!func) {
        log_c("Function not found!");
        exit(1);
    }
    LLVMTypeRef func_type = LLVMGlobalGetValueType(func);
    log_c("Calling initial function");
    LLVMBuildCall2(cgen->IRBuilder, func_type, func, NULL, 0, "");

    log_c("Building return");

    LLVMBuildRet(cgen->IRBuilder, c32(0));

    cgen_verify_module(cgen);


    char *error = NULL;
    if (LLVMPrintModuleToFile(cgen->Module, "output.ll", &error) != 0) {
        fprintf(stderr, "Error writing module to file: %s\n", error);
        LLVMDisposeMessage(error);
    }

    log_c("END CODEGEN");
}

LLVMValueRef _cgen_generate_code(Unused cgen_t *cgen, Unused const parser_t *parser, Unused ast_node_pos pos){
    log_c("Generating code...");

    if(cgen->block_term) {
        log_c("Block already has return, skipping code generation");
        return NULL;
    }

    LLVMBasicBlockRef tmp = LLVMGetInsertBlock(cgen->IRBuilder);
    log_c("I am at block %s", LLVMGetBasicBlockName(tmp));

    #define GENERATE_CHLD(temp) for (; ast_is_child(it); it = ast_next_child(it)) \
	temp = _cgen_generate_code(cgen, parser, it.pos)

	ast_node_t node = node_at(parser, pos);
	ast_node_it it = ast_get_child(parser, pos);
	LLVMValueRef lhs, rhs;

	log_c("checking %s at " PAR_FSTR,
			ast_get_type_str(node), PAR_FPOS(parser, node));
	switch (node.type) {
    /*literals*/
    case AST_TRUE: 
        log_c("Generating true");
        return c8('\x01');
    break; case AST_FALSE:
        log_c("Generating false");
        return c8('\x00');
    break; case AST_NUMBER:
        log_c("Generating number %d", node.pl_data.num);
        return c32(node.pl_data.num);
    break; case AST_CHAR:
        log_c("Generating char '%c'", node.pl_data.ch);
        return c8(node.pl_data.ch);
    break; case AST_STRING:
        log_c("Generating string");
        return LLVMBuildGlobalStringPtr(cgen->IRBuilder, par_get_text(parser, node.pl_data.str), "str");
    break; case AST_NAME: 
        log_c("Generating name %.*s", UNSLICE(par_get_name(parser, node)));
        return _cgen_get_var_value(cgen, parser, pos, false);
    /*expressions*/
    break; case AST_PLUS:
        log_c("Generating plus");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        if(ast_is_child(it)) {
            rhs = _cgen_generate_code(cgen, parser, it.pos);
            return LLVMBuildAdd(cgen->IRBuilder, lhs, rhs, "addtmp");
        } else {
            return lhs;
        }
    break; case AST_MINUS:
        log_c("Generating minus");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        if(ast_is_child(it)) {
            rhs = _cgen_generate_code(cgen, parser, it.pos);
            return LLVMBuildSub(cgen->IRBuilder, lhs, rhs, "subtmp");
        } else {
            return LLVMBuildNeg(cgen->IRBuilder, lhs, "negtmp");
        }
    break; case AST_MULT:
        log_c("Generating mult");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildMul(cgen->IRBuilder, lhs, rhs, "multmp");
    break; case AST_DIV:
        log_c("Generating div");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildSDiv(cgen->IRBuilder, lhs, rhs, "divtmp");
    break; case AST_MOD:
        log_c("Generating mod");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildSRem(cgen->IRBuilder, lhs, rhs, "modtmp");
    break; case AST_BIT_AND:
        log_c("Generating bitwise and");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildAnd(cgen->IRBuilder, lhs, rhs, "andtmp");
    break; case AST_BIT_OR:
        log_c("Generating bitwise or");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildOr(cgen->IRBuilder, lhs, rhs, "ortmp");
    break; case AST_BIT_NOT:
        log_c("Generating bitwise not");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildNot(cgen->IRBuilder, lhs, "nottmp");
    break; case AST_BOOL_AND:
        log_c("Generating boolean and");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildAnd(cgen->IRBuilder, lhs, rhs, "andtmp");
    break; case AST_BOOL_OR:
        log_c("Generating boolean or");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildOr(cgen->IRBuilder, lhs, rhs, "ortmp");
    break; case AST_BOOL_NOT:
        log_c("Generating boolean not");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildNot(cgen->IRBuilder, lhs, "nottmp");
    break; case AST_CMP_EQ:
        log_c("Generating cmp eq");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntEQ, lhs, rhs, "eqtmp");
    break; case AST_CMP_NEQ:
        log_c("Generating cmp neq");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntNE, lhs, rhs, "neqtmp");
    break; case AST_CMP_LEQ:
        log_c("Generating cmp leq");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntSLE, lhs, rhs, "leqtmp");
    break; case AST_CMP_LT:
        log_c("Generating cmp lt");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntSLT, lhs, rhs, "lttmp");
    break; case AST_CMP_GEQ:
        log_c("Generating cmp geq");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntSGE, lhs, rhs, "geqtmp");
    break; case AST_CMP_GT:
        log_c("Generating cmp gt");
        lhs = _cgen_generate_code(cgen, parser, it.pos);
        it = ast_next_child(it);
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        return LLVMBuildICmp(cgen->IRBuilder, LLVMIntSGT, lhs, rhs, "gttmp");
    break; case AST_ARRAY_AT:
        log_c("Generating array at");
        return _cgen_get_var_value(cgen, parser, pos, false);
    break; case AST_FUNC_CALL: case AST_PROC_CALL:
        {
        log_c("Generating function/procedure call");
        slice_char_t func_name =  str_append(_get_current_function(cgen), str_from_number(node.name), .sep = StrLit("_"));
        LLVMValueRef func = LLVMGetNamedFunction(cgen->Module, func_name.ptr);
        str_destroy(&func_name);
        throw_if(!func, LLVMValueRef, PAR_FSTR "function %.*s not declared", PAR_FPOS(parser, node), UNSLICE(func_name));
        LLVMTypeRef func_type = LLVMGlobalGetValueType(func);
        LLVMValueRef arg;
        LLVMValueRef* args;
        LLVMTypeRef* arg_types;
        arr_init(args);
        arr_init(arg_types);

        size_t arg_count = LLVMCountParams(func);
        arr_setcap(arg_types, arg_count);
        LLVMGetParamTypes(func_type, arg_types);

        for (size_t i=0; ast_is_child(it) && i<arg_count; it = ast_next_child(it), i++) {
            if(LLVMGetTypeKind(arg_types[i]) == LLVMPointerTypeKind)
                arg = try(_cgen_get_var_value(cgen, parser, it.pos, true), "while calling args");
            else
                arg = try(_cgen_generate_code(cgen, parser, it.pos), "while calling args");
            arr_push(args, arg);
        }
        slice_char_t call_name = node.type == AST_FUNC_CALL ? StrLit("calltmp") : StrLit("");
        LLVMValueRef call = LLVMBuildCall2(cgen->IRBuilder, func_type, func, args, arr_len(args), call_name.ptr);
        arr_free(args);
        return call;
        }
    /* scope-creating nodes */
    break; case AST_BLOCK: case AST_INDENT:
        {
        log_c("Generating block/indent %u: %u:\t", PAR_FPOS(parser, node));
        LLVMValueRef temp;
        GENERATE_CHLD(temp);
        return temp;
        }
    break; case AST_DECL_PROC ... AST_DECL_BYTE:
           case AST_DEF_PROC ... AST_DEF_BYTE:
        {
        log_c("Generating function declaration/definition %s", ast_get_type_str(node));

        LLVMTypeRef* arg_types;
        arr_init(arg_types);
        log_c("getting arguments of function");
        for (; POS_CMP(it.pos, par_func_locals(parser, pos)) < 0;
				it = ast_next_child(it)) {
			arr_push(arg_types, _cgen_get_var_types(cgen, parser, it.pos, true).type);
        } 
        log_c("Getting return type of function");
        LLVMTypeRef ret_type;
        if(node.type == AST_DECL_PROC || node.type == AST_DEF_PROC)
            ret_type = LLVMVoidTypeInContext(cgen->Context);
        else if (node.type == AST_DECL_INT || node.type == AST_DEF_INT)
            ret_type = cgen->i32;
        else
            ret_type = cgen->i8;

        log_c("Creating function type");
        LLVMTypeRef func_type = LLVMFunctionType(ret_type, arg_types, arr_len(arg_types), false);

        log_c("Creating function name");
        slice_char_t func_name =  str_append(_get_current_function(cgen), str_from_number(node.name), .sep = StrLit("_"));

        log_c("Checking if function %.*s already exists", UNSLICE(func_name));
        LLVMValueRef test_func = LLVMGetNamedFunction(cgen->Module, func_name.ptr);

        if(!test_func) {
            log_c("Function %.*s does not exist, creating it", UNSLICE(func_name));
            test_func = LLVMAddFunction(cgen->Module, func_name.ptr, func_type);
            size_t leng;
            const char* func_name_cstr = LLVMGetValueName2(test_func, &leng);
            log_c("Created function %.*s", (int)leng, func_name_cstr);
            LLVMSetFunctionCallConv(test_func, LLVMCCallConv);
            log_c("Adding function to the symbol table");
            cg_put_symbol(&(cgen->cg_st), node.name, (cgen_var_t){ .type = func_type, .alloca = test_func, .element_type = ret_type, .ptr_type=NULL});
        }

        log_c("Function is done.");
        if(node.type <= AST_DECL_BYTE && node.type >= AST_DECL_PROC) {
            log_c("Function is only a declaration, returning.");
            arr_free(arg_types);
            return test_func;
        }
        log_c("Function is a definition, generating body.");
        cg_push_scope(&(cgen->cg_st));
        log_c("Pushing scope name %.*s", UNSLICE(func_name));
        _cgen_push_function_name(cgen, func_name);

        LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cgen->Context, test_func, "entry");
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, entry);

        int block_counter = stack_top(cgen->block_stack).block_counter;
        block_push(&(cgen->block_stack), test_func, entry, block_counter + 1);
        log_c("Pushed new block with counter %d", stack_top(cgen->block_stack).block_counter);
        log_c("Pushed block stack");

        it = ast_get_child(parser, pos);

        for (int k = 0; POS_CMP(it.pos, par_func_locals(parser, pos)) < 0;
				it = ast_next_child(it), k++) {
			LLVMValueRef param = LLVMGetParam(test_func, k);
            cgen_var_t types = (cgen_var_t){0};
            types.type = arg_types[k];
            switch(it.node->type) {
                break; case AST_ARRAY: 
                    types = _cgen_get_var_types(cgen, parser, it.pos, true);
                    types.alloca = param;
                break; case AST_REF_BYTE: 
                    types.alloca = param;
                    types.type = cgen->i8;
                case AST_REF_INT: 
                    types.alloca = param;
                    types.type = cgen->i32;
                break; case AST_INT: case AST_BYTE:
                    types.alloca = LLVMBuildAlloca(cgen->IRBuilder, types.type, "arg");
                    LLVMBuildStore(cgen->IRBuilder, param, types.alloca);
                break; default:
                    throw(LLVMValueRef, PAR_FSTR "Invalid argument type", PAR_FPOS(parser, *it.node));
            }

            throw_if(cg_put_symbol(&(cgen->cg_st), it.node->name, types), LLVMValueRef,
                    PAR_FSTR "redeclaration of variable %.*s",
                    PAR_FPOS(parser, *it.node),
                    UNSLICE(par_get_name(parser, *it.node))
                    );
		} 
        arr_free(arg_types);

        for (; POS_CMP(it.pos, par_func_body(parser, pos)) < 0;
				it = ast_next_child(it)) {
                    switch(it.node->type) {
                        break; case AST_DECL_PROC ... AST_DECL_BYTE:
                               case AST_DEF_PROC ... AST_DEF_BYTE:
                            _cgen_generate_code(cgen, parser, it.pos);
                        break; case AST_INT: case AST_BYTE: case AST_ARRAY:
                            {                
                            cgen_var_t types = _cgen_get_var_types(cgen, parser, it.pos, false);
                            types.alloca = LLVMBuildAlloca(cgen->IRBuilder, types.type, "local_def");
                            throw_if(cg_put_symbol(&(cgen->cg_st), it.node->name, types), LLVMValueRef,
                                PAR_FSTR "redeclaration of variable %.*s",
                                PAR_FPOS(parser, *it.node),
                                UNSLICE(par_get_name(parser, *it.node))
                                );
                            }
                        break; default:
                            throw(LLVMValueRef, PAR_FSTR "Only variable and function declarations allowed before function body", PAR_FPOS(parser, *it.node));
                    }
        }

        _cgen_generate_code(cgen, parser, it.pos);
        
        cgen->block_term = NO_TERM;
        cgen->loop_to_check = 0;

        LLVMVerifyFunction(test_func, LLVMPrintMessageAction);

        _cgen_pop_function(cgen);
        log_c("Popped name scope");
        cg_pop_scope(&(cgen->cg_st));
        log_c("Popped symbol scope");
        Unused BlockContext temp = *(cgen->block_stack);
        log_c("Popping block stack with counter %d", temp.block_counter);
        log_c("Poping block named %s", LLVMGetBasicBlockName(temp.current_block));
        stack_pop(cgen->block_stack);
        log_c("Popped block stack");
        BlockContext curr_block = *(cgen->block_stack);
        log_c("Got current block");
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, curr_block.current_block);
        log_c("Finished function");

        return test_func;

    }
    /* statements */
    break; case AST_SKIP:
        return NULL;
    break; case AST_EXIT:
        log_c("Generating exit");
        cgen->block_term = CG_RET;
        return LLVMBuildRetVoid(cgen->IRBuilder);
    break; case AST_BREAK: case AST_CONT: 
    {   
        log_c("Generating break");
        LoopInfo loop; 
        if(node.name) {
            loop = try_typed(find_loop(&(cgen->loop_stack), node.name), LLVMValueRef, PAR_FSTR "while generating break", PAR_FPOS(parser, node));
        } else {
            loop = stack_top(cgen->loop_stack);
        }

        LLVMBuildBr(cgen->IRBuilder, node.type == AST_BREAK ? loop.AfterBlock : loop.LoopBlock);
        cgen->block_term = node.type == AST_BREAK ? CG_BREAK: CG_CONT;
        return NULL;
    }
    break; case AST_LOOP:
        {
        log_c("Generating Loop");
        LLVMBasicBlockRef current_block = LLVMGetInsertBlock(cgen->IRBuilder);
        LLVMValueRef current_function = LLVMGetBasicBlockParent(current_block);

        LLVMBasicBlockRef loop_block = LLVMAppendBasicBlockInContext(cgen->Context, current_function, "loop");
        LLVMBasicBlockRef after_block = LLVMAppendBasicBlockInContext(cgen->Context, current_function, "after");

        loop_push(&(cgen->loop_stack), loop_block, after_block, node.name ? node.name : 0);
        LLVMBuildBr(cgen->IRBuilder, loop_block);
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, loop_block);
        cgen->block_stack->current_block = loop_block;

        log_c("I am at loop_block");

        LLVMValueRef ret = _cgen_generate_code(cgen, parser, it.pos);
        if(!cgen->block_term) LLVMBuildBr(cgen->IRBuilder, loop_block);

        LLVMPositionBuilderAtEnd(cgen->IRBuilder, after_block);
        cgen->block_stack->current_block = after_block;
        
        switch(cgen->block_term){
            break; case(CG_BREAK): case(CG_CONT):
            {   
                if(!cgen->loop_to_check || (node.name && cgen->loop_to_check == node.name)) {
                    cgen->block_term = NO_TERM;
                    cgen->loop_to_check = 0;
                } else {
                    LoopInfo loop = try_typed(find_loop(&(cgen->loop_stack), cgen->loop_to_check), LLVMValueRef, PAR_FSTR "while generating break", PAR_FPOS(parser, node));

                    LLVMBuildBr(cgen->IRBuilder, loop.AfterBlock);                    
                }
            }
            break; case(CG_RET): 
            {   
                LLVMTypeRef ret_type = LLVMGetReturnType(
                                            LLVMGlobalGetValueType(
                                                LLVMGetBasicBlockParent(
                                                    LLVMGetInsertBlock(cgen->IRBuilder)
                                                )
                                            )
                                        );
                switch ( LLVMGetTypeKind(ret_type)) 
                {
                    break; case(LLVMIntegerTypeKind): 
                    {
                        unsigned bits = LLVMGetIntTypeWidth(ret_type);
                        if(bits <= 8) LLVMBuildRet(cgen->IRBuilder, c8(1));
                        else LLVMBuildRet(cgen->IRBuilder, c32(1));
                    }
                    break; case(LLVMVoidTypeKind):
                        LLVMBuildRetVoid(cgen->IRBuilder);
                    break; default:
                        throw(LLVMValueRef, "Unrecognised function return type");
                }
            }
            break; case(NO_TERM): break;
        }

        stack_pop(cgen->loop_stack);
        return ret;
        }
    break; case AST_ASSIGN:
        log_c("Generating assignment");
        assert(ast_is_child(it));
        lhs = _cgen_get_var_value(cgen, parser, it.pos, true);
        it = ast_next_child(it);
        assert(ast_is_child(it));
        rhs = _cgen_generate_code(cgen, parser, it.pos);
        LLVMBuildStore(cgen->IRBuilder, rhs, lhs);
        return rhs;
    break; case AST_RETURN:
        {
        log_c("Generating return");
        assert(ast_is_child(it));
        LLVMValueRef ret_val = _cgen_generate_code(cgen, parser, it.pos);
        LLVMBuildRet(cgen->IRBuilder, ret_val);
        cgen->block_term = CG_RET;
        return ret_val;
        }
    break; case AST_COND:
        {
        log_c("Generating if condition");
        assert(ast_is_child(it));
        
        LLVMValueRef func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(cgen->IRBuilder));
        LLVMBasicBlockRef cond_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "cond");
        LLVMBasicBlockRef then_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "then");
        LLVMBasicBlockRef else_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "else(_if)");
        LLVMBasicBlockRef after_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "after_if");

        LLVMBuildBr(cgen->IRBuilder, cond_block);
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, cond_block);
        cgen->block_stack->current_block = cond_block;
        
        LLVMValueRef ret = _cgen_generate_code(cgen, parser, it.pos);
        LLVMBuildCondBr(cgen->IRBuilder, ret, then_block, else_block);

        it = ast_next_child(it);
        assert(ast_is_child(it));

        LLVMPositionBuilderAtEnd(cgen->IRBuilder, then_block);
        cgen->block_stack->current_block = then_block;
        _cgen_generate_code(cgen, parser, it.pos);
        
        if(!cgen->block_term) {
            LLVMBuildBr(cgen->IRBuilder, after_block);
        } else{
            cgen->block_term = NO_TERM;
            cgen->loop_to_check = 0;
        }

        it = ast_next_child(it);
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, else_block);
        cgen->block_stack->current_block = else_block;
        for (; ast_is_child(it); it = ast_next_child(it)) {
			if (ast_is_child(ast_next_child(it))) {
				ret = _cgen_generate_code(cgen, parser, it.pos);
                then_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "then");
                else_block = LLVMAppendBasicBlockInContext(cgen->Context, func, "else(_if)");
                LLVMBuildCondBr(cgen->IRBuilder, ret, then_block, else_block);
                it = ast_next_child(it);

                LLVMPositionBuilderAtEnd(cgen->IRBuilder, then_block);
                cgen->block_stack->current_block = then_block;
                _cgen_generate_code(cgen, parser, it.pos);
                if(!cgen->block_term) {
                    LLVMBuildBr(cgen->IRBuilder, after_block);
                } else{
                    cgen->block_term = NO_TERM;
                    cgen->loop_to_check = 0;
                }
                
                LLVMPositionBuilderAtEnd(cgen->IRBuilder, else_block);
                cgen->block_stack->current_block = else_block;

			} else {
				_cgen_generate_code(cgen, parser, it.pos);
                if(!cgen->block_term) {
                    LLVMBuildBr(cgen->IRBuilder, after_block);
                } else{
                    cgen->block_term = NO_TERM;
                    cgen->loop_to_check = 0;
                }
			}

        }

        if(!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(cgen->IRBuilder))) LLVMBuildBr(cgen->IRBuilder, after_block);
        LLVMPositionBuilderAtEnd(cgen->IRBuilder, after_block);
        cgen->block_stack->current_block = after_block;

        return NULL;
        }
    break; default:
        log_c("Shouldn't have come here");
        return NULL;
    }
}

cgen_var_t _cgen_get_var_types(cgen_t *cgen, const parser_t *parser, ast_node_pos pos, bool is_arg) {
    ast_node_t node = node_at(parser, pos);
    cgen_var_t types = (cgen_var_t){0};

    log_c("Getting variable type");
    switch (node.type) {
        break; case AST_ARRAY: 
            {
            log_c("Getting array type");
            dtype_t type = par_get_type(parser, node.var_data.array);
            if (is_arg) {
                types = _cgen_get_array_type(cgen, parser, node, type, 0);
            } else {
                types = _cgen_get_array_type(cgen, parser, node, type, 1);
                types.type = LLVMArrayType(types.type, type.array_length);
                types.ptr_type = types.type;
            }
            }
        break; case AST_INT: 
            types.type = cgen->i32; 
        break; case AST_BYTE: 
            types.type = cgen->i8; 
        break; case AST_REF_INT: 
            types.type = LLVMPointerType(cgen->i32, 0); 
        break; case AST_REF_BYTE: 
            types.type = LLVMPointerType(cgen->i8, 0); 
        break; default:
            throw(cgen_var_t, PAR_FSTR "Invalid argument type", PAR_FPOS(parser, node));
            return (cgen_var_t){0};
    }
    return types;
}

cgen_var_t _cgen_get_array_type(cgen_t *cgen, const parser_t *parser, ast_node_t node, dtype_t type, uint32_t dim) {
    dtype_pos i = POS_ADV(node.var_data.array, dim);
    cgen_var_t types = (cgen_var_t){0};

    if(POS_DIFF(node.var_data.array, i) < type.length) {
        types = _cgen_get_array_type(cgen, parser, node, type, dim + 1);
        if (dim == 0) {
            types.ptr_type = types.type;
            types.type = LLVMPointerType(types.type, 0);
        } else {
            types.type = LLVMArrayType(types.type, par_get_type(parser, i).array_length);
            types.ptr_type = types.type;
        }
        return types;
    } else {
        if (type.type & DTYPE_INT) {
            types.type = cgen->i32;
            types.element_type = cgen->i32;
            return types;
        }
        if (type.type & DTYPE_BYTE) {
            types.type = cgen->i8;
            types.element_type = cgen->i8;
            return types;
        }
        throw(cgen_var_t, PAR_FSTR "Invalid array base type", PAR_FPOS(parser, node));
        return (cgen_var_t){0};
    }
 
}

LLVMValueRef _cgen_get_var_value(cgen_t *cgen, const parser_t *parser, ast_node_pos pos, bool is_lvalue) {
    ast_node_t node = node_at(parser, pos);
    LLVMValueRef ptr;
    LLVMTypeRef var_type;

    switch(node.type) {
        break; case AST_ARRAY_AT:
            {
            ptr = _cgen_get_array_at_ptr(cgen, parser, pos);
            var_type = cg_get_symbol(&(cgen->cg_st), node.name).element_type;
            }
        break; case AST_NAME:
            {
            cgen_var_t var = cg_get_symbol(&(cgen->cg_st), node.name);
            ptr = var.alloca;
            var_type = var.type; 
            }
        break; default:
            throw(LLVMValueRef, PAR_FSTR "Invalid node for variable", PAR_FPOS(parser, node));
            return NULL;
    }

    if(is_lvalue) return ptr;
    return LLVMBuildLoad2(cgen->IRBuilder, var_type, ptr, "load");
}

LLVMValueRef _cgen_get_array_at_ptr(Unused cgen_t *cgen, Unused const parser_t *parser, Unused ast_node_pos pos) {
    log_c("Getting array at ptr");
    ast_node_t node = node_at(parser, pos);
    ast_node_it it = ast_get_child(parser, pos);

    LLVMValueRef* indices;
    LLVMValueRef index = c32(0);

    cgen_var_t array;
    LLVMValueRef ptr;

    array = cg_get_symbol(&(cgen->cg_st), node.name);    
    
    arr_init(indices);

    if(LLVMGetTypeKind(array.type) == LLVMArrayTypeKind) arr_push(indices, index);
    else if(LLVMGetTypeKind(array.type) != LLVMPointerTypeKind) throw(LLVMValueRef, "Not correct pointer type at array");

    for (; ast_is_child(it); it = ast_next_child(it)) {
			index = _cgen_generate_code(cgen, parser, it.pos);
            arr_push(indices, index);            
	}

    ptr = LLVMBuildInBoundsGEP2(cgen->IRBuilder, array.ptr_type, array.alloca, indices, arr_len(indices), "array_ptr");

    arr_free(indices);
   
    return ptr;
}


void cgen_verify_module(cgen_t *cgen) {
    char *error = NULL;
    LLVMVerifyModule(cgen->Module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    LLVMDumpModule(cgen->Module);
}

#pragma endregion
#endif
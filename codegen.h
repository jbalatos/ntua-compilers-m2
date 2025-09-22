#pragma once

#include "parser.h"
#include "util.h"
#include "util/dynamic_array.h"
#include "util/stack.h"

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Utils.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

#pragma region TYPES
typedef struct {
    LLVMModuleRef Module;
    LLVMBuilderRef IRBuilder;
    LLVMContextRef Context;

    LLVMTypeRef i1, i8, i32, i64;

} cgen_t;

typedef struct {
    LLVMTypeRef type;
    LLVMValueRef alloca;
} cgen_var_t;

typedef struct { uint16_t key; cgen_var_t value; } cg_cell_t;
typedef cg_cell_t  *cg_scope_t;
typedef cg_scope_t *cg_table_t;

#pragma endregion

#pragma region DEBUGGING
#ifdef CGEN_DEBUG
	#define log(fmt,...) \
		printf("CGEN: " fmt "\n" __VA_OPT__(,) __VA_ARGS__)
#else
	#define log(...)
#endif
#pragma endregion

#pragma region SYMBOL TABLE DECLARATIONS
#define ST_CLEANUP __attribute__((cleanup(cg_table_destroy)))
extern cg_table_t cg_table_create(void);
extern void        cg_table_destroy(cg_table_t *this);
#define cg_emplace_symbol(this, name, decl...) \
	cg_put_symbol(this, name, (cgen_var_t{ decl }))
extern cgen_var_t cg_get_symbol(const cg_table_t *this, uint16_t name);
extern void        cg_pop_scope(cg_table_t *this);
extern void        cg_push_scope(cg_table_t *this);
extern bool        cg_put_symbol(cg_table_t *this, uint16_t name, cgen_var_t type);
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

extern void cgen_verify_module(cgen_t *cgen);

#pragma endregion

#ifdef CGEN_IMPLEMENT
#pragma region CODEGEN IMPLEMENTATIONS
cg_table_t
cg_table_create (void)
{
	cg_table_t ret = {0};
	cg_push_scope(&ret);
	// TODO: add default procedures
	return ret;
}

void
cg_table_destroy (cg_table_t *this)
{
	for (; !stack_empty(*this); cg_pop_scope(this));
}

cgen_var_t
cg_get_symbol (const cg_table_t *this, uint16_t name)
{
	ptrdiff_t idx = -1;

	for (cg_scope_t *it = *this; it; it = stack_next(it))
		if ((idx = hm_geti(*it, name)) != -1)
			return (*it)[idx].value;
	return (cgen_var_t){0};
}

inline void
cg_pop_scope (cg_table_t *this)
{
	stack_pop(*this);
}

inline void
cg_push_scope (cg_table_t *this)
{
	stack_push(*this, (cg_scope_t){0});
}

inline bool
cg_put_symbol (cg_table_t *this, uint16_t name, cgen_var_t type)
{
	if (cg_scope_has(this, name)) return true;
	hm_put(stack_top(*this), name, type);
	return false;
}

inline bool
cg_scope_has (const cg_table_t *this, uint16_t name)
{
	return hm_geti(stack_top(*this), name) != -1;
}

#pragma endregion


#pragma region CODEGEN IMPLEMENTATIONS

#define c8(c) LLVMConstInt(LLVMInt8TypeInContext(cgen->Context), (uint64_t)(c), false)
#define c32(i) LLVMConstInt(LLVMInt32TypeInContext(cgen->Context), (uint64_t)(i), false)

void cgen_create(cgen_t *cgen) {
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    cgen->Context = LLVMContextCreate();
    cgen->Module = LLVMModuleCreateWithNameInContext("my_module", cgen->Context);
    cgen->IRBuilder = LLVMCreateBuilderInContext(cgen->Context);

    cgen->i1 = LLVMInt1TypeInContext(cgen->Context);
    cgen->i8 = LLVMInt8TypeInContext(cgen->Context);
    cgen->i32 = LLVMInt32TypeInContext(cgen->Context);
    cgen->i64 = LLVMInt64TypeInContext(cgen->Context);
}

void cgen_destroy(cgen_t *cgen) {
    LLVMDisposeBuilder(cgen->IRBuilder);
    LLVMDisposeModule(cgen->Module);
    LLVMContextDispose(cgen->Context);  
}

void cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos) {
    log("BEGIN CODEGEN");

    LLVMTypeRef main_type = LLVMFunctionType(cgen->i32, NULL, 0, false);
    Unused LLVMValueRef main_func = LLVMAddFunction(cgen->Module, "main", main_type);
    LLVMBasicBlockRef BB = LLVMAppendBasicBlockInContext(cgen->Context, main_func, "entry");
    LLVMPositionBuilderAtEnd(cgen->IRBuilder, BB);


    _cgen_generate_code(cgen, parser, pos);

    LLVMBuildRet(cgen->IRBuilder, c32(0));

    cgen_verify_module(cgen);


    char *error = NULL;
    if (LLVMPrintModuleToFile(cgen->Module, "output.ll", &error) != 0) {
        fprintf(stderr, "Error writing module to file: %s\n", error);
        LLVMDisposeMessage(error);
    }

    log("END CODEGEN");
}

LLVMValueRef _cgen_generate_code(Unused cgen_t *cgen, Unused const parser_t *parser, Unused ast_node_pos pos){
    log("Generating code...");

    #define CHECK_CHLD() for (; ast_is_child(it); it = ast_next_child(it)) \
	try(_sem_check(this, st, it.pos), PAR_FSTR, PAR_FPOS(this, node))

	ast_node_t node = node_at(parser, pos);
	ast_node_it it = ast_get_child(parser, pos);
	dtype_t lhs, rhs;
	enum dtype type;

	log("checking %s at " PAR_FSTR,
			ast_get_type_str(node), PAR_FPOS(parser, node));
	switch (node.type) {
    /*literals*/
    case AST_TRUE: 
        return c8('\x01');
    break; case AST_FALSE:
        return c8('\x00');
    break; case AST_NUMBER:
        return c32(node.pl_data.num);
    break; case AST_CHAR:
        return c8(node.pl_data.ch);
    break; case AST_STRING:
        return LLVMBuildGlobalStringPtr(cgen->IRBuilder, par_get_text(parser, node.pl_data.str), "str");
    break; case AST_NAME:
    /*variables*/
    break; case AST_INT:
    break; case AST_BYTE:
    break; case AST_REF_INT:
    break; case AST_REF_BYTE:
    break; case AST_ARRAY:
    /*expressions*/
    break; case AST_PLUS ... AST_CMP_GEQ:
	       case AST_BOOL_AND ... AST_BOOL_NOT: 
    break; case AST_ARRAY_AT:
    break; case AST_FUNC_CALL:
    break; case AST_PROC_CALL: 
	/* grouping nodes */
    break; case AST_ARGS:
    break; case AST_VARS:
    break; case AST_LOCAL_DEF:
    /* scope-creating nodes */
    break; case AST_BLOCK_SIMPLE:
    break; case AST_BLOCK: case AST_INDENT:
    break; case AST_DECL_PROC ... AST_DECL_BYTE:
           case AST_DEF_PROC ... AST_DEF_BYTE:
    /* statements */
    break; case AST_SKIP:
    break; case AST_EXIT:
    break; case AST_BREAK: 
    break; case AST_CONT:
    break; case AST_LOOP:
    break; case AST_ASSIGN:
    break; case AST_RETURN:
    break; case AST_COND:
    default:
        return c32(0);
    }
}

void cgen_verify_module(cgen_t *cgen) {
    char *error = NULL;
    LLVMVerifyModule(cgen->Module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    LLVMDumpModule(cgen->Module);
}

#pragma endregion
#endif
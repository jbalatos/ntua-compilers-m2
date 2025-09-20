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

#pragma endregion

#pragma region CODEGEN DECLARATIONS

extern LLVMValueRef c8(char c);
extern LLVMValueRef c32(int32_t i);

#define CGEN_CLEANUP __attribute__((cleanup(cgen_destroy)))
extern void cgen_create(cgen_t *cgen);
extern void cgen_destroy(cgen_t *cgen);

extern void cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);
extern void _cgen_generate_code(cgen_t *cgen, const parser_t *parser, ast_node_pos pos);

extern void cgen_verify_module(cgen_t *cgen);

#pragma endregion

#pragma region CODEGEN IMPLEMENTATIONS

#ifdef CGEN_IMPLEMENT

LLVMValueRef c8(char c) {
    return LLVMConstInt(LLVMInt8Type(), (uint64_t)c, false);
}

LLVMValueRef c32(int32_t i) {
    return LLVMConstInt(LLVMInt32Type(), (uint64_t)i, false);
}

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
    LLVMBasicBlockRef BB = LLVMCreateBasicBlockInContext(cgen->Context, "entry");
    LLVMPositionBuilderAtEnd(cgen->IRBuilder, BB);


    _cgen_generate_code(cgen, parser, pos);

    LLVMBuildRet(cgen->IRBuilder, c32(0));

    cgen_verify_module(cgen);
    log("END CODEGEN");
}

void _cgen_generate_code(Unused cgen_t *cgen, Unused const parser_t *parser, Unused ast_node_pos pos){
    log("Generating code...");
}

void cgen_verify_module(cgen_t *cgen) {
    char *error = NULL;
    LLVMVerifyModule(cgen->Module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    LLVMDumpModule(cgen->Module);
}

#endif
#pragma endregion
#pragma once

#define DANA_TYPES							\
	/* 	LEX NAME	AST NAME	SYMBOL */		\
	OP	(PLUS,		PLUS,		"+")	/* math */	\
	OP	(MINUS,		MINUS,		"-")			\
	OP	(MULT,		MULT,		"*")			\
	OP	(DIV,		DIV,		"/")			\
	OP	(MOD,		MOD,		"%")			\
	OP	(AND,		BIT_AND,	"&")	/* bitwise */	\
	OP	(OR,		BIT_OR,		"|")			\
	OP	(NOT,		BIT_NOT,	"!")			\
	OP	(EQ,		CMP_EQ,		"=")	/* compare */	\
	OP	(NEQ,		CMP_NEQ,	"<>")			\
	OP	(LT,		CMP_LT,		"<")			\
	OP	(LEQ,		CMP_LEQ,	"<=")			\
	OP	(GT,		CMP_GT,		">")			\
	OP	(GEQ,		CMP_GEQ,	">=")			\
	OP	(OPEN_PAREN,	FUNC,		"(")	/* dividers */	\
	OP	(CLOSE_PAREN,	ARGS,		")")			\
	OP	(OPEN_BRACKET,	ARR_AT,		"[")			\
	OP_LEX	(CLOSE_BRACKET,			"]")			\
	TK_LEX	(COMMA,				",")			\
	TK	(COLON,		PROC,		":")			\
	TK	(ASSIGN,	ASSIGN,		":=")			\
	LT_LEX	(OPEN_INDENT,			"TAB-BEGIN")		\
	LT_LEX	(CLOSE_INDENT,			"TAB-END")		\
	LT	(NAME,		NAME,		"name")			\
	LT	(NUMBER,	NUMBER,		"number")		\
	LT	(CHAR,		CHAR,		"char")			\
	LT	(STRING,	STRING,		"string")		\
	LT_PAR	(		BOOL,		"boolean")		\
	LT_PAR	(		REF_INT,	"int-ref")		\
	LT_PAR	(		REF_BYTE,	"byte-ref")		\
	LT_PAR	(		ARR_INT,	"int-array")		\
	LT_PAR	(		ARR_BYTE,	"byte-array")		\
	LT_PAR	(		LOCAL_DEF,	"local defs")		\
	LT_LEX	(EOF,				"EOF")			\

#define DANA_KEYWORDS \
	/* 	LEX NAME	AST NAME	SYMBOL */		\
	KW	(AND,		BOOL_AND,	"and")	/* bool ops */	\
	KW	(OR,		BOOL_OR,	"or")			\
	KW	(NOT,		BOOL_NOT,	"not")			\
	KW_LEX	(AS,				"as")			\
	KW	(BEGIN,		BLOCK,		"begin")/* blocks */	\
	KW	(END,		BLOCK_SIMPLE,	"end")			\
	KW	(IF,		COND_SIMPLE,	"if")			\
	KW	(ELIF,		COND,		"elif")			\
	KW_LEX	(ELSE,				"else")			\
	KW	(LOOP,		LOOP,		"loop")			\
	KW	(SKIP,		SKIP,		"skip")	/* simple ops */\
	KW	(EXIT,		EXIT,		"exit")			\
	KW	(BREAK,		BREAK,		"break")		\
	KW	(CONT,		CONT,		"continue")		\
	KW	(RETURN,	RETURN,		"return")		\
	KW_LEX	(VAR,				"var")	/* vars */	\
	KW_LEX	(REF,				"ref")			\
	KW	(INT,		INT,		"int")			\
	KW	(BYTE,		BYTE,		"byte")			\
	KW_LEX	(TRUE,				"true")			\
	KW_LEX	(FALSE,				"false")		\
	KW_LEX	(IS,				"is")			\
	KW	(DECL,		DECL_PROC,	"decl")	/* funcs */	\
	KW_PAR	(		DECL_INT,	"decl int")		\
	KW_PAR	(		DECL_BYTE,	"decl byte")		\
	KW	(DEF,		DEF_PROC,	"def")			\
	KW_PAR	(		DEF_INT,	"def int")		\
	KW_PAR	(		DEF_BYTE,	"def byte")		\

#define DANA_KW case DANA_KW_AND ... DANA_KW_DEF

#define LEFT_ASSOC 0
#define RIGHT_ASSOC 1

#define DANA_OPERATORS \
	IN(PLUS,		6,	LEFT_ASSOC)	/* math */	\
	IN(MINUS,		6,	LEFT_ASSOC)			\
	IN(MULT,		7,	LEFT_ASSOC)			\
	IN(DIV,			7,	LEFT_ASSOC)			\
	IN(MOD,			7,	LEFT_ASSOC)			\
	PRE(MINUS,		8)	/* unary minus */		\
	PRE(NOT,		8)			/* bitwise */	\
	IN(AND,			7,	LEFT_ASSOC)			\
	IN(OR,			6,	LEFT_ASSOC)			\
	IN(KW_AND,		3,	LEFT_ASSOC)	/* boolean */	\
	IN(KW_OR,		2,	LEFT_ASSOC)			\
	PRE(KW_NOT,		4)					\
	IN(EQ,			5,	LEFT_ASSOC)	/* compare */	\
	IN(NEQ,			5,	LEFT_ASSOC)			\
	IN(LT,			5,	LEFT_ASSOC)			\
	IN(LEQ,			5,	LEFT_ASSOC)			\
	IN(GT,			5,	LEFT_ASSOC)			\
	IN(GEQ,			5,	LEFT_ASSOC)			\
	PRE(OPEN_PAREN,		1)			/* groupings */ \
	POST(OPEN_PAREN,	9)			/* groupings */ \
	/* POST(OPEN_BRACKET,	9)					\ */

#define LVALUE		case AST_NAME:   \
			case AST_STRING: \
			case AST_ARR_AT


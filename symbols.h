#pragma once

#define DANA_TYPES					\
	OP(PLUS,		"+")	/* math */	\
	OP(MINUS,		"-")			\
	OP(MULT,		"*")			\
	OP(DIV,			"/")			\
	OP(MOD,			"%")			\
	OP(AND,			"&")	/* bitwise */	\
	OP(OR,			"|")			\
	OP(NOT,			"!")			\
	OP(EQ,			"=")	/* compare */	\
	OP(NEQ,			"<>")			\
	OP(LT,			"<")			\
	OP(LEQ,			"<=")			\
	OP(GT,			">")			\
	OP(GEQ,			">=")			\
	OP(OPEN_PAREN,		"(")	/* dividers */	\
	OP(CLOSE_PAREN,		")")			\
	OP(OPEN_BRACKET,	"[")			\
	OP(CLOSE_BRACKET,	"]")			\
	TK(COMMA,		",")			\
	TK(COLON,		":")			\
	TK(ASSIGN,		":=")			\
	LIT(OPEN_INDENT)				\
	LIT(CLOSE_INDENT)				\
	LIT(NAME)					\
	LIT(NUMBER)					\
	LIT(CHAR)					\
	LIT(STRING)					\
	LIT(EOF)

#define DANA_KEYWORDS					\
	KW(AND,		"and")	/* op keywords */	\
	KW(OR,		"or")				\
	KW(NOT,		"not")				\
	KW(AS,		"as")				\
	KW(BEGIN,	"begin")/* non-op keywords */	\
	KW(BREAK,	"break")			\
	KW(BYTE,	"byte")				\
	KW(CONT,	"continue")			\
	KW(DECL,	"decl")				\
	KW(DEF,		"def")				\
	KW(ELIF,	"elif")				\
	KW(ELSE,	"else")				\
	KW(END,		"end")				\
	KW(EXIT,	"exit")				\
	KW(FALSE,	"false")			\
	KW(IF,		"if")				\
	KW(IS,		"is")				\
	KW(INT,		"int")				\
	KW(LOOP,	"loop")				\
	KW(REF,		"ref")				\
	KW(RETURN,	"return")			\
	KW(SKIP,	"skip")				\
	KW(TRUE,	"true")				\
	KW(VAR,		"var")				\

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
	PRE(OPEN_PAREN,	1)				/* groupings */ \
	POST(OPEN_PAREN,	1)			/* groupings */ \
	POST(OPEN_BRACKET,	9)					\

#define DANA_BIN_OPS         DANA_PLUS   ... DANA_OR: \
			case DANA_KW_AND ... DANA_KW_OR
#define AST_BIN_OPS          AST_PLUS    ... AST_OR: \
			case AST_KW_AND  ... AST_KW_OR

#define DANA_PREF_OPS        DANA_PLUS: case DANA_MINUS: \
			case DANA_NOT:  case DANA_KW_NOT
#define AST_PREF_OPS         AST_NOT:   case AST_KW_NOT

#define DANA_POST_OPS        DANA_OPEN_BRACKET: case DANA_OPEN_PAREN
#define AST_POST_OPS         AST_OPEN_BRACKET:  case DANA_OPEN_PAREN

#define DANA_OPS DANA_PLUS ... DANA_CLOSE_BRACKET: case DANA_KW_AND ... DANA_KW_NOT
#define AST_OPS  AST_PLUS  ... AST_CLOSE_BRACKET:  case AST_KW_AND  ... AST_KW_NOT

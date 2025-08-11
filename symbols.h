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

#define DANA_KEYWORDS \
	KW    (AND,			"and")	/* op keywords */	\
	KW    (OR,			"or")				\
	KW    (NOT,			"not")				\
	KW    (AS,			"as")				\
	KW_EX(BEGIN,	BLOCK,		"begin")/* non-op keywords */	\
	KW   (BREAK,			"break")			\
	KW   (BYTE,			"byte")				\
	KW   (CONT,			"continue")			\
	KW   (DECL,			"decl")				\
	KW   (DEF,			"def")				\
	KW_EX(ELIF,	COND,		"elif")				\
	KW   (ELSE,			"else")				\
	KW_EX(END,	SIMPLE_BLOCK,	"end")				\
	KW   (EXIT,			"exit")				\
	KW   (FALSE,			"false")			\
	KW_EX(IF,	SIMPLE_COND,	"if")				\
	KW   (IS,			"is")				\
	KW   (INT,			"int")				\
	KW_EX(LOOP,	LOOP,		"loop")				\
	KW   (REF,			"ref")				\
	KW   (RETURN,			"return")			\
	KW   (SKIP,			"skip")				\
	KW   (TRUE,			"true")				\
	KW   (VAR,			"var")				\

#define DANA_KW case DANA_KW_AND ... DANA_KW_VAR

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
	POST(OPEN_BRACKET,	9)					\

#define AST_NAMED_NODE	case AST_KW_BREAK: \
			case AST_KW_CONT:  \
			case AST_LOOP:     \
			case AST_NAME
#define CLOSING_OP 	case DANA_CLOSE_PAREN: \
			case DANA_CLOSE_BRACKET
#define BOOLEAN		case DANA_KW_TRUE:\
			case DANA_KW_FALSE


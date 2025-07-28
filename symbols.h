#pragma once

#define DANA_TYPES					\
	OP(PLUS,		"+",	1,	LEFT_ASSOC)	/* math */	\
	OP(MINUS,		"-",	1,	LEFT_ASSOC)			\
	OP(MULT,		"*",	2,	LEFT_ASSOC)			\
	OP(DIV,			"/",	2,	LEFT_ASSOC)			\
	OP(MOD,			"%",	2,	LEFT_ASSOC)			\
	OP(NOT,			"!",	3,	UNARY)	/* bitwise */	\
	OP(AND,			"&",	3,	LEFT_ASSOC)			\
	OP(OR,			"|",	3,	LEFT_ASSOC)			\
	OP(EQ,			"=",	1,	LEFT_ASSOC)	/* compare */	\
	OP(NEQ,			"<>",	1,	LEFT_ASSOC)			\
	OP(LT,			"<",	1,	LEFT_ASSOC)			\
	OP(LEQ,			"<=",	1,	LEFT_ASSOC)			\
	OP(GT,			">",	1,	LEFT_ASSOC)			\
	OP(GEQ,			">=",	1,	LEFT_ASSOC)			\
	OP(OPEN_PAREN,		"(",	1,	LEFT_ASSOC)	/* dividers */	\
	OP(CLOSE_PAREN,		")",	1,	LEFT_ASSOC)			\
	OP(OPEN_BRACKET,	"[",	1,	LEFT_ASSOC)			\
	OP(CLOSE_BRACKET,	"]",	1,	LEFT_ASSOC)			\
	TK(COMMA,		",")						\
	TK(COLON,		":")						\
	TK(ASSIGN,		":=")						\
	LIT(OPEN_INDENT)							\
	LIT(CLOSE_INDENT)							\
	LIT(NAME)								\
	LIT(NUMBER)								\
	LIT(CHAR)								\
	LIT(STRING)								\
	LIT(EOF)

#define DANA_KEYWORDS				\
	KW(AND,		"and")			\
	KW(AS,		"as")			\
	KW(BEGIN,	"begin")		\
	KW(BREAK,	"break")		\
	KW(BYTE,	"byte")			\
	KW(CONT,	"continue")		\
	KW(DECL,	"decl")			\
	KW(DEF,		"def")			\
	KW(ELIF,	"elif")			\
	KW(ELSE,	"else")			\
	KW(END,		"end")			\
	KW(EXIT,	"exit")			\
	KW(FALSE,	"false")		\
	KW(IF,		"if")			\
	KW(IS,		"is")			\
	KW(INT,		"int")			\
	KW(LOOP,	"loop")			\
	KW(NOT,		"not")			\
	KW(OR,		"or")			\
	KW(REF,		"ref")			\
	KW(RETURN,	"return")		\
	KW(SKIP,	"skip")			\
	KW(TRUE,	"true")			\
	KW(VAR,		"var")			\


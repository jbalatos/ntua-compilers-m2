#pragma once

#include "lexer.h"
#include "types.h"

struct parser_bp_t { uint8_t lhs, rhs; };

extern parser_bp_t prefix_table[LEX_TYPES_LEN],
		   infix_table[LEX_TYPES_LEN],
		   postfix_table[LEX_TYPES_LEN];

extern bool _bp_is_prefix (uint32_t type, parser_bp_t *out);
extern bool _bp_is_infix (uint32_t type, parser_bp_t *out);
extern bool _bp_is_postfix (uint32_t type, parser_bp_t *out);

#define bp_is_prefix(x, out)  _bp_is_prefix((x).type, (out))
#define bp_is_infix(x, out)   _bp_is_infix((x).type, (out))
#define bp_is_postfix(x, out) _bp_is_postfix((x).type, (out))

#ifdef BP_IMPLEMENT
/** table definitions */
/* {{{ */
#define PRE(e, bp) [DANA_ ## e] = { 0, (bp) << 1 },
#define IN(e, bp, ass)
#define POST(e, bp)
parser_bp_t prefix_table[LEX_TYPES_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp)
#define IN(e, bp, ass) [DANA_ ## e] = { ((bp) << 1) + (ass), ((bp) << 1) + 1 - (ass) },
#define POST(e, bp)
parser_bp_t infix_table[LEX_TYPES_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST

#define PRE(e, bp)
#define IN(e, bp, ass)
#define POST(e, bp) [DANA_ ## e] = { (bp) << 1, 0 },
parser_bp_t postfix_table[LEX_TYPES_LEN] = { DANA_OPERATORS };
#undef PRE
#undef IN
#undef POST
/* }}} */

bool
_bp_is_prefix (uint32_t type, parser_bp_t *out)
{
	if (out) *out = prefix_table[type];
	return prefix_table[type].rhs;
}

bool
_bp_is_infix (uint32_t type, parser_bp_t *out)
{
	if (out) *out = infix_table[type];
	return infix_table[type].lhs && infix_table[type].rhs;
}

bool
_bp_is_postfix (uint32_t type, parser_bp_t *out)
{
	if (out) *out = postfix_table[type];
	return postfix_table[type].lhs;
}

#endif // BP_IMPLEMENT

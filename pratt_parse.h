#pragma once 

ast_node_pos
_parse_args (parser_t *this)
{/* {{{ */
	ast_node_t node = { .type = AST_ARGS };
	lex_token_t __attribute__((unused)) tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	arr_push(tmp, _parse_expr(this, 0));
	while (PEEK_TOKEN(this, tok, pos).type == DANA_COMMA) {
		POP_TOKEN(this, tok, pos);
		arr_push(tmp, _parse_expr(this, 0));
	}

	node.extra_data = parser_append_extras(this, tmp);
	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_block (parser_t *this)
{/* {{{ */
	static uint16_t level = 0;
	ast_node_t node;
	lex_token_t __attribute__((unused)) tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	printf("parse block:\tlevel: %2u\n", ++level);
	switch(PEEK_TOKEN(this, tok, pos).type) {
	// TODO: Add auto-indent case here
	case DANA_KW_BEGIN:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = AST_BLOCK };
		break;
	default:
		node = (ast_node_t){
			.type = AST_SIMPLE_BLOCK,
			.bin_data.lhs = _parse_stmt(this),
		};
		printf("--- simple block, END lvl %2u\n", level--);
		return AST_APPEND(this, node);
	}

	while (true) {
		switch (PEEK_TOKEN(this, tok, pos).type) {
		case DANA_EOF:
			_assert(false, "Reached EOF before end of block");
		// TODO: similar
		case DANA_KW_END:
			POP_TOKEN(this, tok, pos);
			node.extra_data = parser_append_extras(this, tmp);
			printf("--- block lvl %2u END\n", level--);
			return AST_APPEND(this, node);
		default:
			arr_push(tmp, _parse_stmt(this));
			break;
		}
	}

	_assert(false, "Reached end of parse_block. shouldn't happen");
}/* }}} */

ast_node_pos
_parse_cond (parser_t *this)
{/* {{{ */
	ast_node_t node = {
		.type = AST_COND,
	};
	lex_token_t tok;
	lex_token_pos pos;
	ast_node_pos *tmp = {0};

	printf("\tparsing cond\n");
	_assert(NEXT_TOKEN(this, tok, pos).type == DANA_KW_IF,
			"Expected if, found [%s] |%.*s|",
			lex_ttype_str(tok),
			UNSLICE(parser_token_val(this, tok)));
	arr_push(tmp, _parse_expr(this, 0));

	_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
			"Expected colon, found [%s] |%.*s|",
			lex_ttype_str(tok),
			UNSLICE(parser_token_val(this, tok)));
	arr_push(tmp, _parse_block(this));

	while (PEEK_TOKEN(this, tok, pos).type == DANA_KW_ELIF) {
		POP_TOKEN(this, tok, pos);
		arr_push(tmp, _parse_expr(this, 0));
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Expected colon, found [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		arr_push(tmp, _parse_block(this));
	}

	if (PEEK_TOKEN(this, tok, pos).type == DANA_KW_ELSE) {
		POP_TOKEN(this, tok, pos);
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Expected colon, found [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		arr_push(tmp, _parse_block(this));
	}

	node.extra_data = parser_append_extras(this, tmp);
	printf("cond END\n");
	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_expr (parser_t *this, uint8_t min_bp)
{/* {{{ */
	static uint16_t level = 0;

	ast_node_t node;
	ast_node_pos lhs;
	parser_bp_t bp;
	lex_token_t tok;
	lex_token_pos pos;

	NEXT_TOKEN(this, tok, pos);
	printf("parse expr (min_bp = %2u, level = %u)\tfirst_token = %10s\t|%.*s|\n",
			min_bp, ++level, lex_ttype_str(tok),
			UNSLICE(parser_token_val(this, tok)));

	/* initial token */
	switch (tok.type) {
	BOOLEAN:
		node = (ast_node_t){ .type = tok.type };
		lhs = AST_APPEND(this, node);
		break;
	case DANA_NAME:
		node = (ast_node_t){ .type = AST_NAME, .mixed_data.tok = pos };
		lhs = AST_APPEND(this, node);
		break;
	case DANA_NUMBER: {
		slice_char_t sl = parser_token_val(this, tok);
		LEX_TEMP_SLICE(sl);
		node = (ast_node_t){
			.type = AST_NUMBER, .num_data.value = atoi(sl.ptr),
		};
		lhs = AST_APPEND(this, node);
		break;
	}
	case DANA_OPEN_PAREN:
		lhs = _parse_expr(this, 0);
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_PAREN,
				"No closing paren found: found [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		node = parser_get_node(this, lhs);
		break;
	default:
		if (bp_is_prefix(tok, &bp)) {
			node = (ast_node_t){
				.type = tok.type,
				.bin_data.lhs = _parse_expr(this, bp.rhs),
			};
			lhs = AST_APPEND(this, node);
			break;
		}
		_assert(false, "First token not recognised: [%s] |%.*s|",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
	}

	/* following */
	while (PEEK_TOKEN(this, tok, pos).type != DANA_EOF) {
		printf("\tpeeking\t\t\t\tfirst_token = %10s\t|%.*s|\n",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		switch (tok.type) {
		CLOSING_OP:
			node = (ast_node_t){ .type = tok.type };
			break;
		default:
			if (bp_is_infix(tok, &bp) || bp_is_postfix(tok, &bp)) {
				node = (ast_node_t){ .type = tok.type };
				break;
			}
			printf("\tdidn't use peeked token --- END lvl %2u\n", level--);
			return lhs;
		}

		if (bp_is_postfix(node, &bp)) {
			if (bp.lhs < min_bp) break;
			POP_TOKEN(this, tok, pos);
			node.bin_data.lhs = lhs;
			switch (tok.type) {
			case DANA_OPEN_BRACKET:
				node.bin_data.rhs = _parse_expr(this, 0);
				_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_BRACKET,
						"No closing bracket found: found [%s] |%.*s|",
						lex_ttype_str(tok),
						UNSLICE(parser_token_val(this, tok)));
				break;
			case DANA_OPEN_PAREN:
				node.bin_data.rhs = _parse_expr(this, 0);
				_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_PAREN,
						"No closing paren found: found [%s] |%.*s|",
						lex_ttype_str(tok),
						UNSLICE(parser_token_val(this, tok)));
				break;
			default:
			}
			lhs = AST_APPEND(this, node);
			continue;
		}

		if (bp_is_infix(node, &bp)) {
			if (bp.lhs < min_bp) break;
			POP_TOKEN(this, tok, pos);
			node.bin_data.lhs = lhs;
			node.bin_data.rhs = _parse_expr(this, bp.rhs);
			lhs = AST_APPEND(this, node);
			continue;
		}
		break;
	}

	printf("parse expr --- END lvl %2u\n", level--);
	return lhs;
}/* }}} */

ast_node_pos
_parse_lval (parser_t *this)
{/* {{{ */
	ast_node_t node;
	lex_token_t tok;
	lex_token_pos pos;

	switch (NEXT_TOKEN(this, tok, pos).type) {
	case DANA_STRING:
		node = (ast_node_t){
			.type = AST_STRING, .mixed_data.tok = pos,
		};
		break;
	case DANA_NAME:
		node = (ast_node_t){
			.type = AST_NAME, .mixed_data.tok = pos,
		};
		if (PEEK_TOKEN(this, tok, pos).type == DANA_OPEN_BRACKET) {
			POP_TOKEN(this, tok, pos);
			node.mixed_data.node = _parse_lval(this);
			_assert(NEXT_TOKEN(this, tok, pos).type == DANA_CLOSE_BRACKET,
					"No closing bracket:\t%s [%.*s]",
					lex_ttype_str(tok),
					UNSLICE(parser_token_val(this, tok)));
		}
		break;
	default:
		_assert(false, "Wrong token when parsing token: %10s [%.*s]",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
	}

	return AST_APPEND(this, node);
}/* }}} */

ast_node_pos
_parse_stmt (parser_t *this)
{/* {{{ */
	ast_node_t node;
	ast_node_pos npos;
	lex_token_t tok;
	lex_token_pos pos;

	PEEK_TOKEN(this, tok, pos);
	printf("parse statement:\t\t%10s [%.*s]\n",
			lex_ttype_str(tok),
			UNSLICE(parser_token_val(this, tok)));

	switch (PEEK_TOKEN(this, tok, pos).type) {
	case DANA_KW_IF:
		return _parse_cond(this);
	case DANA_KW_SKIP:
	case DANA_KW_EXIT:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = tok.type };
		break;
	case DANA_KW_RETURN:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = tok.type };
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Wrong token after return keyword:\t%s [%.*s]",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		node.bin_data.lhs = _parse_expr(this, 0);
		break;
	case DANA_KW_BREAK:
	case DANA_KW_CONT:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = tok.type };
		if (PEEK_TOKEN(this, tok, pos).type == DANA_COLON) {
			POP_TOKEN(this, tok, pos);
			_assert(NEXT_TOKEN(this, tok, pos).type == DANA_NAME,
					"Expected name for break/cont, found:\t%s [%.*s]",
					lex_ttype_str(tok),
					UNSLICE(parser_token_val(this, tok)));
			node.mixed_data.tok = pos;
		}
		break;
	case DANA_KW_LOOP:
		POP_TOKEN(this, tok, pos);
		node = (ast_node_t){ .type = AST_LOOP };
		if (PEEK_TOKEN(this, tok, pos).type == DANA_NAME) {
			POP_TOKEN(this, tok, pos);
			node.mixed_data.tok = pos;
		}
		_assert(NEXT_TOKEN(this, tok, pos).type == DANA_COLON,
				"Wrong token after loop definition:\t%s [%.*s]",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
		node.mixed_data.node = _parse_block(this);
		break;
	LVALUE:
		npos = _parse_lval(this);
		if (PEEK_TOKEN(this, tok, pos).type == DANA_ASSIGN) {
			POP_TOKEN(this, tok, pos);
			node = (ast_node_t){
				.type = AST_ASSIGN,
				.bin_data = { npos, _parse_expr(this, 0) },
			};
		} else {
			node = (ast_node_t){
				.type = AST_PROC, .bin_data.lhs = npos,
			};
			if (PEEK_TOKEN(this, tok, pos).type == DANA_COLON) {
				POP_TOKEN(this, tok, pos);
				node.bin_data.rhs = _parse_args(this);
			}
		}
		break;
	default:
		_assert(false, "Unknown token in stmt:\t%s [%.*s]",
				lex_ttype_str(tok),
				UNSLICE(parser_token_val(this, tok)));
	}

	printf("stmt parsed: %10s\n", ast_type_str(node));
	return AST_APPEND(this, node);
}/* }}} */



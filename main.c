#include <argp.h>
#include <stdio.h>

#if defined(ENABLE_SANITIZER)
#	include <sanitizer/asan_sanitizer.h>
#endif
#if defined(ENABLE_DEBUG)
#	define SEM_DEBUG
#	define CGEN_DEBUG
#endif

enum compiler_options { OPT_EXEC = 0, OPT_ASM, OPT_IR } __attribute__((packed));

#define ALLOC_IMPLEMENT
#include "util/alloc.h"

#define PARSER_IMPLEMENT
#include "parser.h"

#define SEM_IMPLEMENT
#include "semantic.h"

#define CGEN_IMPLEMENT
#include "codegen.h"

#pragma region CLI options
const char *argp_program_version = "Dana parser 1.0";
const char *argp_program_bug_address = "<el21170@ntua.gr>, <el21001@ntua.gr>";
static const char doc[] = "Compiler for Dana language. Part of Compilers class for NTUA ECE.\n"
                          "Unless -f/-i is used, must get source file as an argument. By default, creates binary file a.out";
static const char args_doc[] = "[FILE]";
static const char default_output[] = "./a.out";

static struct argp_option options[] = {
	{ .key = 'o', .doc = "Write output to <out_file>", .arg = "OUT_FILE" },
	{ .key = 'O', .doc = "Optimize code" },
	{ .key = 'f', .doc = "Read source code from stdin, write binary to stdout" },
	{ .key = 'i', .doc = "Read source code from stdin, write IR to stdout" },
	{ 0 },
};

struct opt_args {
	const char *input, *output;
	enum compiler_options flg;
	bool optimize : 1;
};

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
	struct opt_args *args = state->input;

	switch (key) {
	break; case 'o':
		if (args->flg != 0)
			argp_error(state, "Can only use one of -o / -f / -i");
		args->output = arg;
	break; case 'f':
		if (args->flg != 0)
			argp_error(state, "Can only use one of -o / -f / -i");
		args->flg = OPT_ASM;
	break; case 'i':
		if (args->flg != 0)
			argp_error(state, "Can only use one of -o / -f / -i");
		args->flg = OPT_IR;
	break; case 'O':
		args->optimize = true;
	break; case ARGP_KEY_ARG:
		args->input = arg;
	break; case ARGP_KEY_END:
		if (!args->output)
			args->output = default_output;
		if (args->flg != OPT_EXEC) {
			if (args->input) {
				argp_error(state, "Cannot include input file if -f or -i is specified");
				argp_usage(state);
			}
			break;
		}
		if (!args->input) {
			argp_error(state, "Must include input file");
			argp_usage(state);
		}
		if (strlen(args->input) < 5 || strncmp(
					args->input + strlen(args->input) - 5,
					".dana", 5)) {
			argp_error(state, "Invalid input file type; must be .dana");
			argp_usage(state);
		}
	break; default: return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static struct argp argp = {
	.options = options,
	.parser = parse_opt,
	.args_doc = args_doc,
	.doc = doc,
};
#pragma endregion

/* debug printing */
static void debug_ast(const parser_t *parser, ast_node_pos root);
static void debug_ast_array(const parser_t *parser);
static void debug_line_map(const parser_t *parser);
static void debug_name_table(const parser_t *parser);
static void debug_type_table(const parser_t *parser);

int main (int argc, char *argv[argc])
{
	struct opt_args args = {0};
	argp_parse(&argp, argc, argv, 0, 0, &args);

#if defined(ENABLE_DEBUG)
	printf("---DEBUG INFO (%lu):---\n", sizeof(uint8_t));
	printf("\tLEXER:\r");
	printf("\t\t lexer_t size: %lu\n", sizeof(lexer_t));
	printf("\t\t lex_token_t size: %lu\n", sizeof(lex_token_t));
	printf("\t\t lex_buf_pos size: %lu\n", sizeof(lex_buf_pos));
	printf("\t\t lex_types length: %u\n", LEX_TYPE_LEN);
	printf("\tPARER:\r");
	printf("\t\t parser_t size: %lu\n", sizeof(parser_t));
	printf("\t\t ast_node_t size: %lu\n", sizeof(ast_node_t));
	printf("\t\t ast_ttype size: %lu\n", sizeof(enum ast_type));
	printf("\t\t ast_ttype length: %u\n", AST_TYPE_LEN);
	printf("---DEBUG END---\n\n");
#endif

	parser_t PARSER_CLEANUP parser = parser_create(args.flg == OPT_EXEC
			? lexer_create_from_file(&LIBC, args.input)
			: lexer_create_from_stdin(&LIBC)
			);
	ast_node_pos root = parse(&parser);
	if (!POS_OK(root)) {
		printf("\n=== Parsing Failed ===\n");
		return 1;
	}

	debug_ast(&parser, root);
	debug_ast_array(&parser);

	if (!sem_check(&parser, root)) {
		printf("\n=== Semantic Check Failed ===\n");
		return 1;
	}

	cgen_t CGEN_CLEANUP cgen = cgen_create(&parser, 0);
	cgen_generate_code(&cgen, &parser, root, args.flg, args.input);

	if (args.flg == OPT_EXEC) {
		string_node nodes[] = {
			{ .str = StrLit("clang -fno-pie -no-pie") },
			{ .str = { .ptr = (char*)args.input, .length = strlen(args.input) } },
			{ .str = StrLit("lib.a") },
			{ .str = StrLit("-o") },
			{ .str = { .ptr = (char*)args.output, .length = strlen(args.output) } },
		};
		string_list l = {0};
		for (size_t i=0; i<LENGTH(nodes); ++i) ListPushBack(l, nodes + i);
		slice_char_t STR_CLEANUP cmd = str_join(l, .sep = StrLit(" "), .c_str = true);

		system(cmd.ptr);
	}

	return 0;
}

#pragma region DEBUG INFO
static void __attribute__((unused))
debug_ast (const parser_t *parser, ast_node_pos root)
{
	printf("\n=== AST PRETTY PRINT ===\n");
	par_print(parser, root);
	printf("\n");
}

static void __attribute__((unused))
debug_ast_array (const parser_t *parser)
{
	printf("\n=== AST ARRAY ===\n");	
	for (size_t i=0; i<arr_ulen(parser->nodes); ++i)
		switch (parser->nodes[i].type){
		case AST_TRUE ... AST_FALSE:
			printf("%3lu :\t %10s\t\t\t(1)\n", i,
					ast_get_type_str(parser->nodes[i]));
			break;
		case AST_NUMBER:
			printf("%3lu :\t %10s\t%8d\t(1)\n", i,
					ast_get_type_str(parser->nodes[i]),
					parser->nodes[i].pl_data.num);
			break;
		case AST_STRING:
			printf("%3lu :\t %10s\t%10s\t(1)\n", i,
					ast_get_type_str(parser->nodes[i]),
					par_get_text(parser, parser->nodes[i].pl_data.str));
			break;
		case AST_NAME:
		case AST_ARRAY_AT:
		case AST_FUNC_CALL:
		case AST_PROC_CALL:
		case AST_BREAK:
		case AST_CONT:
		case AST_LOOP:
		case AST_INT ... AST_BYTE:
		case AST_REF_INT ... AST_REF_BYTE:
		case AST_DECL_PROC ... AST_DECL_BYTE:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u)\n", i,
					ast_get_type_str(parser->nodes[i]),
					UNSLICE(par_get_name(
							parser, parser->nodes[i]
							)),
					parser->nodes[i].name,
					parser->nodes[i].length);
			break;
		case AST_DEF_PROC ... AST_DEF_BYTE:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u) (locals at +%u, body at +%u)\n", i,
					ast_get_type_str(parser->nodes[i]),
					UNSLICE(par_get_name(
							parser, parser->nodes[i]
							)),
					parser->nodes[i].name,
					parser->nodes[i].length,
					parser->nodes[i].def_data.local_off,
					parser->nodes[i].def_data.body_off);
			break;
		case AST_ARRAY:
			printf("%3lu :\t %10s\t%8.*s (%u)\t(%u)\n", i,
					ast_get_type_str(parser->nodes[i]),
					UNSLICE(par_get_name(
							parser, parser->nodes[i]
							)),
					parser->nodes[i].name,
					parser->nodes[i].length);
			dtype_t type = par_get_type(parser, parser->nodes[i].var_data.array);
			printf("\t->\t%s", type.type & DTYPE_BYTE ? "byte" : "int");
			for (dtype_t *arr = &type; arr->type & DTYPE_ARRAY; ++arr)
				if (arr->type & DTYPE_VAR_ARRAY) printf("[]");
				else printf("[%u]", arr->array_length);
			printf("\n");
			break;
		default:
			printf("%3lu :\t %10s\t\t\t(%u)\n", i,
					ast_get_type_str(parser->nodes[i]),
					parser->nodes[i].length);
		}
	printf("\n");
}

static void __attribute__((unused))
debug_line_map (const parser_t *parser)
{
	printf("\n=== LINE MAP ===\n");
	for (size_t i=0; i<hm_ulen(parser->lexer.lines); ++i)
		printf("Line %4lu\t|%6u\n", i,
				parser->lexer.lines[i].pos);
	printf("\n");
}

static void __attribute__((unused))
debug_name_table (const parser_t *parser)
{
	printf("\n=== NAME TABLE ===\n");
	for (size_t i=0; i<arr_ulen(parser->names); ++i) {
		printf("%4u\t%.*s\n", parser->names[i].value,
				UNSLICE(parser->names[i].decl));
	}
	printf("\n");
}

static void __attribute__((unused))
debug_type_table (const parser_t *parser)
{
	printf("\n=== DATA TYPE TABLE ===\n");
	for (size_t i=1; i<arr_ulen(parser->types); ++i) {
		printf("%4lu :\t%15s, %u", i,
				dtype_get_type_str(parser->types[i].type),
				parser->types[i].length);
		if (parser->types[i].type & DTYPE_ARRAY)
			printf(" [%u]\n", parser->types[i].array_length);
		else
			printf("\n");
	}
	printf("\n");
}
#pragma endregion

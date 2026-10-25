#define _DEFAULT_SOURCE
// Janis Kikulis 251RMC245
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

#define STUDENT_NAME "Janis"
#define STUDENT_LASTNAME "Kikulis"
#define STUDENT_ID "251RMC245"

typedef enum {
    TOKEN_NUMBER,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_POW,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_EOF,
    TOKEN_ERROR
} TokenType;

typedef struct {
    TokenType type;
    double value;
    int is_float_literal;
    size_t pos;
} Token;

typedef struct {
    const char *src;
    size_t length;
    size_t index;
    size_t file_pos;
} Tokenizer;

void init_tokenizer(Tokenizer *t, const char *src, size_t length) {
    t->src = src;
    t->length = length;
    t->index = 0;
    t->file_pos = 1;
}

Token next_token(Tokenizer *t) {
    while (t->index < t->length) {
        char c = t->src[t->index];
        size_t current_pos = t->file_pos;

        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            t->index++;
            t->file_pos++;
            continue;
        }

        if (c == '+') { t->index++; t->file_pos++; return (Token){TOKEN_PLUS, 0, 0, current_pos}; }
        if (c == '-') { t->index++; t->file_pos++; return (Token){TOKEN_MINUS, 0, 0, current_pos}; }
        if (c == '*') {
            if (t->index + 1 < t->length && t->src[t->index + 1] == '*') {
                t->index += 2;
                t->file_pos += 2;
                return (Token){TOKEN_POW, 0, 0, current_pos};
            }
            t->index++;
            t->file_pos++;
            return (Token){TOKEN_MUL, 0, 0, current_pos};
        }
        if (c == '/') { t->index++; t->file_pos++; return (Token){TOKEN_DIV, 0, 0, current_pos}; }
        if (c == '(') { t->index++; t->file_pos++; return (Token){TOKEN_LPAREN, 0, 0, current_pos}; }
        if (c == ')') { t->index++; t->file_pos++; return (Token){TOKEN_RPAREN, 0, 0, current_pos}; }

        if (isdigit(c)) {
            size_t start_idx = t->index;
            int has_dot = 0;
            int has_exp = 0;

            while (t->index < t->length) {
                char ch = t->src[t->index];
                if (isdigit(ch)) {
                    t->index++;
                    t->file_pos++;
                } else if (ch == '.' && !has_dot && !has_exp) {
                    has_dot = 1;
                    t->index++;
                    t->file_pos++;
                } else if ((ch == 'e' || ch == 'E') && !has_exp) {
                    has_exp = 1;
                    t->index++;
                    t->file_pos++;
                    if (t->index < t->length && (t->src[t->index] == '+' || t->src[t->index] == '-')) {
                        t->index++;
                        t->file_pos++;
                    }
                } else {
                    break;
                }
            }

            if (t->index > start_idx && t->src[t->index - 1] == '.') {
                t->index--;
                t->file_pos--;
                return (Token){TOKEN_ERROR, 0, 0, t->file_pos};
            }

            char temp[128];
            size_t len = t->index - start_idx;
            if (len >= sizeof(temp)) len = sizeof(temp) - 1;
            memcpy(temp, &t->src[start_idx], len);
            temp[len] = '\0';

            double val = strtod(temp, NULL);
            int is_float = (has_dot || has_exp);
            return (Token){TOKEN_NUMBER, val, is_float, current_pos};
        }

        return (Token){TOKEN_ERROR, 0, 0, current_pos};
    }
    return (Token){TOKEN_EOF, 0, 0, t->file_pos};
}

typedef struct {
    Tokenizer tokenizer;
    Token current_token;
    int error_occurred;
    size_t error_pos;
    int expression_has_float;
} Parser;

void advance(Parser *p) {
    p->current_token = next_token(&p->tokenizer);
}

void set_error(Parser *p, size_t pos) {
    if (!p->error_occurred) {
        p->error_occurred = 1;
        p->error_pos = pos;
    }
}

double parse_expr(Parser *p);

double parse_primary(Parser *p) {
    Token tok = p->current_token;
    if (tok.type == TOKEN_NUMBER) {
        if (tok.is_float_literal) {
            p->expression_has_float = 1;
        }
        advance(p);
        return tok.value;
    } else if (tok.type == TOKEN_LPAREN) {
        advance(p);
        double val = parse_expr(p);
        if (p->current_token.type == TOKEN_RPAREN) {
            advance(p);
            return val;
        } else {
            set_error(p, p->current_token.pos);
            return 0;
        }
    } else {
        set_error(p, tok.pos);
        return 0;
    }
}

double parse_power(Parser *p) {
    double base = parse_primary(p);
    if (p->error_occurred) return 0;

    if (p->current_token.type == TOKEN_POW) {
        size_t op_pos = p->current_token.pos;
        advance(p);
        
        if (p->current_token.type == TOKEN_POW) {
            set_error(p, p->current_token.pos);
            return 0;
        }

        double exp = parse_power(p);
        if (p->error_occurred) return 0;

        if (exp < 0) {
            set_error(p, op_pos);
            return 0;
        }

        if (!(floor(base) == base && floor(exp) == exp && exp >= 0)) {
            p->expression_has_float = 1;
        }

        errno = 0;
        double res = pow(base, exp);
        if (isinf(res) || isnan(res) || errno == EDOM || errno == ERANGE) {
            set_error(p, op_pos);
            return 0;
        }
        return res;
    }
    return base;
}

double parse_term(Parser *p) {
    double val = parse_power(p);
    if (p->error_occurred) return val;

    while (p->current_token.type == TOKEN_MUL || p->current_token.type == TOKEN_DIV) {
        TokenType op = p->current_token.type;
        size_t op_pos = p->current_token.pos;
        advance(p);
        double right = parse_power(p);
        if (p->error_occurred) return 0;

        if (op == TOKEN_MUL) {
            val *= right;
        } else {
            if (right == 0.0) {
                set_error(p, op_pos);
                return 0;
            }
            p->expression_has_float = 1;
            val /= right;
        }
    }
    return val;
}

double parse_expr(Parser *p) {
    double val = parse_term(p);
    if (p->error_occurred) return val;

    while (p->current_token.type == TOKEN_PLUS || p->current_token.type == TOKEN_MINUS) {
        TokenType op = p->current_token.type;
        advance(p);
        double right = parse_term(p);
        if (p->error_occurred) return 0;

        if (op == TOKEN_PLUS) {
            val += right;
        } else {
            val -= right;
        }
    }
    return val;
}

void evaluate_line(const char *line_start, size_t line_len, size_t file_offset, FILE *out) {
    size_t i = 0;
    while (i < line_len && (line_start[i] == ' ' || line_start[i] == '\t' || line_start[i] == '\r')) {
        i++;
    }
    if (i >= line_len || line_start[i] == '\n' || line_start[i] == '\r') {
        return;
    }
    if (line_start[i] == '#') {
        return;
    }

    size_t effective_len = line_len;
    for (size_t k = i; k < line_len; k++) {
        if (line_start[k] == '#') {
            effective_len = k;
            break;
        }
    }

    Parser p;
    init_tokenizer(&p.tokenizer, line_start, effective_len);
    p.error_occurred = 0;
    p.error_pos = 0;
    p.expression_has_float = 0;

    advance(&p);
    double result = parse_expr(&p);

    if (!p.error_occurred && p.current_token.type != TOKEN_EOF) {
        set_error(&p, p.current_token.pos);
    }

    if (p.error_occurred) {
        size_t abs_pos = file_offset + (p.error_pos - 1);
        fprintf(out, "ERROR:%zu\n", abs_pos);
    } else {
        for (size_t k = i; k < line_len; k++) {
            if (line_start[k] == '#') {
                size_t abs_pos = file_offset + k;
                fprintf(out, "ERROR:%zu\n", abs_pos);
                return;
            }
        }

        if (p.expression_has_float || floor(result) != result) {
            fprintf(out, "%.15g\n", result);
        } else {
            fprintf(out, "%11lld\n", (long long)result);
        }
    }
}

void process_file(const char *input_path, const char *output_path) {
    FILE *in = fopen(input_path, "rb");
    if (!in) {
        fprintf(stderr, "Error opening input file: %s\n", input_path);
        exit(1);
    }

    fseek(in, 0, SEEK_END);
    long size = ftell(in);
    fseek(in, 0, SEEK_SET);

    char *buf = malloc(size + 1);
    if (!buf) {
        fclose(in);
        exit(1);
    }
    size_t rbytes = fread(buf, 1, size, in);
    (void)rbytes;
    buf[size] = '\0';
    fclose(in);

    char temp_path[1024];
    snprintf(temp_path, sizeof(temp_path), "%s", output_path);
    for (char *p = temp_path + 1; *p; p++) {
        if (*p == '/' || *p == '\\') {
            *p = '\0';
            mkdir(temp_path, 0777);
            *p = '/';
        }
    }

    FILE *out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error opening output file: %s\n", output_path);
        free(buf);
        exit(1);
    }

    size_t line_start_idx = 0;
    size_t file_offset = 1;

    for (long i = 0; i <= size; i++) {
        if (i == size || buf[i] == '\n') {
            size_t line_len = i - line_start_idx;
            evaluate_line(buf + line_start_idx, line_len, file_offset, out);
            file_offset += (i - line_start_idx + 1);
            line_start_idx = i + 1;
        }
    }

    free(buf);
    fclose(out);
}

void get_basename_without_ext(const char *path, char *dest, size_t max_len) {
    const char *last_slash = strrchr(path, '/');
    #ifdef _WIN32
    const char *last_backslash = strrchr(path, '\\');
    if (last_backslash && (!last_slash || last_backslash > last_slash)) {
        last_slash = last_backslash;
    }
    #endif
    const char *filename = last_slash ? last_slash + 1 : path;
    strncpy(dest, filename, max_len);
    char *ext = strrchr(dest, '.');
    if (ext) *ext = '\0';
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return 1;
    }

    char *dir_arg = NULL;
    char *outdir_arg = NULL;
    char *input_arg = NULL;

    for (int i = 1; i < argc; i++) {
        if ((strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0)) {
            if (i + 1 < argc) {
                dir_arg = argv[++i];
            } else {
                fprintf(stderr, "Missing directory argument\n");
                return 1;
            }
        } else if ((strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0)) {
            if (i + 1 < argc) {
                outdir_arg = argv[++i];
            } else {
                fprintf(stderr, "Missing output directory argument\n");
                return 1;
            }
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else {
            input_arg = argv[i];
        }
    }

    if (dir_arg) {
        DIR *dir = opendir(dir_arg);
        if (!dir) {
            fprintf(stderr, "Error opening directory: %s\n", dir_arg);
            return 1;
        }

        char outdir_buf[512];
        if (!outdir_arg) {
            snprintf(outdir_buf, sizeof(outdir_buf), "%s_%s_%s_%s", dir_arg, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
            outdir_arg = outdir_buf;
        }
        mkdir(outdir_arg, 0777);

        struct dirent *entry;
        while ((entry = readdir(dir)) != NULL) {
            if (entry->d_type == DT_REG || entry->d_type == DT_UNKNOWN) {
                size_t len = strlen(entry->d_name);
                if (len > 4 && strcmp(entry->d_name + len - 4, ".txt") == 0) {
                    char in_path[1024];
                    char out_path[1024];
                    snprintf(in_path, sizeof(in_path), "%s/%s", dir_arg, entry->d_name);

                    char base_name[256];
                    get_basename_without_ext(entry->d_name, base_name, sizeof(base_name));
                    snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s_%s.txt", outdir_arg, base_name, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

                    process_file(in_path, out_path);
                }
            }
        }
        closedir(dir);
    } else if (input_arg) {
        FILE *test_in = fopen(input_arg, "r");
        if (!test_in) {
            fprintf(stderr, "Missing or unreadable input file: %s\n", input_arg);
            return 1;
        }
        fclose(test_in);

        char base_name[256];
        get_basename_without_ext(input_arg, base_name, sizeof(base_name));

        char out_path[1024];
        if (outdir_arg) {
            mkdir(outdir_arg, 0777);
            snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s_%s.txt", outdir_arg, base_name, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
        } else {
            char default_outdir[512];
            snprintf(default_outdir, sizeof(default_outdir), "%s_%s_%s_%s", base_name, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
            mkdir(default_outdir, 0777);
            snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s_%s.txt", default_outdir, base_name, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
        }
        process_file(input_arg, out_path);
    } else {
        fprintf(stderr, "Missing input file or directory.\n");
        return 1;
    }

    return 0;
}
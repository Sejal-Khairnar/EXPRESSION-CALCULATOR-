#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#define MAX_EXPR 4096
#define MAX_TOKENS 4096
#define MAX_TOKEN_LEN 64

// -------------------- Simple stack for operators (chars) --------------------
typedef struct {
    char data[MAX_TOKENS];
    int top;
} CharStack;

void cs_init(CharStack *s) { s->top = -1; }
int  cs_empty(CharStack *s) { return s->top < 0; }
char cs_peek(CharStack *s) { return s->data[s->top]; }
int  cs_push(CharStack *s, char c) {
    if (s->top + 1 >= MAX_TOKENS) return 0;
    s->data[++s->top] = c;
    return 1;
}
char cs_pop(CharStack *s) { return s->data[s->top--]; }

// -------------------- Simple stack for numbers (long long) --------------------
typedef struct {
    long long data[MAX_TOKENS];
    int top;
} NumStack;

void ns_init(NumStack *s) { s->top = -1; }
int  ns_empty(NumStack *s) { return s->top < 0; }
int  ns_push(NumStack *s, long long v) {
    if (s->top + 1 >= MAX_TOKENS) return 0;
    s->data[++s->top] = v;
    return 1;
}
long long ns_pop(NumStack *s) { return s->data[s->top--]; }

// -------------------- Operator utilities --------------------
int is_operator(char c) {
    return c=='+' || c=='-' || c=='*' || c=='/' || c=='%' || c=='^' || c=='u'; // 'u' = unary minus
}

int precedence(char op) {
    switch (op) {
        case 'u': return 4; // unary minus: highest
        case '^': return 3;
        case '*': case '/': case '%': return 2;
        case '+': case '-': return 1;
        default: return 0;
    }
}

int is_right_assoc(char op) {
    return (op == '^' || op == 'u'); // right associative
}

// -------------------- Integer power (handles non-negative exponent) --------------------
int safe_pow_ll(long long base, long long exp, long long *out) {
    if (exp < 0) return 0; // not supporting negative exponents in integer arithmetic
    long long result = 1;
    while (exp) {
        if (exp & 1) {
            // Basic overflow check (conservative)
            if (base != 0 && llabs(result) > LLONG_MAX / llabs(base)) return 0;
            result *= base;
        }
        exp >>= 1;
        if (exp) {
            if (llabs(base) > 0 && llabs(base) > LLONG_MAX / llabs(base)) return 0;
            base *= base;
        }
    }
    *out = result;
    return 1;
}

// -------------------- Token helpers --------------------
typedef struct {
    char items[MAX_TOKENS][MAX_TOKEN_LEN];
    int count;
} TokenList;

void tokens_init(TokenList *tl) { tl->count = 0; }
int  tokens_add(TokenList *tl, const char *s) {
    if (tl->count >= MAX_TOKENS) return 0;
    strncpy(tl->items[tl->count], s, MAX_TOKEN_LEN-1);
    tl->items[tl->count][MAX_TOKEN_LEN-1] = '\0';
    tl->count++;
    return 1;
}

// -------------------- Infix to Postfix (Shunting-Yard) --------------------
int infix_to_postfix(const char *expr, TokenList *out_postfix, char *err_msg) {
    CharStack ops; cs_init(&ops);
    tokens_init(out_postfix);

    int i = 0;
    int expect_operand = 1; // start by expecting an operand (or unary minus or '(')

    while (expr[i]) {
        if (isspace((unsigned char)expr[i])) { i++; continue; }

        // Number (supports multi-digit and leading spaces)
        if (isdigit((unsigned char)expr[i])) {
            char buf[MAX_TOKEN_LEN]; int bi = 0;
            while (isdigit((unsigned char)expr[i])) {
                if (bi+1 >= MAX_TOKEN_LEN) { strcpy(err_msg,"Number token too long"); return 0; }
                buf[bi++] = expr[i++];
            }
            buf[bi] = '\0';
            if (!tokens_add(out_postfix, buf)) { strcpy(err_msg,"Too many tokens"); return 0; }
            expect_operand = 0; // next should be operator or ')'
            continue;
        }

        // Parentheses
        if (expr[i] == '(') {
            if (!cs_push(&ops, '(')) { strcpy(err_msg,"Operator stack overflow"); return 0; }
            i++; expect_operand = 1;
            continue;
        }
        if (expr[i] == ')') {
            int matched = 0;
            while (!cs_empty(&ops)) {
                char top = cs_pop(&ops);
                if (top == '(') { matched = 1; break; }
                char op_str[2] = {top, '\0'};
                if (!tokens_add(out_postfix, op_str)) { strcpy(err_msg,"Too many tokens"); return 0; }
            }
            if (!matched) { strcpy(err_msg,"Mismatched parentheses"); return 0; }
            i++; expect_operand = 0;
            continue;
        }

        // Operators (including unary minus)
        if (is_operator(expr[i])) {
            char op = expr[i];

            // Determine unary minus
            if (op == '-' && expect_operand) {
                op = 'u'; // mark as unary minus
            } else if (expect_operand && op != 'u') {
                strcpy(err_msg,"Unexpected operator");
                return 0;
            }

            // Pop while higher precedence (or equal & left-assoc)
            while (!cs_empty(&ops) && is_operator(cs_peek(&ops))) {
                char top = cs_peek(&ops);
                int ptop = precedence(top), popr = precedence(op);
                if ( (ptop > popr) || (ptop == popr && !is_right_assoc(op)) ) {
                    top = cs_pop(&ops);
                    char op_str[2] = {top, '\0'};
                    if (!tokens_add(out_postfix, op_str)) { strcpy(err_msg,"Too many tokens"); return 0; }
                } else break;
            }
            if (!cs_push(&ops, op)) { strcpy(err_msg,"Operator stack overflow"); return 0; }
            i++;
            expect_operand = (op != 'u'); // after unary minus we still expect an operand; for binary op we expect operand next
            continue;
        }

        // Unknown character
        sprintf(err_msg, "Invalid character: '%c'", expr[i]);
        return 0;
    }

    // Drain operator stack
    while (!cs_empty(&ops)) {
        char top = cs_pop(&ops);
        if (top == '(' || top == ')') { strcpy(err_msg,"Mismatched parentheses"); return 0; }
        char op_str[2] = {top, '\0'};
        if (!tokens_add(out_postfix, op_str)) { strcpy(err_msg,"Too many tokens"); return 0; }
    }

    if (expect_operand) { strcpy(err_msg,"Expression ends unexpectedly"); return 0; }

    return 1;
}

// -------------------- Postfix evaluation --------------------
int apply_op(char op, NumStack *stk, long long *err_val, char *err_msg) {
    if (op == 'u') {
        if (stk->top < 0) { strcpy(err_msg,"Not enough operands for unary minus"); return 0; }
        long long a = ns_pop(stk);
        if (!ns_push(stk, -a)) { strcpy(err_msg,"Value stack overflow"); return 0; }
        return 1;
    }

    if (stk->top < 1) { strcpy(err_msg,"Not enough operands for binary operator"); return 0; }
    long long b = ns_pop(stk);
    long long a = ns_pop(stk);
    long long r = 0;

    switch (op) {
        case '+': r = a + b; break;
        case '-': r = a - b; break;
        case '*': r = a * b; break;
        case '/':
            if (b == 0) { strcpy(err_msg,"Division by zero"); return 0; }
            r = a / b; break; // integer division
        case '%':
            if (b == 0) { strcpy(err_msg,"Modulo by zero"); return 0; }
            r = a % b; break;
        case '^':
            if (!safe_pow_ll(a, b, &r)) { strcpy(err_msg,"Invalid or overflow in exponentiation"); return 0; }
            break;
        default:
            strcpy(err_msg,"Unknown operator in evaluation");
            return 0;
    }
    if (!ns_push(stk, r)) { strcpy(err_msg,"Value stack overflow"); return 0; }
    (void)err_val;
    return 1;
}

int evaluate_postfix(const TokenList *postfix, long long *result, char *err_msg) {
    NumStack stk; ns_init(&stk);

    for (int i = 0; i < postfix->count; ++i) {
        const char *t = postfix->items[i];

        // operator (single char token)
        if (strlen(t) == 1 && is_operator(t[0])) {
            if (!apply_op(t[0], &stk, result, err_msg)) return 0;
            continue;
        }

        // number
        errno = 0;
        char *endptr = NULL;
        long long val = strtoll(t, &endptr, 10);
        if (errno != 0 || endptr == t || *endptr != '\0') {
            strcpy(err_msg,"Invalid number in postfix");
            return 0;
        }
        if (!ns_push(&stk, val)) { strcpy(err_msg,"Value stack overflow"); return 0; }
    }

    if (stk.top != 0) { strcpy(err_msg,"Extra operands or insufficient operators"); return 0; }
    *result = ns_pop(&stk);
    return 1;
}

// -------------------- Utility: join postfix tokens to a printable string --------------------
void print_postfix(const TokenList *postfix) {
    for (int i = 0; i < postfix->count; ++i) {
        // Print unary minus as '~' just for display clarity
        if (strlen(postfix->items[i])==1 && postfix->items[i][0]=='u') putchar('~');
        else fputs(postfix->items[i], stdout);
        if (i + 1 < postfix->count) putchar(' ');
    }
    putchar('\n');
}

// -------------------- Main: interactive single-line evaluator --------------------
int main(void) {
    char line[MAX_EXPR];

    printf("Expression Calculator (integers)\n");
    printf("Supports: + - * / %% ^, parentheses, unary minus\n");
    printf("Examples:\n");
    printf("  -3 + 4*(2-1) ^ 3\n");
    printf("  2*-5 + (7 - -(3))\n");
    printf("Enter expression (or empty line to quit):\n\n");

    while (1) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) break;

        // Trim leading spaces; quit on empty
        int allspace = 1;
        for (char *p=line; *p; ++p) { if (!isspace((unsigned char)*p)) { allspace = 0; break; } }
        if (allspace) break;

        TokenList postfix;
        char err[128] = {0};

        if (!infix_to_postfix(line, &postfix, err)) {
            printf("Error (infix->postfix): %s\n", err);
            continue;
        }

        printf("Postfix: ");
        print_postfix(&postfix);

        long long value = 0;
        if (!evaluate_postfix(&postfix, &value, err)) {
            printf("Error (evaluate): %s\n", err);
            continue;
        }
        printf("Result: %lld\n", value);
    }

    printf("Goodbye!\n");
    return 0;
}

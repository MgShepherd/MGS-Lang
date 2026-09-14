#ifndef _LEXER_H_
#define _LEXER_H_

#include <stdlib.h>

// Using X-Macros to create TokenType enum with a generated t_type_to_string method
#define TOKEN_TYPES                                                                                                    \
  X(T_NONE)                                                                                                            \
  X(T_FUNCTION)                                                                                                        \
  X(T_VAR)                                                                                                             \
  X(T_RETURN)                                                                                                          \
  X(T_I32)                                                                                                             \
  X(T_BOOL)                                                                                                            \
  X(T_TRUE)                                                                                                            \
  X(T_FALSE)                                                                                                           \
  X(T_IDENTIFIER)                                                                                                      \
  X(T_NUMERIC_LIT)                                                                                                     \
  X(T_ARROW)                                                                                                           \
  X(T_LEFT_PAREN)                                                                                                      \
  X(T_RIGHT_PAREN)                                                                                                     \
  X(T_LEFT_CURLY)                                                                                                      \
  X(T_RIGHT_CURLY)                                                                                                     \
  X(T_EQUALS)                                                                                                          \
  X(T_COLON)                                                                                                           \
  X(T_PLUS)                                                                                                            \
  X(T_MINUS)                                                                                                           \
  X(T_LT)                                                                                                              \
  X(T_LTE)                                                                                                             \
  X(T_GT)                                                                                                              \
  X(T_GTE)                                                                                                             \
  X(T_IF)                                                                                                              \
  X(T_ELSE)                                                                                                            \
  X(T_SEMI)

// TODO: Should work out where we want type definitions to live in future
#define X(N) N,
typedef enum { TOKEN_TYPES } TokenType;
#undef X

const char *t_type_to_string(TokenType t);

typedef struct {
  TokenType t_type;
  char *item;
} Token;

typedef struct {
  Token *elements;
  size_t count;
  size_t capacity;
} Tokens;

/*
 * Converts the data into a array of tokens, output into the tokens parameter
 * Expects Tokens to be an uninitialised object, if not any data should be freed before this call
 * Will return 0 on success, 1 on failure (alongside logging the failure cause to stderr)
 * Note: Once tokens has been created, data parameter can be freed as string copies are made for each element
 */
unsigned char lexer_process_tokens(Tokens *tokens, const char *data);

#endif // _LEXER_H_

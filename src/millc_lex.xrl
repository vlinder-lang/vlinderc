Definitions.

WHITESPACE          = [\s\n]+

COLON               = :
COMMA               = ,
DOT                 = \.
EQUALS              = =
FAT_ARROW           = =>
LBRACE              = {
LPAREN              = \(
RBRACE              = }
RPAREN              = \)

ALIAS               = alias
FOREIGN             = foreign
IMPORT              = import
STRUCT              = struct
SUB                 = sub

IDENTIFIER          = [a-zA-Z_][a-zA-Z_0-9]*

STRING_LITERAL      = ".*?"

Rules.

{COLON}             : {token, {colon, TokenLine}}.
{COMMA}             : {token, {comma, TokenLine}}.
{DOT}               : {token, {dot, TokenLine}}.
{EQUALS}            : {token, {equals, TokenLine}}.
{FAT_ARROW}         : {token, {fat_arrow, TokenLine}}.
{LBRACE}            : {token, {lbrace, TokenLine}}.
{LPAREN}            : {token, {lparen, TokenLine}}.
{RBRACE}            : {token, {rbrace, TokenLine}}.
{RPAREN}            : {token, {rparen, TokenLine}}.

{ALIAS}             : {token, {alias, TokenLine}}.
{FOREIGN}           : {token, {foreign, TokenLine}}.
{IMPORT}            : {token, {import, TokenLine}}.
{STRUCT}            : {token, {struct, TokenLine}}.
{SUB}               : {token, {sub, TokenLine}}.

{IDENTIFIER}        : {token, {identifier, TokenLine, TokenChars}}.

{STRING_LITERAL}    : {token, {string_literal, TokenLine, string_literal(TokenChars)}}.

{WHITESPACE}        : skip_token.

Erlang code.

string_literal(Text) ->
    string:substr(Text, 2, string:len(Text) - 2).

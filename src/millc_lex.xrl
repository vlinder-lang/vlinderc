Definitions.

WHITESPACE          = [\s\n]+

LBRACE              = {
RBRACE              = }
LPAREN              = \(
RPAREN              = \)
COLON               = :
COMMA               = ,
DOT                 = \.
FAT_ARROW           = =>
EQUALS              = =

ALIAS               = alias
FOREIGN             = foreign
IMPORT              = import
STRUCT              = struct
SUB                 = sub

IDENTIFIER          = [a-zA-Z_][a-zA-Z_0-9]*

STRING_LITERAL      = ".*?"

Rules.

{LBRACE}            : {token, {lbrace, TokenLine}}.
{RBRACE}            : {token, {rbrace, TokenLine}}.
{LPAREN}            : {token, {lparen, TokenLine}}.
{RPAREN}            : {token, {rparen, TokenLine}}.
{COLON}             : {token, {colon, TokenLine}}.
{COMMA}             : {token, {comma, TokenLine}}.
{DOT}               : {token, {dot, TokenLine}}.
{FAT_ARROW}         : {token, {fat_arrow, TokenLine}}.
{EQUALS}            : {token, {equals, TokenLine}}.

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

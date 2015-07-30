Nonterminals
    alias_decl
    block_expr
    call_expr
    constructor
    constructors0
    decl
    decls0
    expr
    exprs0
    exprs_comma0
    field
    fields0
    import_decl
    module
    module_name
    name
    name_expr
    param
    param_list
    params_comma0
    primary_expr
    string_literal_expr
    struct_decl
    struct_literal_expr
    struct_literal_field
    struct_literal_fields0
    sub_decl
    type_expr
    type_exprs_comma0
    union_decl
.

Terminals
    colon
    comma
    dot
    equals
    fat_arrow
    lbrace
    lparen
    rbrace
    rparen

    alias
    import
    mk
    struct
    sub
    union

    identifier

    string_literal
.

Rootsymbol module.

module_name -> identifier : [token_value('$1')].
module_name -> identifier dot module_name : [token_value('$1') | '$3'].

name -> identifier : {unqualified_name, token_value('$1')}.
name ->
    identifier dot identifier
    : {qualified_name, token_value('$1'), token_value('$3')}.

param_list -> lparen params_comma0 rparen : '$2'.

params_comma0 -> '$empty' : [].
params_comma0 -> param : ['$1'].
params_comma0 -> param comma params_comma0 : ['$1' | '$3'].

param -> identifier colon type_expr : {token_value('$1'), '$3'}.

module -> decls0 : {module, '$1', #{}}.

decls0 -> '$empty' : [].
decls0 -> decl decls0 : ['$1' | '$2'].

decl -> import_decl : '$1'.
decl -> alias_decl : '$1'.
decl -> struct_decl : '$1'.
decl -> union_decl : '$1'.
decl -> sub_decl : '$1'.

import_decl -> import module_name : {import_decl, '$2', #{}}.

alias_decl ->
    alias identifier equals type_expr
    : {alias_decl, token_value('$2'), '$4', #{}}.

struct_decl ->
    struct identifier lbrace fields0 rbrace
    : {struct_decl, token_value('$2'), '$4', #{}}.

fields0 -> '$empty' : [].
fields0 -> field fields0 : ['$1' | '$2'].

field -> identifier colon type_expr : {token_value('$1'), '$3'}.

union_decl ->
    union identifier lbrace constructors0 rbrace
    : {union_decl, token_value('$2'), '$4', #{}}.

constructors0 -> '$empty' : [].
constructors0 -> constructor constructors0 : ['$1' | '$2'].

constructor -> identifier : {token_value('$1'), []}.

sub_decl ->
    sub identifier param_list colon type_expr block_expr
    : {sub_decl, token_value('$2'), '$3', '$5', '$6', #{}}.

exprs0 -> '$empty' : [].
exprs0 -> expr exprs0 : ['$1' | '$2'].

exprs_comma0 -> '$empty' : [].
exprs_comma0 -> expr : ['$1'].
exprs_comma0 -> expr comma exprs_comma0 : ['$1' | '$3'].

expr -> call_expr : '$1'.

call_expr ->
    call_expr lparen exprs_comma0 rparen
    : {call_expr, '$1', '$3', #{}}.
call_expr -> primary_expr : '$1'.

primary_expr -> name_expr : '$1'.
primary_expr -> string_literal_expr : '$1'.
primary_expr -> struct_literal_expr : '$1'.
primary_expr -> block_expr : '$1'.

name_expr -> name : {name_expr, '$1', #{}}.

string_literal_expr ->
    string_literal : {string_literal_expr, token_value('$1'), #{}}.

struct_literal_expr ->
    mk type_expr lbrace struct_literal_fields0 rbrace
    : {struct_literal_expr, '$2', '$4', #{}}.

struct_literal_fields0 -> '$empty' : [].
struct_literal_fields0 -> struct_literal_field : ['$1'].
struct_literal_fields0 -> struct_literal_field comma struct_literal_fields0 : ['$1' | '$3'].

struct_literal_field -> identifier colon expr : {token_value('$1'), '$3'}.

block_expr -> lbrace exprs0 rbrace : {block_expr, '$2', #{}}.

type_exprs_comma0 -> '$empty' : [].
type_exprs_comma0 -> type_expr : ['$1'].
type_exprs_comma0 -> type_expr comma type_exprs_comma0 : ['$1' | '$3'].

type_expr -> name : {name_type_expr, '$1', #{}}.
type_expr ->
    lparen type_exprs_comma0 rparen fat_arrow type_expr
    : {sub_type_expr, '$2', '$5', #{}}.
type_expr -> lparen rparen : {tuple_type_expr, [], #{}}.

Erlang code.

token_value({_Token, _Line, Value}) ->
    Value.

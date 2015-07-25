Nonterminals
    block_expr
    call_expr
    decl
    decls0
    expr
    exprs0
    exprs_comma0
    import_decl
    module
    module_name
    name
    name_expr
    param
    param_list
    params0
    primary_expr
    string_literal_expr
    sub_decl
    type_expr
.

Terminals
    colon
    comma
    dot
    lbrace
    lparen
    rbrace
    rparen

    import
    sub

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

param_list -> lparen params0 rparen : '$2'.

params0 -> '$empty' : [].
params0 -> param params0 : ['$1' | '$2'].

param -> identifier colon type_expr : {token_value('$1'), '$3'}.

module -> decls0 : {module, '$1'}.

decls0 -> '$empty' : [].
decls0 -> decl decls0 : ['$1' | '$2'].

decl -> import_decl : '$1'.
decl -> sub_decl : '$1'.

import_decl -> import module_name : {import_decl, '$2'}.

sub_decl ->
    sub identifier param_list colon type_expr block_expr
    : {sub_decl, token_value('$2'), '$3', '$5', '$6'}.

exprs0 -> '$empty' : [].
exprs0 -> expr exprs0 : ['$1' | '$2'].

exprs_comma0 -> '$empty' : [].
exprs_comma0 -> expr : ['$1'].
exprs_comma0 -> expr comma exprs_comma0 : ['$1' | '$3'].

expr -> call_expr : '$1'.

call_expr ->
    call_expr lparen exprs_comma0 rparen
    : {call_expr, '$1', '$3'}.
call_expr -> primary_expr : '$1'.

primary_expr -> name_expr : '$1'.
primary_expr -> string_literal_expr : '$1'.
primary_expr -> block_expr : '$1'.

name_expr -> name : {name_expr, '$1'}.

string_literal_expr ->
    string_literal : {string_literal_expr, token_value('$1')}.

block_expr -> lbrace exprs0 rbrace : {block_expr, '$2'}.

type_expr -> name : {name_type_expr, '$1'}.
type_expr -> lparen rparen : {tuple_type_expr, []}.

Erlang code.

token_value({_Token, _Line, Value}) ->
    Value.

module Language.Mill.AST where

data ModuleName = ModuleName [String]

data Name
    = UnqualifiedName String
    | QualifiedName ModuleName String

type ParameterList = [Parameter]
data Parameter = Parameter String Type

data Type = NamedType Name

data Module = Module [Decl]

data Decl
    = ImportDecl ModuleName
    | SubDecl String [ParameterList] Type Expr

data Expr
    = BlockExpr [Expr]
    | CallExpr Expr [Expr]
    | NameExpr Name
    | StringLiteralExpr String

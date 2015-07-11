module Language.Mill.AST where

import Language.Mill.AST.ID (TypeID, DeclID, ExprID)

newtype ModuleName = ModuleName [String]
                     deriving (Eq, Show)

data Name
    = UnqualifiedName String
    | QualifiedName ModuleName String
    deriving (Eq, Show)

data Type
    = NamedType TypeID Name
    | SubType TypeID [Type] Type
    | TupleType TypeID [Type]
    deriving (Eq, Show)

type ParameterList = [Parameter]
data Parameter = Parameter String Type
                 deriving (Eq, Show)

data Module = Module [Decl]
              deriving (Eq, Show)

data Decl
    = ImportDecl DeclID ModuleName
    | SubDecl DeclID String ParameterList Type Expr
    | ForeignSubDecl DeclID ForeignLibrary CallingConvention String ParameterList Type
    | AliasDecl DeclID String Type
    | StructDecl DeclID String [Field]
    deriving (Eq, Show)

data Field = Field String Type
             deriving (Eq, Show)

newtype ForeignLibrary = ForeignLibrary String
                         deriving (Eq, Show)
newtype CallingConvention = CallingConvention Name
                            deriving (Eq, Show)

data Expr
    = BlockExpr ExprID [Stmt]
    | CallExpr ExprID Expr [Expr]
    | NameExpr ExprID Name
    | StringLiteralExpr ExprID String
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr
    | DeclStmt Decl
    deriving (Eq, Show)

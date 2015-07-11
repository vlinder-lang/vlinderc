module Language.Mill.AST where

import Language.Mill.AST.ID (ID)
import Language.Mill.Module (ModuleName)

data Name
    = UnqualifiedName String
    | QualifiedName String String
    deriving (Eq, Show)

data Type
    = NamedType ID Name
    | SubType ID [Type] Type
    | TupleType ID [Type]
    deriving (Eq, Show)

type ParameterList = [Parameter]
data Parameter = Parameter ID String Type
                 deriving (Eq, Show)

data Module = Module [Decl]
              deriving (Eq, Show)

data Decl
    = ImportDecl ID ModuleName
    | SubDecl ID String ParameterList Type Expr
    | ForeignSubDecl ID ForeignLibrary CallingConvention String ParameterList Type
    | AliasDecl ID String Type
    | StructDecl ID String [Field]
    deriving (Eq, Show)

data Field = Field String Type
             deriving (Eq, Show)

newtype ForeignLibrary = ForeignLibrary String
                         deriving (Eq, Show)
newtype CallingConvention = CallingConvention Name
                            deriving (Eq, Show)

data Expr
    = BlockExpr ID [Stmt]
    | CallExpr ID Expr [Expr]
    | NameExpr ID Name
    | StringLiteralExpr ID String
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr
    | DeclStmt Decl
    deriving (Eq, Show)

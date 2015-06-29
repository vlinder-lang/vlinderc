module Language.Mill.AST where

newtype ModuleName = ModuleName [String]
                     deriving (Eq, Show)

data Name
    = UnqualifiedName String
    | QualifiedName ModuleName String
    deriving (Eq, Show)

data Type
    = NamedType Name
    | SubType [Type] Type
    | TupleType [Type]
    deriving (Eq, Show)

type ParameterList = [Parameter]
data Parameter = Parameter String Type
                 deriving (Eq, Show)

data Module = Module [Decl]
              deriving (Eq, Show)

data Decl
    = ImportDecl ModuleName
    | SubDecl String ParameterList Type Expr
    | ForeignSubDecl ForeignSource CallingConvention String ParameterList Type
    deriving (Eq, Show)

newtype ForeignSource = ForeignSource String
                        deriving (Eq, Show)
newtype CallingConvention = CallingConvention Name
                            deriving (Eq, Show)

data Expr
    = BlockExpr [Stmt]
    | CallExpr Expr [Expr]
    | NameExpr Name
    | StringLiteralExpr String
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr
    | DeclStmt Decl
    deriving (Eq, Show)

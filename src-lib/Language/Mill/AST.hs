module Language.Mill.AST where

newtype ModuleName = ModuleName [String]
                     deriving (Eq, Show)

class ID a where
    idFromInt :: Int -> a
    idToInt :: a -> Int

newtype TypeID = TypeID Int
                 deriving (Show)

instance ID TypeID where
    idFromInt = TypeID
    idToInt (TypeID id) = id

newtype DeclID = DeclID Int
                 deriving (Show)

instance ID DeclID where
    idFromInt = DeclID
    idToInt (DeclID id) = id

newtype ExprID = ExprID Int
                 deriving (Show)

instance ID ExprID where
    idFromInt = ExprID
    idToInt (ExprID id) = id

instance Eq TypeID where
    (==) = idEq

instance Eq DeclID where
    (==) = idEq

instance Eq ExprID where
    (==) = idEq

idEq :: ID a => a -> a -> Bool
a `idEq` b = idToInt a == (-1) || idToInt b == (-1) || idToInt a == idToInt b

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

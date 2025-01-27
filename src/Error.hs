module Error where

data Error = UnexpectedEOF
    | BadImpl String
    | NoSuchVar String
    | NoSuchFunc String
    | BadValue String
    | IllegalGlobal
    | NoMainFunc
    | DuplicateBinding String
    | IncorrectNumberOfArgs
    | UnexpectedToken String
    | UnknownError
    deriving Show


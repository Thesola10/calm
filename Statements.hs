module Statements(  StatementC(SSet, SList, SFile, SText, SBool, SInt, SFloat, SFunctor, SScoped, SToken, SNull),
                    SFilePath(FLocal, FAbsolute, FUri, FPath),
                    UQText(TextPart, StatementPart),
                    Protocol(Http, Https, Ftp, Sftp, Git),
                    Scope, Statement, Token,
                    apply, getType) where

import Types

data StatementC = SSet      [ (String, Statement) ] -- { a = ...; } (recursive)
                | SList     [ Statement ]           -- [ ... ]      (recursive)
                | SFile     SFilePath               -- ./txt https:// /usr <path>
                | SText     [ UQText ]              -- "text" ''text'' "t${a}" ''t${a}''
                | SBool     Bool                    -- true false
                | SInt      Int                     -- 1 -2 3
                | SFloat    Float                   -- 1.1 -3.14
                | SFunctor  Token Statement         -- a: a + a
                | SScoped   Scope Statement         -- let ... in, with, f x
                | SToken    Token                   -- a
                | SNull                             -- null

data SFilePath  = FLocal    String                  -- ./file
                | FAbsolute String                  -- /usr/bin
                | FUri      Protocol String         -- https://lol
                | FPath     String                  -- <this/that>

data UQText     = TextPart      String              -- normal text
                | StatementPart Statement           -- "${statement}"

data Protocol   = Http | Https | Ftp | Sftp | Git

type Scope      = [ (String, Statement) ]           -- current key/value variables

type Statement  = (Type, StatementC)
type Token      = (Type, String)

-- |Apply an unbound token or a functor to a scope.
-- This function traverses Statements recursively until it finds functors,
-- and just returns its parameter if already applied.
apply :: Statement -> Statement
apply (_, SFile p) = (Bare TFile, SFile p)
apply (_, SText l) = (Bare TText, SText (map sapply l))
                    where sapply (StatementPart s) = StatementPart (apply s) ;
                          sapply (TextPart t) = TextPart t
apply (_, SBool b) = (Bare TBool, SBool b)
apply (_, SInt  n) = (Bare TInt, SInt n)
apply (_, SFloat f) = (Bare TFloat, SFloat f)
apply (_, SNull) = (Bare TNull, SNull)

apply (t, SSet l) = (t, SSet (map applyv l))
                    where applyv (n, s) = (n, apply s)
apply (t, SList l) = (t, SList (map apply l))
apply _ = error "Not implemented"

-- |Construct a type from a given statement.
getType :: StatementC -> Type
getType (SFile _) = Bare TFile
getType (SText _) = Bare TText
getType (SBool _) = Bare TBool
getType (SInt _) = Bare TInt
getType (SFloat _) = Bare TFloat
getType SNull = Bare TNull
getType (SFunctor (tt, _) (st, sc)) = Application tt st
getType (SSet c) = Bare (TSet (map toType c))
                where toType (n, (t, _)) = (n, t)
getType (SList ((t,_):_)) = Bare (TList t)
getType _ = error "Not implemented."

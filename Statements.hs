import Types

data StatementC = SSet      [ (String, Statement) ] -- { a = ...; } (recursive)
                | SList     [ Statement ]           -- [ ... ]      (recursive)
                | SFile     SFilePath               -- ./txt https:// /usr <path>
                | SText     [ UQText ]              -- "text" ''text'' "t${a}" ''t${a}''
                | SBool     Bool                    -- true false
                | SInt      Int                     -- 1 -2 3
                | SFloat    Float                   -- 1.1 -3.14
                | SFunctor  String Statement        -- a: a + a
                | SScoped   Scope Statement         -- let ... in, with, f x
                | SToken    String                  -- a
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


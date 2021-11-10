module Types(   Type(Bare, Application, Wide),
                TypeAtom(TSet, TList, TFile, TText, TBool, TInt, TFloat, TNull, TNamed)) where

data Type       = Application   Type Type           -- T -> (...)
                | Bare          TypeAtom            -- T
                | Wide          [ TypeAtom ]        -- (T or T)

data TypeAtom   = TNamed String Type                -- T :: ...;
                | TList Type                        -- ([T])
                | TSet  [ (String, Type) ]          -- { (T) a, (T) b }
                | TBool                             -- (Bool)
                | TNull                             -- (Null)
                | TFloat                            -- (Float)
                | TInt                              -- (Int)
                | TFile                             -- (File)
                | TText                             -- (Text)

instance Eq Type where
    (Bare a)            == (Bare b)             = a == b
    (Application a1 t1) == (Application a2 t2)  = (a1 == a2) && (t1 == t2)
    (Wide l1)           == (Wide l2)            = and $ zipWith (==) l1 l2
    _                   == _                    = False

instance Ord Type where
    Wide _              <=  Bare _              = False
    Bare _              <=  Application _ _     = False

    Bare t1             <=  Bare t2             = t2 < t1
    Application a1 t1   <=  Application a2 t2   = (a2 < a1) || (t2 < t1)
    Wide l1             <=  Wide l2             = length l2 < length l1
                                                  || and (zipWith (<) l2 l1)

    _ <= _ = True

instance Eq TypeAtom where
    TList t1        == TList t2     = t1 == t2
    TNamed n1 t1    == TNamed n2 t2 = (n1 == n2) && (t1 == t2)
    TSet l1         == TSet l2      = and $ zipWith tcompare l1 l2
                                      where tcompare (s1, t1) (s2, t2) = (s1 == s2) && (t1 == t2)
    TFile   ==  TFile   = True
    TText   ==  TText   = True
    TBool   ==  TBool   = True
    TInt    ==  TInt    = True
    TFloat  ==  TFloat  = True
    TNull   ==  TNull   = True
    _       ==  _       = False

instance Ord TypeAtom where
    TText       <=  TFile       = False
    TFile       <=  TInt        = False
    TInt        <=  TFloat      = False
    TFloat      <=  TNull       = False
    TNull       <=  TBool       = False
    TBool       <=  TSet _      = False
    TSet _      <=  TList _     = False
    TList _     <=  TNamed _ _  = False

    TList t1    <=  TList t2    = t2 < t1
    TSet l1     <=  TSet l2     = length l2 > length l1 || and (zipWith tsort l2 l1)
                                  where tsort (_, t1) (_, t2) = t1 < t2

    _           <=  _           = True

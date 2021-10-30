module Types(Type, TypeAtom) where

data Type       = Bare          TypeAtom            -- T
                | Application   TypeAtom Type       -- T -> (...)
                | Wide          [ TypeAtom ]        -- (T or T)

data TypeAtom   = TSet  [ (String, Type) ]          -- { (T) a, (T) b }
                | TList Type                        -- ([T])
                | TFile                             -- (File)
                | TText                             -- (Text)
                | TBool                             -- (Bool)
                | TInt                              -- (Int)
                | TFloat                            -- (Float)
                | TNull                             -- (Null)
                | TNamed String Type                -- T :: ...;


instance Eq Type where
    (Bare a)            == (Bare b)             = a == b
    (Application a1 t1) == (Application a2 t2)  = (a1 == a2) && (t1 == t2)
    (Wide l1)           == (Wide l2)            = and $ zipWith (==) l1 l2
    _                   == _                    = False

instance Ord Type where
    Application _ _                 < Bare _                    = True
    Bare _                          < Wide _                    = True
    Bare t1                         < Bare t2                   = t1 < t2
    Application _ (Application _ _) < Application _ (Bare _)    = True
    Application a1 t1               < Application a2 t2         = (a1 < a2) || (t1 < t2)
    Wide l1                         < Wide l2                   = length l1 < length l2
                                                                || and (zipWith (<) l1 l2)

    a < b = not (b <= a)
    a <= b = (a == b) || (a < b)


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
    TNamed _ _  <   TList _     = True
    TList _     <   TSet _      = True
    TSet _      <   TBool       = True
    TBool       <   TNull       = True
    TNull       <   TFloat      = True
    TFloat      <   TInt        = True
    TInt        <   TFile       = True
    TFile       <   TText       = True

    TList t1    <   TList t2    = t1 < t2
    TSet l1     <   TSet l2     = length l1 > length l2 || and (zipWith tsort l1 l2)
                                where tsort (_, t1) (_, t2) = t1 < t2

    a < b = not (b <= a)
    a <= b = (a == b) || (a < b)


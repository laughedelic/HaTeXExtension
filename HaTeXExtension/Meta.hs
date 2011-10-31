{-# LANGUAGE TemplateHaskell #-}

-- | This module offers templates, that HaTeXExtension uses to simplify declarations of
--   additional commands
module HaTeXExtension.Meta 
    ( defTeXCommand
    , defFusedOperators
    )
where

import Text.LaTeX.Base.Syntax
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import HaTeXExtension.Meta.Sugar

{-| Template @ defTeXCommand \"foo\" \"bar\" [\"+\#\&\",\"qux\"] @ declares

    * value @foo@, that represents TeX command @bar@ without arguments:

@ foo :: `LaTeX'
 foo = TeXComm \"bar\" []
@

    * operator:

@ (+\#\&) :: `LaTeX' -> `LaTeX' -> `LaTeX'
 a +#& b = a \<\> foo \<\> b
@

    * function:

@ qux :: `LaTeX' -> `LaTeX' -> `LaTeX'
 a \`qux\` b = a \<\> foo \<\> b
@

    Template can take any count of such operators-synonims.
    If first argument is @\"\"@, then function name will be 
    the same as second argument - command name.
    This is top-level declaration, so it can be used without 
    special splicing @$(...)@ syntax.
-}
defTeXCommand ::  String   -- ^ function name
              ->  String   -- ^ TeX command name
              -> [String]  -- ^ operator names
              ->  Q [Dec]  -- ^ top-level declaration
defTeXCommand  ""  comm ops = defTeXCommand comm comm ops
defTeXCommand name comm ops = 
    qDecs $ [ name ^:: [t| LaTeX |]
            , name ^= [| TeXComm comm [] |]
            ] ++ (concatMap opDec ops)
    where
        opDec op = [ op ^:: [t| LaTeX -> LaTeX -> LaTeX |]
                   , op ^= [| \a b -> a <> $(dyn name) <> b |]
                   ]

{-| Template @defFusedOperators [\"< ... <\"]@ declares
    operators, that are combinations of several others:

@ \<\...\< :: `LaTeX' -> `LaTeX' -> `LaTeX'
 a \<\...\< b = a \< \"\" \... \"\" \< b
@

    This is top-level declaration, so it can be used without 
    special splicing @$(...)@ syntax.
-}
defFusedOperators :: [String]   -- ^ strings with operator names
                   -> Q [Dec]   -- ^ top-level declaration
defFusedOperators list = 
    qDecs [ concat ops ^= body ops | ops <- (map words list) ]
    where 
        body, fuse :: [String] -> Q Exp
        body ops = [| \a b -> a <> $(fuse ops) <> b |]

        fuse   []   = [| "" |]
        fuse (x:xs) = [| $(dyn x) "" "" <> $(fuse xs) |]


{--
Different experiments with templates

defTeXCommand name comm ops = 
    qDecs $ [ name ^:: [t| LaTeX |]
            , name ^= body
            ] ++ (concatMap opDec ops)
    where
        body = [| TeXComm comm [] |]
        opDec op = [ op ^:: [t| LaTeX -> LaTeX -> LaTeX |]
                   , op ^= [| \a b -> a <> $body <> b |]
                   ]

defTeXCommand name comm = do
    let name' = mkName name
    body <- [| TeXComm comm [] |]
    return [ SigD name' (ConT ''LaTeX)                -- name :: LaTeX
           , FunD name' [Clause [] (NormalB body) []] -- name = TeXComm comm []
           ]
                          
--}

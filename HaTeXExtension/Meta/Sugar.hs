{-# LANGUAGE TemplateHaskell #-}

-- | This module defines some nice syntax for defining templates.
--   Look for examples in the source of HaTeXExtension.Meta module.
--   It is not exported by package.
module HaTeXExtension.Meta.Sugar where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Data.Generics.Aliases
import Data.Generics.Schemes

(^::) :: String -> TypeQ -> DecQ
name ^:: typeQ = sigD (mkName name) typeQ

(^=) :: String -> ExpQ -> DecQ
name ^= bodyQ = valD (varP (mkName name)) (normalB bodyQ) []
                      
qDecs :: [Q Dec] -> Q [Dec]
qDecs = mapM id

-------------------------------------------------------------------------
{- experiments with name substitution in templates


dec :: Q Dec
dec = [d| foo :: Num a => a -> a -> String
          foo x 1 = show $ x + y
          foo x 2 = show $ x - z
          foo x _ = show x ++ blah
        |]

changeName :: Name -> Dec -> Dec
changeName name (SigD  _  typ) = 
                SigD name typ
changeName name (FunD  _  clauses) = 
                FunD name clauses
changeName name (DataD cxt  _  tyVarBndr con dervs) =
                DataD cxt name tyVarBndr con dervs
changeName name (NewtypeD cxt  _  tyVarBndr con dervs) =
                NewtypeD cxt name tyVarBndr con dervs
changeName name (TySynD  _  tyVarBndr typ) = 
                TySynD name tyVarBndr typ
changeName name (ClassD cxt  _  tyVarBndr funDep dec) = 
                ClassD cxt name tyVarBndr funDep dec
changeName _ dec = dec -- yeah, these are not all cases with Name parameter
                       -- but I don't know them and don't want to change therefore

quux :: String -> Int -> Int -> String -> DecsQ
quux name y z blah = (liftM . map) (changeName (mkName name)) dec
    

--------------------------------------------------------
substName :: String -> Name -> Name
substName bar foo | nameBase foo == "foo" = mkName bar
                  | otherwise = foo

substName' :: String -> Dec -> Dec
substName' name = everywhere (mkT (substName name))

quuux :: String -> Int -> Int -> String -> DecsQ
quuux name y z blah = (liftM . map) (substName' name) dec

-}

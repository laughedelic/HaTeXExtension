{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides those TeX commands, 
-- that are usually used in math mode.
-- Also it makes `LaTeX' type an instance 
-- of `Num' and `Frac' classes and derives `Eq` instance.
module HaTeXExtension.Math 
    ( 
    -- *** @in@ command
      in_, from, (∈)
    -- *** @notin@ command
    , notin, notFrom, (∉)

    -- ** Parenthesis, brackets and braces
    , left, right, leftRight
    , parenthesis, prn, brackets, braces

    -- *** Application operator
    , (.:), (⁖)

    -- ** Compare commands
    , (==:)
    -- *** @leq@ command
    , leq, (<=:), (≤)
    -- *** @geq@ command
    , geq, (>=:), (≥)

    -- ** Subscript and superscript
    , (⁀), (‿)

    -- ** Other symbols and operators
    , frac, forall, (⎕)
    )
where

import Text.LaTeX
import Text.LaTeX.Base.Syntax hiding (braces)

import HaTeXExtension.Meta

import Data.Ratio (numerator, denominator)


------------------------------------------------------------------
-- Why not?
deriving instance Eq TeXArg
deriving instance Eq LaTeX

-- Old HaTeX had this instance, and it is useful, as for me
instance Num LaTeX where
    (+) = TeXOp "+"
    (-) = TeXOp "-"
    (*) = (<>)
    negate = (TeXCommS "not" <>)
    fromInteger = fromString . show
    -- don't know how to define this:
    abs = undefined
    signum = undefined
    
-- | Just \\frac comand
frac ::  LaTeX -> LaTeX -> LaTeX
frac a b = TeXComm "frac" [FixArg a, FixArg b]
-- may be there should be also \cfrac and \dfrac

-- This is also as useful as Num instance
instance Fractional LaTeX where
    (/) = frac
    fromRational r = let a = fromInteger $ numerator r
                         b = fromInteger $ denominator r
                     in a/b

------------------------------------------------------------------
-- * Parenthesis, brackets and braces
--
-- | @\\left@ modificator command
left :: Char -> LaTeX
left l = TeXCommS "left" <> fromString [l]

-- | @\\right@ modificator command
right :: Char -> LaTeX
right r = TeXCommS "right" <> fromString [r]

-- | combines 'left' and 'right' functions to surround given statement
leftRight ::  (Char, Char) -> LaTeX -> LaTeX
leftRight (l,r) stmt = left l <> " " <> stmt <> " " <> right r

-- | Encloses expression in @\\left(@ and @\\right)@
parenthesis :: LaTeX -> LaTeX
parenthesis = leftRight ('(',')')

-- | Shothand for @parenthesis@ function
prn :: LaTeX -> LaTeX
prn = parenthesis

-- | Encloses expression in @\\left[@ and @\\right]@
brackets :: LaTeX -> LaTeX
brackets = leftRight ('[',']')

-- | Encloses expression in @\\left{@ and @\\right}@
braces :: LaTeX -> LaTeX
braces = leftRight ('{','}')

------------------------------------------------------------------
-- | Application operator - encloses second arg in parenthesis. 
-- Example: @f.:(x + y)@ means @f \\left( x + y \\right)@ in TeX.
(.:) :: LaTeX -> LaTeX -> LaTeX
func .: params = func <> prn params

-- | Unicode synonym for application operator @(.:)@ (Unicode: U+2056)
(⁖) :: LaTeX -> LaTeX -> LaTeX
(⁖) = (.:)

------------------------------------------------------------------
-- | @from@ and @(&#x2208)@ (Unicode: U+2208) are operators 
-- for @in_@ command
-- (not @in@ because it is haskell keyword)
defTeXCommand "in_" "in" ["∈", "from"]

-- | @notFrom@ and @(&#x2209)@ (Unicode: U+2209) are operators 
-- for @notin@ command
defTeXCommand "" "notin" ["∉", "notFrom"]

------------------------------------------------------------------
-- * Compare commands
-- | Just equal sign @\"=\"@ as operator
(==:) :: LaTeX -> LaTeX -> LaTeX
(==:) = TeXOp "="

-- | @(<=:)@ and @(&#x2264)@ (Unicode: U+2264) are operators 
-- for @leq@ command
defTeXCommand "" "leq" ["<=:", "≤"] 

-- | @(>=:)@ and @(&#x2265)@ (Unicode: U+2265) are operators 
-- for @geq@ command
defTeXCommand "" "geq" [">=:", "≥"] 


------------------------------------------------------------------
-- * Subscript and superscript
-- a‿i⁀2
-- | Just a synonym for @(^:)@ superscript operator (Unicode: U+2040)
(⁀) :: LaTeX -> LaTeX -> LaTeX
(⁀) = (^:)

-- | Just a synonym for @(!:)@ subscript operator (Unicode: U+203F)
(‿) :: LaTeX -> LaTeX -> LaTeX
(‿) = (!:)

------------------------------------------------------------------
-- | Just symbol @\forall@
defTeXCommand "" "forall" []

-- | Math medium space command @\\;@ 
-- that is 5/18 of @quad@ (Unicode: U+2395)
(⎕) :: LaTeX -> LaTeX -> LaTeX
a ⎕ b = a <> TeXCommS ";" <> b


------------------------------------------------------------------
-- ❨a❩ ⦅a⦆ (a) ⨭a⨮   ⨴a⨵ 
-- a ⨴ b = a <> left '(' <> b
-- a ⨵ b = a <> right ')' <> b

a ⨴ b = a <> " " <> math b
a ⨵ b = a <> b
-- infixl 3 ⨴
infixl 8 ⨵

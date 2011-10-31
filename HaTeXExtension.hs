{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides some additional commands 
-- and operators for HaTeX.
module HaTeXExtension
    ( 
    -- * Math module
    module HaTeXExtension.Math

    -- * Space commands
    , (□)
    -- *** @quad@ command
    , quad, (⎕⎕), space
    -- *** @qquad@ command
    , qquad, (⎕⎕⎕⎕), space2

    -- * Other symbols and commands
    -- *** @dots@ command
    , dots, (...)
    -- *** Other operators
    , (¸), (⤸)
    )
where

import Text.LaTeX
import Text.LaTeX.Base.Syntax

import HaTeXExtension.Meta
import HaTeXExtension.Math

------------------------------------------------------------------
-- * Space commands
--
-- | Simple space @\" \"@ as operator (Unicode: U+25A1)
(□) :: LaTeX -> LaTeX -> LaTeX
a □ b = a <> " " <> b
-- alternative symbols: ▢ ▫

-- | @space@ and @(&#x2395;&#x2395;)@ (Unicode: U+2395) 
-- are operators for @quad@ command
defTeXCommand "" "quad" ["⎕⎕", "space"]

-- | @space2@ and @(&#x2395;&#x2395;&#x2395;&#x2395;)@ (Unicode: U+2395) 
-- are operators for @qquad@ command
defTeXCommand "" "qquad" ["⎕⎕⎕⎕", "space2"]

------------------------------------------------------------------
-- * Other symbols and commands
--
-- | @(...)@ is operator for @dots@ command
defTeXCommand "" "dots" ["..."]

-- | @(&#x2938)@ is operator for @newline@ command (Unicode: U+2938)
(⤸) :: LaTeX -> LaTeX -> LaTeX
a ⤸ b = a <> newline <> b
-- alternative symbols: ⤸ ⤾  ⤦  ↵ ↩ ↲

-- | Just comma symbol @,@ as operator (Unicode: U+00B8)
(¸) :: LaTeX -> LaTeX -> LaTeX
(¸) = TeXOp ", "


enum n x = x‿1 ¸ dots ¸ x‿n

enumN x = x‿1 ¸ dots ¸ x‿"n"

enumm n x i = x‿(i‿1) ¸ dots ¸ x‿(i‿n)

defFusedOperators ["¸ ⎕"]

-- | Takes a string of format @\"a b c\"@ and 
-- returns list @[\"a\",\"b\",\"c\"] :: [LaTeX]@.
-- It can be used like that: 
--
-- @ let [x,y,z] = vars \"x y z\"
-- in math (x*2 + (y-1) = z)@
vars :: String -> [LaTeX]
vars = map fromString . words
-- TODO: template for generating such declarations.
-- something like @ let $(defLocalVars "x y z") in ... @

-----------------------------------------------------
--
-- ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊝ ⚀ ⚗ ❨⠕ ⠪ ⨱ ⨾⸪ ⸫ ꁞꇤ꒰꒱꒲꒺꒪꒫꜊꜏  
-- a⃕ a⃑ a⃗ .⃗

-- 
-- justImage :: [LaTeX] -> FilePath -> LaTeX
-- justImage opts f = comm4 "includegraphics" opts (fromString f)

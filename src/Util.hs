{-# LANGUAGE LambdaCase #-}

module Util
  ( q
  , qc
  , qq
  , deindent
  ) where

import           Data.List                     (intercalate)
import           Language.Haskell.TH.Quote
import qualified Text.InterpolatedString.Perl6 as P

-- | Identical to Text.InterpolatedString.Perl6.q except that the string is
-- deindented and stripped of its leading newline (if any) first.
q :: QuasiQuoter
q = deindentQQ P.q

-- | Identical to Text.InterpolatedString.Perl6.qc except that the string is
-- deindented and stripped of its leading newline (if any) first.
qc :: QuasiQuoter
qc = deindentQQ P.qc

-- | Identical to Text.InterpolatedString.Perl6.qq except that the string is
-- deindented and stripped of its leading newline (if any) first.
qq :: QuasiQuoter
qq = deindentQQ P.qq

-- | Deindent and strip leading newline from the input to a 'QuasiQuoter'
deindentQQ :: QuasiQuoter -> QuasiQuoter
deindentQQ quasiQuoter = QuasiQuoter
  { quoteExp = quoteExp quasiQuoter . stripLeadingNewline . deindent
  , quotePat = quotePat quasiQuoter . stripLeadingNewline . deindent
  , quoteType = quoteType quasiQuoter . stripLeadingNewline . deindent
  , quoteDec = quoteDec quasiQuoter . stripLeadingNewline . deindent
  }

stripLeadingNewline :: String -> String
stripLeadingNewline = \case
  '\n':s -> s
  s -> s

-- | Remove a number of characters from the start of each line equal the the
-- minimum indentation of the string. Useful for formatting the 'qc' splices in
-- a more natural way
deindent :: String -> String
deindent s = intercalate "\n" . fmap (drop indentLength) $ sLines
  where sLines = lines s
        indentLength = minimum (indent <$> filter (any (/= ' ')) sLines)
        indent = length . takeWhile (== ' ')

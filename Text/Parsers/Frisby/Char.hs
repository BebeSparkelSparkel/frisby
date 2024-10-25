{-# LANGUAGE FlexibleContexts #-}
-- | Unicode character parsers. The character classification is identical to the
-- classification in the "Data.Char" module.
module Text.Parsers.Frisby.Char where

import Data.Char
import Text.Parsers.Frisby (P, anyChar)
import qualified Text.Parsers.Frisby
import Data.MonoTraversable (Element)
import Text.Printf (IsChar(toChar))

onlyIf :: IsChar (Element mono) => P s mono (Element mono) -> (Char -> Bool) -> P s mono (Element mono)
onlyIf x f = x `Text.Parsers.Frisby.onlyIf` (f . toChar)

-- | Match a control character.
control :: IsChar (Element mono) => P s mono (Element mono)
control = anyChar `onlyIf` isControl

-- | Match a white-space character in the Latin-1 range.
space :: IsChar (Element mono) => P s mono (Element mono)
space = anyChar `onlyIf` isSpace

-- | Match a lower-case alphabetic Unicode character.
lower :: IsChar (Element mono) => P s mono (Element mono)
lower = anyChar `onlyIf` isLower

-- | Match an upper-case or title-case alphabetic Unicode character.
upper :: IsChar (Element mono) => P s mono (Element mono)
upper = anyChar `onlyIf` isUpper

-- | Match an alphabetic Unicode character. Equivalent to 'letter'.
alpha :: IsChar (Element mono) => P s mono (Element mono)
alpha = anyChar `onlyIf` isAlpha

-- | Match an alphabetic or numeric digit Unicode character.
alphaNum :: IsChar (Element mono) => P s mono (Element mono)
alphaNum = anyChar `onlyIf` isAlphaNum

-- | Match a printable Unicode character.
printable :: IsChar (Element mono) => P s mono (Element mono)
printable = anyChar `onlyIf` isPrint

-- | Match an ASCII digit.
digit :: IsChar (Element mono) => P s mono (Element mono)
digit = anyChar `onlyIf` isDigit

-- | Match an ASCII octal digit.
octDigit :: IsChar (Element mono) => P s mono (Element mono)
octDigit = anyChar `onlyIf` isOctDigit

-- | Match an ASCII hexadecimal digit.
hexDigit :: IsChar (Element mono) => P s mono (Element mono)
hexDigit = anyChar `onlyIf` isHexDigit

-- | Match an alphabetic Unicode character. Equivalent to 'alpha'.
letter :: IsChar (Element mono) => P s mono (Element mono)
letter = anyChar `onlyIf` isLetter

-- | Match a Unicode mark character.
mark :: IsChar (Element mono) => P s mono (Element mono)
mark = anyChar `onlyIf` isMark

-- | Match a Unicode numeric character.
number :: IsChar (Element mono) => P s mono (Element mono)
number = anyChar `onlyIf` isNumber

-- | Match a Unicode punctuation character.
punctuation :: IsChar (Element mono) => P s mono (Element mono)
punctuation = anyChar `onlyIf` isPunctuation

-- | Match a Unicode symbol character.
symbol :: IsChar (Element mono) => P s mono (Element mono)
symbol = anyChar `onlyIf` isSymbol

-- | Match a Unicode space or separator character.
separator :: IsChar (Element mono) => P s mono (Element mono)
separator = anyChar `onlyIf` isSeparator

-- | Match a character of the ASCII character set.
ascii :: IsChar (Element mono) => P s mono (Element mono)
ascii = anyChar `onlyIf` isAscii

-- | Match a character of the ISO 8859-1 (Latin-1) character set.
latin1 :: IsChar (Element mono) => P s mono (Element mono)
latin1 = anyChar `onlyIf` isLatin1

-- | Match an ASCII upper-case letter.
asciiUpper :: IsChar (Element mono) => P s mono (Element mono)
asciiUpper = anyChar `onlyIf` isAsciiUpper

-- | Match an ASCII lower-case letter.
asciiLower :: IsChar (Element mono) => P s mono (Element mono)
asciiLower = anyChar `onlyIf` isAsciiLower

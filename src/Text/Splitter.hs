{-# LANGUAGE RankNTypes #-}

module Text.Splitter (split) where

import Text.Parsec
import Data.Maybe (isJust)
import Control.Monad (msum, join)

type ParsecParser a = forall x . Parsec String x a

type Splitter = ParsecParser [String]

split = parse spl ""

spl :: Splitter
spl =  many (try section) <* trailing

begin :: ParsecParser String
begin = string "/*<begin#*/"

end   :: ParsecParser String
end   = string "/*#end>*/"

trailing :: ParsecParser String
trailing = many anyChar

section :: ParsecParser String
section =  manyTill anyChar (try begin) *> manyTill anyChar (try end)

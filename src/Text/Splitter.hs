{-# LANGUAGE RankNTypes #-}

module Text.Splitter (split) where

import Text.Parsec
import Data.Maybe (isJust)
import Control.Monad (msum, join)

type ParsecParser a = forall x . Parsec String x a

type Splitter = ParsecParser [[String]]

split = parse spl ""

spl :: Splitter
spl =  many (try section) <* trail

begin :: ParsecParser String
begin = string "/*<" *> manyTill anyChar (try $ string "#*/")

end   :: ParsecParser String
end   = string "/*#" *> manyTill anyChar (try $ string ">*/")

trail :: ParsecParser ()
trail = many anyChar *> return ()


trailing :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a] -> ParsecT s u m [a]
trailing p end      = scan
                    where
                      scan  = end <|> (p *> scan *> return [])


section :: ParsecParser [String]
section =  do
  tag <- trailing anyChar (try begin)
  body <- manyTill anyChar (try end)
  return [tag, body]

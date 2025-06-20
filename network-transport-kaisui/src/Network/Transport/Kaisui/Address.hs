module Network.Transport.Kaisui.Address
  ( parseAddress
  ) where

import Data.Convertible
import Network.Socket
import Network.Transport
import RIO hiding (some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parse address format: "host:port:id"
-- Supports IPv6 addresses in bracket notation: "[::1]:port:id"
--
-- NOTE: Current implementation accepts any string inside brackets,
-- not just valid IPv6 addresses. This is a known limitation.
-- For example, "[invalid]:8080:1" will be parsed successfully.
parseAddress :: EndPointAddress -> Either (ParseErrorBundle Text Void) (Text, PortNumber, Word32)
parseAddress (EndPointAddress addr) =
  parse addressParser "" (convert addr :: Text)

-- | Parser for address format
addressParser :: Parsec Void Text (Text, PortNumber, Word32)
addressParser =
  (,,)
    <$> label "host" hostParser
    <* char ':'
    <*> label "port" (fromIntegral <$> (L.decimal :: Parsec Void Text Word))
    <* char ':'
    <*> label "endpoint id" L.decimal
    <* eof

-- | Parser for host part (supports IPv6 in brackets)
hostParser :: Parsec Void Text Text
hostParser =
  try ipv6InBrackets <|> regularHost
 where
  -- Parse IPv6 address in brackets like [::1] or [2001:db8::1]
  ipv6InBrackets = do
    openBracket <- char '['
    addr <- takeWhile1P (Just "IPv6 address") (/= ']') -- takeWhile1P to ensure non-empty
    closeBracket <- char ']'
    pure $ convert [openBracket] <> addr <> convert [closeBracket]

  -- Parse regular hostname or IPv4 address
  regularHost = do
    host <- takeWhile1P (Just "hostname") (\c -> c /= ':' && c /= '[' && c /= ']')
    -- Reject if the hostname is just brackets
    if host == "[]"
      then fail "Empty brackets are not a valid hostname"
      else pure host

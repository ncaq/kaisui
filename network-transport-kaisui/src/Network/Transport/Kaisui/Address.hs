module Network.Transport.Kaisui.Address
  ( parseAddress
  ) where

import Data.Convertible
import Network.Socket
import Network.Transport
import RIO hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parse address format: "host:port:id"
parseAddress :: EndPointAddress -> Either (ParseErrorBundle Text Void) (Text, PortNumber, Word32)
parseAddress (EndPointAddress addr) =
  parse addressParser "" (convert addr :: Text)

-- | Parser for address format
addressParser :: Parsec Void Text (Text, PortNumber, Word32)
addressParser =
  (,,)
    <$> label "host" (takeWhile1P Nothing (/= ':'))
    <* char ':'
    <*> label "port" (fromIntegral <$> (L.decimal :: Parsec Void Text Word))
    <* char ':'
    <*> label "endpoint id" L.decimal
    <* eof

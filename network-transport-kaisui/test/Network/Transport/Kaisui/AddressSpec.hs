module Network.Transport.Kaisui.AddressSpec
  ( spec
  ) where

import Control.Monad (replicateM)
import Data.Convertible
import Network.Transport
import Network.Transport.Kaisui.Address
import Numeric (showHex)
import RIO
import qualified RIO.List as L
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "parseAddress" $ do
  describe "Valid addresses" $ do
    it "should parse simple localhost address" $ do
      let addr = EndPointAddress "localhost:8080:1"
      parseAddress addr `shouldBe` Right ("localhost", 8080, 1)

    it "should parse IPv4 address" $ do
      let addr = EndPointAddress "192.168.1.1:9999:42"
      parseAddress addr `shouldBe` Right ("192.168.1.1", 9999, 42)

    it "should parse IPv6 address with brackets" $ do
      let addr = EndPointAddress "[::1]:3000:0"
      parseAddress addr `shouldBe` Right ("[::1]", 3000, 0)

    it "should parse domain names" $ do
      let addr = EndPointAddress "example.com:443:100"
      parseAddress addr `shouldBe` Right ("example.com", 443, 100)

    it "should parse addresses with hyphens" $ do
      let addr = EndPointAddress "my-server.example.com:5000:7"
      parseAddress addr `shouldBe` Right ("my-server.example.com", 5000, 7)

  describe "IPv6 addresses" $ do
    it "should parse various IPv6 formats" $ do
      -- Loopback
      parseAddress (EndPointAddress "[::1]:8080:42")
        `shouldBe` Right ("[::1]", 8080, 42)

      -- Unspecified address
      parseAddress (EndPointAddress "[::]:9000:1")
        `shouldBe` Right ("[::]", 9000, 1)

      -- Full IPv6 address
      parseAddress (EndPointAddress "[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443:99")
        `shouldBe` Right ("[2001:db8:85a3:8d3:1319:8a2e:370:7348]", 443, 99)

      -- IPv6 with zeros omitted
      parseAddress (EndPointAddress "[2001:db8::1]:80:0")
        `shouldBe` Right ("[2001:db8::1]", 80, 0)

      -- IPv4-mapped IPv6
      parseAddress (EndPointAddress "[::ffff:192.168.1.1]:3000:5")
        `shouldBe` Right ("[::ffff:192.168.1.1]", 3000, 5)

    it "should fail on IPv6 without brackets" $ do
      parseAddress (EndPointAddress "::1:8080:42") `shouldSatisfy` isLeft
      parseAddress (EndPointAddress "2001:db8::1:8080:42") `shouldSatisfy` isLeft

    it "should reject empty brackets" $ do
      let result = parseAddress (EndPointAddress "[]:8080:42")
      case result of
        Left _ -> pure ()
        Right (host, _, _) -> expectationFailure $ "Expected failure but got: " <> show host

    it "should handle unclosed brackets" $ do
      parseAddress (EndPointAddress "[::1:8080:42") `shouldSatisfy` isLeft
      parseAddress (EndPointAddress "[2001:db8::1:8080:42") `shouldSatisfy` isLeft

  describe "Port number range" $ do
    it "should parse minimum valid port" $ do
      let addr = EndPointAddress "localhost:1:0"
      parseAddress addr `shouldBe` Right ("localhost", 1, 0)

    it "should parse maximum valid port" $ do
      let addr = EndPointAddress "localhost:65535:0"
      parseAddress addr `shouldBe` Right ("localhost", 65535, 0)

  describe "Endpoint ID range" $ do
    it "should parse minimum endpoint ID" $ do
      let addr = EndPointAddress "localhost:8080:0"
      parseAddress addr `shouldBe` Right ("localhost", 8080, 0)

    it "should parse large endpoint ID" $ do
      let addr = EndPointAddress "localhost:8080:4294967295"
      parseAddress addr `shouldBe` Right ("localhost", 8080, 4294967295)

  describe "Invalid addresses" $ do
    it "should fail on missing port" $ do
      let addr = EndPointAddress "localhost"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on missing endpoint ID" $ do
      let addr = EndPointAddress "localhost:8080"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on extra colons" $ do
      let addr = EndPointAddress "localhost:8080:1:extra"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on empty host" $ do
      let addr = EndPointAddress ":8080:1"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on non-numeric port" $ do
      let addr = EndPointAddress "localhost:abc:1"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on non-numeric endpoint ID" $ do
      let addr = EndPointAddress "localhost:8080:xyz"
      parseAddress addr `shouldSatisfy` isLeft

    it "should fail on negative numbers" $ do
      let addr = EndPointAddress "localhost:-8080:1"
      parseAddress addr `shouldSatisfy` isLeft

  describe "IPv6 QuickCheck properties" $ do
    it "should parse generated IPv6 addresses"
      $ property
      $ \(ipv6 :: ValidIPv6) (port' :: ValidPort) (endpointId :: Word32) ->
        let ValidIPv6 addr = ipv6
            ValidPort port = port'
            addrStr = "[" <> addr <> "]:" <> show port <> ":" <> show endpointId
            endpointAddr = EndPointAddress $ fromString addrStr
         in parseAddress endpointAddr == Right ("[" <> convert addr <> "]", fromIntegral port, endpointId)

    it "should reject IPv6 without brackets"
      $ property
      $ \(ipv6 :: ValidIPv6) (port' :: ValidPort) (endpointId :: Word32) ->
        let ValidIPv6 addr = ipv6
            ValidPort port = port'
            addrStr = addr <> ":" <> show port <> ":" <> show endpointId
            endpointAddr = EndPointAddress $ fromString addrStr
         in parseAddress endpointAddr `shouldSatisfy` isLeft

    it "should handle mixed valid addresses"
      $ property
      $ \(host' :: ValidMixedHost) (port' :: ValidPort) (endpointId :: Word32) ->
        let ValidMixedHost host = host'
            ValidPort port = port'
            addrStr = host <> ":" <> show port <> ":" <> show endpointId
            endpointAddr = EndPointAddress $ fromString addrStr
         in case parseAddress endpointAddr of
              Right (h, p, e) ->
                h == convert host && p == fromIntegral port && e == endpointId
              Left _ -> False

  describe "QuickCheck properties" $ do
    it "should roundtrip valid addresses"
      $ property
      $ \(host' :: ValidHost) (port' :: ValidPort) (endpointId :: Word32) ->
        let ValidHost host = host'
            ValidPort port = port'
            addrStr = host <> ":" <> show port <> ":" <> show endpointId
            addr = EndPointAddress $ fromString addrStr
         in parseAddress addr == Right (convert host, fromIntegral port, endpointId)

    it "should always fail on empty string" $ do
      let addr = EndPointAddress ""
      parseAddress addr `shouldSatisfy` isLeft

    it "should preserve all components"
      $ property
      $ \(host' :: ValidHost) (port' :: ValidPort) (endpointId :: Word32) ->
        let ValidHost host = host'
            ValidPort port = port'
            addrStr = host <> ":" <> show port <> ":" <> show endpointId
            addr = EndPointAddress $ fromString addrStr
         in case parseAddress addr of
              Right (h, p, e) ->
                h == convert host && p == fromIntegral port && e == endpointId
              Left _ -> False

-- | Valid hostname newtype for QuickCheck
newtype ValidHost = ValidHost String
  deriving (Eq, Show)

instance Arbitrary ValidHost where
  arbitrary =
    ValidHost
      <$> oneof
        [ pure "localhost"
        , pure "127.0.0.1"
        , pure "[::1]" -- IPv6 addresses need brackets
        , do
            -- Generate simple domain name
            subdomain <- listOf1 $ elements ['a' .. 'z']
            domain <- listOf1 $ elements ['a' .. 'z']
            tld <- elements ["com", "net", "org", "io"]
            pure $ subdomain <> "." <> domain <> "." <> tld
        , do
            -- Generate IPv4 address
            octets <- replicateM 4 $ choose (0, 255) :: Gen [Int]
            pure $ L.intercalate "." $ map show octets
        ]

-- | Valid port number newtype for QuickCheck
newtype ValidPort = ValidPort Word16
  deriving (Eq, Show)

instance Arbitrary ValidPort where
  arbitrary = ValidPort <$> choose (1, 65535)

-- | Valid IPv6 address newtype for QuickCheck
newtype ValidIPv6 = ValidIPv6 String
  deriving (Eq, Show)

instance Arbitrary ValidIPv6 where
  arbitrary =
    ValidIPv6
      <$> oneof
        [ pure "::1" -- loopback
        , pure "::" -- unspecified
        , pure "::ffff:127.0.0.1" -- IPv4-mapped
        , pure "fe80::1" -- link-local
        , pure "2001:db8::1" -- documentation
        , pure "2001:db8:85a3::8a2e:370:7334" -- documentation with more segments
        , do
            -- Generate random IPv6 address segments
            segments <-
              replicateM 8
                $ elements
                $ map (`showHex` "") [0x0000 .. 0xffff :: Word16]
            pure $ L.intercalate ":" segments
        , do
            -- Generate compressed IPv6 (with ::)
            prefix <-
              replicateM 2
                $ elements
                $ map (`showHex` "") [0x0000 .. 0xffff :: Word16]
            suffix <-
              replicateM 2
                $ elements
                $ map (`showHex` "") [0x0000 .. 0xffff :: Word16]
            pure $ L.intercalate ":" prefix <> "::" <> L.intercalate ":" suffix
        ]

-- | Valid mixed host (IPv4, IPv6 with brackets, or hostname)
newtype ValidMixedHost = ValidMixedHost String
  deriving (Eq, Show)

instance Arbitrary ValidMixedHost where
  arbitrary =
    ValidMixedHost
      <$> oneof
        [ do
            ValidHost h <- arbitrary
            pure h
        , do
            ValidIPv6 ipv6 <- arbitrary
            pure $ "[" <> ipv6 <> "]"
        ]

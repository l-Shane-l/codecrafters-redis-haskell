module RESP where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

-- Protocol markers
arrayMarker :: Word8
arrayMarker = 42 -- '*'

bulkStringMarker :: Word8
bulkStringMarker = 36 -- '$'

simpleStringMarker :: Word8
simpleStringMarker = 43 -- '+'

errorMarker :: Word8
errorMarker = 45 -- '-'

integerMarker :: Word8
integerMarker = 58 -- ':'

-- Line endings
carriageReturn :: Word8
carriageReturn = 13 -- '\r'

lineFeed :: Word8
lineFeed = 10 -- '\n'

lineEnding :: ByteString
lineEnding = BS.pack [carriageReturn, lineFeed]

-- Helper to add line ending
terminate :: ByteString -> ByteString
terminate bs = bs <> lineEnding


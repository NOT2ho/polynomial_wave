-----------------------------------------
-- some codes are from Codec.Audio.Wave © 2016–present Mark Karpov, License:  BSD 3 clause
-- stack and cabal are not worked.
-----------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data (Data)
import Data.Set qualified as E
import Data.Typeable
import Data.Word
import System.IO

data Wave = Wave
  {waveSampleRate :: !Word32,
    waveDataOffset :: !Word32,
    waveSampleFormat :: !Word16,
    waveDataSize :: !Word64,
    waveSamplesTotal :: !Word64,
    waveOtherChunks :: [(ByteString, ByteString)]
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data)

defaultWave :: Wave
defaultWave =
  Wave
    { waveSampleRate = 44100,
      waveSampleFormat = 16,
      waveDataOffset = 0,
      waveDataSize = 0,
      waveSamplesTotal = 0,
      waveOtherChunks = []
    }

data Chunk m = Chunk
  { -- | Four-byte chunk tag
    chunkTag :: !ByteString,
    -- | Chunk size
    chunkSize :: !Word32,
    -- | Chunk body in some form
    chunkBody :: !(m ByteString)
  }

writeNoData :: Either (Handle -> IO ()) a
writeNoData = (Left . const . return) ()


writeBsChunk ::
  -- | 'Handle' where to write
  Handle ->
  -- | Chunk tag
  ByteString ->
  -- | Chunk body
  ByteString ->
  IO ()
writeBsChunk h chunkTag body =
  let chunkSize = fromIntegral (B.length body)
      chunkBody = Right body
   in writeChunk h Chunk {..}

data Header = Header {
    af :: Word16
    , noc :: Word16
    , sl :: Word32
    , bl :: Word32
    , ba :: Word16
    , bps :: Word16
}

defaultHeader = Header {
    af = 0x0001
    , noc = fromInteger (1 :: Integer) ::Word16
    , sl = fromInteger (44100 :: Integer) ::Word32
    , bl = fromInteger (2*44100 :: Integer) ::Word32
    , ba = fromInteger (2 :: Integer) ::Word16
    , bps = fromInteger (16 :: Integer) ::Word16

}

fromWord32be :: Word32 -> [Word8]
fromWord32be w = [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

fromWord16be :: Word16 -> [Word8]
fromWord16be w = [fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

fromWord32le :: Word32 -> [Word8]
fromWord32le w = [ fromIntegral w
    , fromIntegral (w `shiftR` 8)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 24)
    ]

fromWord16le :: Word16 -> [Word8]
fromWord16le w = [fromIntegral w
    , fromIntegral (w `shiftR` 8)
    ]

renderFmtChunk :: Header -> ByteString
renderFmtChunk header@Header {..} =
  B.pack $ fromWord16le af ++ fromWord16le noc ++ fromWord32le sl ++ fromWord32le bl ++ fromWord16le ba ++ fromWord16le bps


writeWave ::
    Handle ->
    Wave ->
    Header ->
    (Handle -> IO ()) ->
    IO ()
writeWave h wave header writeData = do
  let nonPcm = False
  beforeOuter <- hTell h
  writeChunk h (Chunk "RIFF" 0 writeNoData)
  -- Write the WAVE format tag.
  B.hPut h "WAVE"
  -- Write fmt chunk.
  writeBsChunk h "fmt " (renderFmtChunk header)
  -- Write a dummy fact chunk if necessary.
  beforeFact <- hTell h
  -- Write any extra chunks if present.
  forM_ (waveOtherChunks wave) (uncurry $ writeBsChunk h)
  -- Write data chunk.
  beforeData <- hTell h
  writeChunk h (Chunk "data" 0 (Left writeData))
  -- Take care of alignment.
  rightAfterData <- hTell h
  when (odd rightAfterData) $
    B.hPut h "\0"
  -- Go back and overwrite dummy values.
  afterData <- hTell h
  let riffSize = fromIntegral (afterData - beforeOuter - 8)
      dataSize = fromIntegral (afterData - beforeData - 8)
      samplesTotal =
        fromIntegral $
          pcmSamplesTotal wave {waveDataSize = fromIntegral dataSize}
  hSeek h AbsoluteSeek beforeData
  writeChunk h (Chunk "data" dataSize writeNoData)
  hSeek h AbsoluteSeek beforeOuter
  writeChunk h (Chunk "RIFF" riffSize writeNoData)

writeChunk ::
  -- | Opened 'Handle' where to write the 'Chunk'
  Handle ->
  -- | The 'Chunk' to write
  Chunk (Either (Handle -> IO ())) ->
  IO ()
writeChunk h Chunk {..} = do
  let bytes = B.unpack (B.take 4 (chunkTag <> B.replicate 4 0x00)) ++ fromWord32le chunkSize
  B.hPut h $ B.pack bytes
  case chunkBody of
    Left action -> action h
    Right body -> B.hPut h body

pcmSamplesTotal :: Wave -> Word64
pcmSamplesTotal wave =
  waveDataSize wave `quot` 2


writeWaveFile ::
  (MonadIO m) =>
  FilePath ->
  Wave ->
  Header ->
  (Handle -> IO ()) ->
  m ()
writeWaveFile path wave header writeData = liftIO . withBinaryFile path WriteMode $ \h ->
  writeWave h wave header writeData >>= (\h' -> hClose h)

main :: IO ()
main = writeWaveFile "C:/Users/i5-32/Desktop/polynomial_wave/wav.wav" defaultWave defaultHeader (\h -> B.hPut h (B.pack $ take 1000000 $ cycle (body 1000)))

body :: Int -> [Word8]
body i =  (fromIntegral $ round (100*cos (2 * (22/7) * (fromIntegral i :: Float))) ::Word8 ): body (i - 1)
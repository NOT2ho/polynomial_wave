-----------------------------------------
-- stack and cabal are not worked.
-- some codes are from Codec.Audio.Wave © 2016–present Mark Karpov, License:  BSD 3 clause
-- it is modified since it has dependency
-----------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Float ( castFloatToWord32, int2Float )
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
import Text.Read (readMaybe)

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

fromFloat :: Float -> [Word8]
fromFloat f =  take 2 (fromWord32le $ castFloatToWord32 f)

fromInt :: Int -> [Word8]
fromInt i = fromWord16le (fromIntegral i :: Word16)


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


saveWave :: String -> [Int] ->  IO ()
saveWave s f = writeWaveFile s defaultWave defaultHeader (\h -> B.hPut h (B.pack $ take 500000 (cycle (concatMap fromInt f))))

saveWaveParted :: String -> [Int] ->  IO ()
saveWaveParted s f = writeWaveFile s defaultWave defaultHeader (\h -> B.hPut h (B.pack (concatMap fromInt f)))


polynomial :: [Float] -> Float ->  Float
polynomial (i:is) x = i * (x ** int2Float (length (i:is) -1)) + polynomial is x
polynomial [] x = 0

fxList :: [Float] -> [Float] -> [Float]
fxList xs f = map (polynomial f) xs

interval :: Float -> Float -> Int -> [Float]
interval a b i =map ((+a) . (*(b-a)) . (/ int2Float i)) (take i [0, 1..]) ++[b]

coefficients :: Int -> [Float] -> IO [Float]
coefficients i its = do
    putStr ("coefficient of x^" ++ show (i-1) ++ ": ")
    n <- inputFloat
    if i == 1 then return $ n : its else coefficients (i-1) (n : its)

graph :: [Float] -> String
graph y = show (normalize y) ++ concatMap ((++ "\n") . (\x -> "*" ++ replicate (round x) ' ')) (normalize y)

normalize :: [Float] -> [Float]
normalize ys = map ( (*10) . (+1) . (/(maximum ys - minimum ys)) . ( `subtract` minimum ys)) ys

normalize65535 :: [Float] -> [Int]
normalize65535 ys = map ( round . (+ (-32768)) .(*65535) . (/(maximum ys - minimum ys)) . ( + (- minimum ys))) ys

normalize65535Int :: [Int] -> [Int]
normalize65535Int ys = map ((+ (-32768)) .(*65535) . (`div`(maximum ys - minimum ys)) . ( + (- minimum ys))) ys

aSin :: Float -> Int -> Int ->  [Float]
aSin a n f = map ((* a) . sin . (* int2Float n) . (* (44/7)) . (/ int2Float f)) [0..int2Float f]

bCos :: Float -> Int ->  Int -> [Float]
bCos a n f = map ((* a) . cos . (* int2Float n) . (* (44/7)) . (/ int2Float f)) [0..int2Float f]

fourier :: Float -> Float -> Int ->  Int -> [Float]
fourier a b n f = zipWith (+) (aSin a n f) (bCos b n f)

series :: [(Float, Float)] -> Int -> [Float]
series (c:cs) f =  zipWith (+) (uncurry fourier c (length (c:cs)) f) (series cs f)
series [] _ = [0, 0..]

fourierCoef :: Int -> [(Float, Float)] -> IO [(Float, Float)]
fourierCoef i its = do
    putStr ("coefficient of sin " ++ show (i-1) ++ "x: ")
    n <- inputFloat
    putStr ("coefficient of cos " ++ show (i-1) ++ "x: ")
    n2 <- inputFloat
    if i == 1 then return $ (n,n2) : its else fourierCoef (i-1) ((n,n2) : its)

fourierIO :: String -> IO ()
fourierIO dir = do
  putStr "file name: "
  fileName <- inputStr
  putStr "period: "
  term <- inputInt
  putStr "how much sum (0-n): "
  n' <- inputInt
  let n= n' +1
  clist' <- fourierCoef n []
  let clist = reverse clist'
  let ylist = series clist term
  print clist
  print $ normalize65535 ylist
  print ylist
  saveWave (dir ++ "/" ++ fileName ++ ".wav") $ normalize65535 ylist
  putStrLn "file saved. enter next file name"

fourierIOPart :: Int -> Int -> IO [Int]
fourierIOPart i v = do
  putStr "period: "
  term <- inputInt
  putStr "how much sum (0-n): "
  n' <- inputInt
  let n= n' +1
  clist' <- fourierCoef n []
  let clist = reverse clist'
  let ylist = take i $ cycle (series clist term)
  print clist
  print (series clist term)
  return $ map (`div` v) $ normalize65535 ylist


inputStr :: IO String
inputStr = do
    str <- getLine
    case str of
        [] -> putStr "enter something: " >> inputStr
        _ -> return str

inputInt :: IO Int
inputInt = do
    str <- getLine
    case readMaybe str ::Maybe Int of
        Just s -> return s
        Nothing -> putStr "enter INT: " >> inputInt

inputFloat :: IO Float
inputFloat = do
    str <- getLine
    case readMaybe str :: Maybe Float of
        Just s -> return s
        Nothing -> putStr "enter Float: " >> inputFloat

asciiart :: IO ()
asciiart = putStrLn $ unlines
 [   "_______ _______ ___    __   __ __    _ _______ __   __ ___ _______ ___     "
    ,"|       |       |   |  |  | |  |  |  | |       |  |_|  |   |   _   |   |    "
    ,"|    _  |   _   |   |  |  |_|  |   |_| |   _   |       |   |  |_|  |   |    "
    ,"|   |_| |  | |  |   |  |       |       |  | |  |       |   |       |   |    "
    ,"|    ___|  |_|  |   |__|_     _|  _    |  |_|  |       |   |       |   |___ "
    ,"|   |   |       |       ||   | | | |   |       | ||_|| |   |   _   |       |"
    ,"|___|   |_______|_______||___| |_|  |__|_______|_|   |_|___|__| |__|_______|"
    ," _     _ _______ __   __ _______                                            "
    ,"| | _ | |   _   |  | |  |       |                                           "
    ,"| || || |  |_|  |  |_|  |    ___|                                           "
    ,"|       |       |       |   |___                                            "
    ,"|       |       |       |    ___|                                           "
    ,"|   _   |   _   ||     ||   |___                                            "
    ,"|__| |__|__| |__| |___| |_______|                                           "
    ," _______ _______ ______                                                     "
    ,"|       |       |    _ |                                                    "
    ,"|    ___|   _   |   | ||                                                    "
    ,"|   |___|  | |  |   |_||_                                                   "
    ,"|    ___|  |_|  |    __  |                                                  "
    ,"|   |   |       |   |  | |                                                  "
    ,"|___|   |_______|___|  |_|                                                  "
    ," __   __ _______ _______ __   __                                            "
    ,"|  | |  |       |   _   |  | |  |                                           "
    ,"|  | |  |_     _|  |_|  |  | |  |                                           "
    ,"|  |_|  | |   | |       |  |_|  |                                           "
    ,"|       | |   | |       |       |                                           "
    ,"|       | |   | |   _   |       |                                           "
    ,"|_______| |___| |__| |__|_______|                                           "
    ]


partedIO :: String -> IO ()
partedIO dir = do
  putStr "file name: "
  fileName <- inputStr
  putStrLn "front part generation"
  putStr "front part length(*44100): "
  i <- inputInt
  putStr "front part volume(/1), it is reciprocal: "
  v <- inputInt
  front <- select' i v
  putStrLn "front part generated."
  putStrLn "tail ganeration"
  putStr "tail length(*44100): "
  i' <- inputInt
  putStr "tail volume(/1), it is reciprocal: "
  v' <- inputInt
  tail <- select' i' v'
  putStrLn "tail generated."
  putStr "fade length(*44100): "
  i'' <- inputInt
  putStr "fade scale(/1), it is reciprocal: "
  v'' <- inputInt
  let faded = front ++ fade v'' i'' front tail ++ tail
  saveWaveParted (dir ++ "/" ++ fileName ++ ".wav") faded
  putStrLn "file saved. enter next file name"


fade :: Int -> Int -> [Int] -> [Int] -> [Int]
fade v 0 a b= []
fade v i (l:ist) (li:st) = ((l+li)`div` v) :fade v (i-1) ist st
fade _ _ [] [] = []


polynomialIO :: String -> IO ()
polynomialIO dir = do
    putStr "file name: "
    fileName <- inputStr
    putStr "Interval start: "
    from <- inputFloat
    putStr "Interval end: "
    to <- inputFloat
    putStr "Interval interval: "
    term <- inputInt
    putStr "degree: "
    degree' <- inputInt
    let degree = degree' +1
        xlist = interval from to term
    clist' <- coefficients degree []
    let clist = reverse clist'
    let ylist = fxList xlist clist
    print clist
    print xlist
    print $ normalize65535 ylist
    print ylist
    saveWave (dir ++ "/" ++ fileName ++ ".wav") $ normalize65535 ylist
    putStrLn "file saved. enter next file name"

polynomialIOPart :: Int -> Int -> IO [Int]
polynomialIOPart i v = do
    putStr "Interval start: "
    from <- inputFloat
    putStr "Interval end: "
    to <- inputFloat
    putStr "Interval interval: "
    term <- inputInt
    putStr "degree: "
    degree' <- inputInt
    let degree = degree' +1
        xlist = interval from to term
    clist' <- coefficients degree []
    let clist = reverse clist'
    let ylist = take i $ cycle (fxList xlist clist)
    print clist
    print xlist
    print (fxList xlist clist)
    return $ map (`div` v) $ normalize65535 ylist

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    asciiart
    putStrLn "welcome to the polynomial wave"
    putStrLn "ver0.0.5"
    putStrLn "bug fixed : reversed coefficient list, float to word16 issue, normalize to 32767"
    putStrLn "update : add 2-parted wav"
    putStrLn "***************************************************************"
    putStr "file save dir: "
    dir <- getLine
    superSelect dir

superSelect :: String -> IO ()
superSelect dir = do
    putStr "enter 0 to simple wave, enter 1 to two-parted wav: "
    n <- inputInt
    case n of
      0 -> select dir
      1 -> forever $ partedIO dir
      _ -> superSelect dir

select :: String -> IO ()
select dir = do
    putStr "enter 0 to polynomial, enter 1 to fourier series: "
    n <- inputInt
    case n of
      0 -> forever $ polynomialIO dir
      1 -> forever $ fourierIO dir
      _ -> select dir

select' :: Int -> Int -> IO [Int]
select' i v  = do
    putStr "enter 0 to generate polynomial, or enter 1 to generate fourier series: "
    n <- inputInt
    case n of
      0 -> polynomialIOPart i v
      1 -> fourierIOPart i v
      _ -> select' i v

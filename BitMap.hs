module BitMap (write) where

import Data.Binary.Put
import Data.ByteString.Lazy as BL
import Image (Pixel, Raster)
import System.IO

-- Writes a raster into a BMP file
write :: String -> Raster -> IO ()

write fileName (pixels, width, height) = do
  file <- openFile fileName WriteMode
  BL.hPutStr file $ runPut (
    do
      writeFileHeader width height
      writeInfoHeader width height
      writeRaster width height pixels 0 0
    )
  hClose file

-- Calculates the raster size in bytes
rasterSize :: Int -> Int -> Int

rasterSize width height | lineSize `mod` 4 == 0 = lineSize * height
                        | otherwise = (lineSize - (lineSize `rem` 4) + 4) * height
  where lineSize = width * 3

-- Bitmap file header size
fileHeaderSize :: Int

fileHeaderSize = 14

-- Bitmap info header size
infoHeaderSize :: Int

infoHeaderSize = 40

-- Writes the BMP file header
writeFileHeader :: Int -> Int -> Put

writeFileHeader width height = do
  putWord8 0x42 -- Magic number 'B'
  putWord8 0x4D -- Magic number 'M'
  putWord32le (fromIntegral ((rasterSize width  height) + fileHeaderSize + infoHeaderSize)) -- Bitmap file size = raster size + header size
  putWord16le 0 -- Reserved, always 0
  putWord16le 0 -- Reserved, always 0
  putWord32le (fromIntegral (fileHeaderSize + infoHeaderSize)) -- Raster offset = total header size

-- Writes the BMP info header
writeInfoHeader :: Int -> Int -> Put

writeInfoHeader width height = do
  putWord32le 40 -- Info header size
  putWord32le (fromIntegral width) -- Width in pixels
  putWord32le (fromIntegral height) --  Height in pixels
  putWord16le 1 -- Number of color planes 
  putWord16le 24 -- Bits per pixel, for RGB8 = 24
  putWord32le 0 -- Compression format = Bitfields
  putWord32le (fromIntegral (width * height * 4)) -- Raster data size
  putWord32le 2835 -- Pixels per meter horizontal
  putWord32le 2835 -- Pixels per meter vertical
  putWord32le 0 -- Palette size = 0 because we don't use a palette
  putWord32le 0 -- Important colors in palette = 0 because we don't use a palette

-- Writes a raster
writeRaster :: Int -> Int -> [Pixel] -> Int -> Int -> Put

writeRaster width height [] 0 0 = return ()

writeRaster width height [] _ _ = error("pixel buffer too small")

writeRaster width height (pixel:pixels) x y
  | y == height = return ()
  | x == width = do
      writePadding (rowPadding width)
      writeRaster width height (pixel:pixels) 0 (y + 1)
  | otherwise = do
        writePixel pixel
        writeRaster width height pixels (x + 1) y

-- Writes row padding
writePadding :: Int -> Put

writePadding 0 = return ()

writePadding padding = do
  putWord8 0
  writePadding (padding - 1)

-- Calculates row padding
rowPadding :: Int -> Int

rowPadding width
  | lineSize `mod` 4 == 0 = 0
  | otherwise = 4 - lineSize `rem` 4
  where lineSize = width * 3

-- Writes a pixel
writePixel :: Pixel -> Put

writePixel (r, g, b, a) = do
  writeChannel b a
  writeChannel g a
  writeChannel r a

-- Writes a channel
writeChannel :: Float -> Float -> Put

writeChannel value alpha = putWord8 (fromInteger (round ((normalizeChannel value alpha) * 255.0)))
  
-- Normalizes a channel
normalizeChannel :: Float -> Float -> Float
  
normalizeChannel value alpha
  | blended < 0.0 = 0.0
  | blended > 1.0 = 1.0
  | otherwise = blended
  where blended = value * alpha

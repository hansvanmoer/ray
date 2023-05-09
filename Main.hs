module Main where
import qualified BitMap (write)

generatePixels :: [(Float, Float, Float, Float)]

generatePixels = ((1.0, 0.0, 0.0, 1.0):generatePixels)

main :: IO ()
main = do
  BitMap.write "/home/hans/projects/haskell/ray/data/test.bmp" (generatePixels, 2, 2)

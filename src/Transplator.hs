module Transplator
    ( 
        transplate
    ) where
import Codec.FFmpeg
import Codec.Picture
import Control.Applicative
import Data.Vector(Vector)
import qualified Data.Vector as V 

input_video:: FilePath
input_video = "/Users/edevi86/Movies/road2.mp4"

resolutionX :: (Integral a) => a
resolutionX = 900
resolutionY :: (Integral a) => a
resolutionY = 450
numFrames = resolutionX

videoFPS :: (Integral a) => a
videoFPS = 30

-- firstFrame :: IO (Maybe DynamicImage)
-- firstFrame = do initFFmpeg
--                 (getFrame, cleanup) <- imageReader (File "myVideo.mov")
--                 (fmap ImageRGB8 <$> getFrame) <* cleanup
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
transplate :: IO ()
transplate = do
    initFFmpeg
    (getFrame, cleanup) <- imageReader (File input_video)
    frames <- replicateM resolutionX getFrame -- $ (fmap ImageRGB8  <$> getFrame)
    let frames' = frames >>= toList 
        framesₜ = transposeVideo frames'
    -- traverse saveFramesAsPng (zip [0..] framesₜ)
    cleanup
    saveAsVideo framesₜ
    

saveAsVideo :: [Image PixelRGB8] -> IO ()
saveAsVideo frames = do
    boom <- imageWriter  ((defaultParams resolutionX resolutionY) { epFps = videoFPS }) "/tmp/frames/road2_900.mp4"  
    traverse (boom . Just) frames
    boom Nothing
    

-- saveFramesAsPng :: Maybe DynamicImage -> IO ()
saveFramesAsPng :: (Int, Image PixelRGB8) -> IO ()
saveFramesAsPng (i, avf) =
  do
    -- putStrLn $ "Frame at " ++ show ts
    savePngImage ("/tmp/frames/frame_" <> show i <> ".png") (ImageRGB8 avf)

transposeVideo :: [Image PixelRGB8] ->  [Image PixelRGB8]
transposeVideo input = getSliceAtX <$> [0..resolutionX - 1] 
  where
    vs = V.fromList input
    getSliceAtX x = generateImage gen numFrames resolutionY
      where
        gen t y = pixelAt (vs V.! t) x y
    
    
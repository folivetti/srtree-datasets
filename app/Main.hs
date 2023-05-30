module Main (main) where

import Data.SRTree.Datasets ( loadDataset )
import System.Environment ( getArgs )
import qualified Data.Vector as V
import Numeric.LinearAlgebra (size)

main :: IO ()
main = do
    (fname:hasHeader:_) <- getArgs
    ((x,y,xv,yv), hm) <- loadDataset fname (read hasHeader)
    print $ size $ V.head x
    print $ size $ V.head xv
    print x
    print y
    print hm

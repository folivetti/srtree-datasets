{-# language ImportQualifiedPost #-}
{-# language ViewPatterns #-}
module Data.SRTree.Datasets (loadDataset)
    where

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BS
import Data.List (intercalate, delete, find)
import Data.Vector qualified as V
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra ((??), (¿), Extractor(..))
import Codec.Compression.GZip ( decompress )
import System.FilePath (takeExtension)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

type Columns = V.Vector Column
type Column  = LA.Vector Double

loadMtx :: [[B.ByteString]] -> LA.Matrix Double
loadMtx = LA.fromLists . map (map (read . B.unpack))
{-# INLINE loadMtx #-}

isGZip :: FilePath -> Bool
isGZip = (== ".gz") . takeExtension
{-# INLINE isGZip #-}

detectSep :: [B.ByteString] -> Char
detectSep xss = go seps
  where
    seps = [' ','\t','|',':',';',',']
    xss' = map B.strip xss
    allSameLen [] = True
    allSameLen (y:ys) = y /= 1 && all (==y) ys

    go [] = error $ "CSV parsing error: unsupported separator. Supporter separators are "
                   <> intercalate "," (map show seps)
    go (c:cs) = if allSameLen $ map (length . B.split c) xss'
                   then c
                   else go cs
{-# INLINE detectSep #-}

readFileToLines :: FilePath -> IO [[B.ByteString]]
readFileToLines filename = do
  content <- filter (not . B.null) . B.split '\n' . toChar8 . appGz <$> BS.readFile filename
  let sep = detectSep $ take 100 content
  pure $ filter (not.null) . map (B.split sep) $ content
  where
      appGz   = if isGZip filename then decompress else id
      toChar8 = B.pack . map (toEnum . fromEnum) . BS.unpack
{-# INLINE readFileToLines #-}

splitFileNameParams :: FilePath -> (FilePath, [B.ByteString])
splitFileNameParams (B.pack -> filename) = (B.unpack fname, take 4 params)
  where
    (fname : params') = B.split ':' filename
    params            = params' <> replicate (4 - min 4 (length params')) B.empty
{-# inline splitFileNameParams #-}

parseVal :: String -> Either String Int
parseVal xs = case readMaybe xs of
                Nothing -> Left xs
                Just x  -> Right x
{-# inline parseVal #-}

getColumns :: [(B.ByteString, Int)] -> B.ByteString -> B.ByteString -> ([Int], Int)
getColumns headerMap target columns = (ixs, iy)
  where
      n_cols = length headerMap
      getIx c = case parseVal c of
                  Left name -> case find ((== B.pack name) . fst) headerMap of
                                 Nothing -> error $ "column name " <> name <> " does not exist."
                                 Just v  -> snd v
                  Right v   -> if v >= 0 && v < n_cols
                                 then v
                                 else error $ "column index " <> show v <> " out of range."
      ixs = if B.null columns
               then delete iy [0 .. n_cols - 1]
               else map (getIx . B.unpack) $ B.split ',' columns
      iy = if B.null target
              then n_cols - 1
              else getIx $ B.unpack target
{-# inline getColumns #-}

getRows :: B.ByteString -> B.ByteString -> Int -> (Extractor, Extractor)
getRows (B.unpack -> start) (B.unpack -> end) nRows
  | st_ix >= end_ix = error $ "Invalid range: " <> show start <> ":" <> show end <> "."
  | st_ix == 0 && end_ix == nRows-1 = (All, All)
  | otherwise = (Range st_ix 1 end_ix, Range (end_ix + 1) 1 (nRows-1))
  where
      st_ix = if null start
                then 0
                else case readMaybe start of
                       Nothing -> error $ "Invalid starting row " <> start <> "."
                       Just x  -> if x < 0 || x >= nRows
                                    then error $ "Invalid starting row " <> show x <> "."
                                    else x
      end_ix = if null end
                then nRows - 1
                else case readMaybe end of
                       Nothing -> error $ "Invalid end row " <> end <> "."
                       Just x  -> if x < 0 || x >= nRows
                                    then error $ "Invalid end row " <> show x <> "."
                                    else x
{-# inline getRows #-}

loadDataset :: FilePath -> Bool -> IO ((Columns, Column, Columns, Column), String)
loadDataset filename hasHeader = do
  let
    (fname, params) = splitFileNameParams filename
  csv <- readFileToLines fname
  let
    ncols             = length $ head csv
    nrows             = length csv - if hasHeader then 1 else 0
    (header, content) = if hasHeader
                           then (head csv, tail csv)
                           else ([B.pack ('x' : show i) | i <- [0 .. ncols]], csv)
    headerMap = zip header [0 ..]
    (ixs, iy) = getColumns headerMap (params !! 2) (params !! 3)
    (rows_train, rows_val) = getRows (params !! 0) (params !! 1) nrows
    datum = loadMtx content
    x = datum ¿ ixs
    y = datum ¿ [iy]
    x_train = x ?? (rows_train, All)
    y_train = y ?? (rows_train, All)
    x_val = x ?? (rows_val, All)
    y_val = y ?? (rows_val, All)
    toColumns = V.fromList . LA.toColumns
    toColumn  = head . LA.toColumns
    varnames  = intercalate "," [B.unpack v | c <- ixs, let v = fst . fromJust $ find ((==c).snd) headerMap]

  pure ((toColumns x_train, toColumn y_train, toColumns x_val, toColumn y_val ) , varnames)

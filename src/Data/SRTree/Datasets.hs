{-# language ImportQualifiedPost #-}
{-# language ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SRTree.Datasets
-- Copyright   :  (c) Fabricio Olivetti 2021 - 2021
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  FlexibleInstances, DeriveFunctor, ScopedTypeVariables, ConstraintKinds
--
-- Utility library to handle regression datasets
-- this module exports only the `loadDataset` function.
--
-----------------------------------------------------------------------------
module Data.SRTree.Datasets ( loadDataset )
    where

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BS
import Data.List (intercalate, delete, find)
import Data.Vector qualified as V
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra ((??), (¿), Extractor(..), idxs)
import Codec.Compression.GZip ( decompress )
import System.FilePath (takeExtension)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

type Columns = V.Vector Column
type Column  = LA.Vector Double

-- | Loads a list of list of bytestrings to a matrix of double
loadMtx :: [[B.ByteString]] -> LA.Matrix Double
loadMtx = LA.fromLists . map (map (read . B.unpack))
{-# INLINE loadMtx #-}

-- | Returns true if the extension is .gz
isGZip :: FilePath -> Bool
isGZip = (== ".gz") . takeExtension
{-# INLINE isGZip #-}

-- | Detects the separator automatically by 
--   checking whether the use of each separator generates
--   the same amount of columns in every row and at least two columns.
detectSep :: [B.ByteString] -> Char
detectSep xss = go seps
  where
    seps = [' ','\t','|',':',';',',']
    xss' = map B.strip xss

    allSameLen []     = True
    allSameLen (y:ys) = y /= 1 && all (==y) ys

    go []     = error $ "CSV parsing error: unsupported separator. Supporter separators are "
                      <> intercalate "," (map show seps)
    go (c:cs) = if allSameLen $ map (length . B.split c) xss'
                   then c
                   else go cs
{-# INLINE detectSep #-}

readFileToLines :: FilePath -> IO [[B.ByteString]]
readFileToLines filename = do
  content <- removeBEmpty . toLines . toChar8 . unzip <$> BS.readFile filename
  let sep = getSep content
  pure . removeEmpty . map (B.split sep) $ content
  where
      getSep       = detectSep . take 100 -- use only first 100 rows to detect separator
      removeBEmpty = filter (not . B.null)
      removeEmpty  = filter (not . null)
      toLines      = B.split '\n'
      unzip        = if isGZip filename then decompress else id
      toChar8      = B.pack . map (toEnum . fromEnum) . BS.unpack
{-# INLINE readFileToLines #-}

-- | Splits the parameters from the filename
-- the expected format of the filename is *filename.ext:p1:p2:p3:p4*
-- where p1 and p2 is the starting and end rows for the training data,
-- by default p1 = 0 and p2 = number of rows - 1
-- p3 is the target column, it can be a string corresponding to the header
-- or an index.
-- p4 is a comma separated list of columns (either index or name) to be used as 
-- input variables. These will be renamed internally as x0, x1, ... in the order
-- of this list.
splitFileNameParams :: FilePath -> (FilePath, [B.ByteString])
splitFileNameParams (B.pack -> filename) = (B.unpack fname, take 4 params)
  where
    (fname : params') = B.split ':' filename
    -- fill up the empty parameters with an empty string
    params            = params' <> replicate (4 - min 4 (length params')) B.empty
{-# inline splitFileNameParams #-}

-- | Tries to parse a string into an int
parseVal :: String -> Either String Int
parseVal xs = case readMaybe xs of
                Nothing -> Left xs
                Just x  -> Right x
{-# inline parseVal #-}

-- | Given a map between column name and indeces,
-- the target column and the variables columns,
-- returns the indices of the variables columns and the target
getColumns :: [(B.ByteString, Int)] -> B.ByteString -> B.ByteString -> ([Int], Int)
getColumns headerMap target columns = (ixs, iy)
  where
      n_cols  = length headerMap
      getIx c = case parseVal c of
                  -- if the column is a name, retrive the index
                  Left name -> case find ((== B.pack name) . fst) headerMap of
                                 Nothing -> error $ "column name " <> name <> " does not exist."
                                 Just v  -> snd v
                  -- if it is an int, check if it is within range
                  Right v   -> if v >= 0 && v < n_cols
                                 then v
                                 else error $ "column index " <> show v <> " out of range."
      -- if the input variables columns are ommitted, use
      -- every column except for iy
      ixs = if B.null columns
               then delete iy [0 .. n_cols - 1]
               else map (getIx . B.unpack) $ B.split ',' columns
      -- if the target column is ommitted, use the last one
      iy = if B.null target
              then n_cols - 1
              else getIx $ B.unpack target
{-# inline getColumns #-}

-- | Given the start and end rows, it returns the 
-- hmatrix extractors for the training and validation data
getRows :: B.ByteString -> B.ByteString -> Int -> (Extractor, Extractor)
getRows (B.unpack -> start) (B.unpack -> end) nRows
  | st_ix >= end_ix                 = error $ "Invalid range: " <> show start <> ":" <> show end <> "."
  | st_ix == 0 && end_ix == nRows-1 = (All, All)
  | otherwise                       = (Range st_ix 1 end_ix, Pos (idxs[ix | ix <- [0 .. nRows-1], ix < st_ix || ix > end_ix]))
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

-- | `loadDataset` loads a dataset with a filename in the format:
--   filename.ext:start_row:end_row:target:features
--
-- where
--
-- **start_row:end_row** is the range of the training rows (default 0:nrows-1).
--   every other row not included in this range will be used as validation
-- **target** is either the name of the column (if the datafile has headers) or the index
-- of the target variable
-- **features** is a comma separated list of columns names or indices to be used as
-- input variables of the regression model.
loadDataset :: FilePath -> Bool -> IO ((Columns, Column, Columns, Column), String)
loadDataset filename hasHeader = do  
  csv <- readFileToLines fname
  pure $ processData csv params hasHeader
  where
    (fname, params) = splitFileNameParams filename

-- support function that does everything for loadDataset
processData :: [[B.ByteString]] -> [B.ByteString] -> Bool -> ((Columns, Column, Columns, Column), String)
processData csv params hasHeader = ((toColumns x_train, toColumn y_train, toColumns x_val, toColumn y_val) , varnames)
  where
    ncols             = length $ head csv
    nrows             = length csv - fromEnum hasHeader
    (header, content) = if hasHeader
                           then (zip (map B.strip $ head csv) [0..], tail csv)
                           else (map (\i -> (B.pack ('x' : show i), i)) [0 .. ncols], csv)
    varnames          = intercalate "," [B.unpack v | c <- ixs
                                        , let v = fst . fromJust $ find ((==c).snd) header
                                        ]
    -- get rows and columns indices
    (rows_train, rows_val) = getRows (params !! 0) (params !! 1) nrows
    (ixs, iy)              = getColumns header (params !! 2) (params !! 3)

    -- load data and split sets
    datum   = loadMtx content
    x       = datum ¿ ixs
    y       = datum ¿ [iy]
    x_train = x ?? (rows_train, All)
    y_train = y ?? (rows_train, All)
    x_val   = x ?? (rows_val, All)
    y_val   = y ?? (rows_val, All)

    -- support functions
    toColumns = V.fromList . LA.toColumns
    toColumn  = head . LA.toColumns
    
{-# inline processData #-}

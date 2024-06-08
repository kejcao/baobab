import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getFileSize)
import System.FilePath (addTrailingPathSeparator)
import Text.Printf (printf)

inCircle :: RealFloat a => a -> (a,a) -> Bool
inCircle r (x,y) = sqrt (x**2 + y**2) < r

grid :: Integer -> [[(Integer,Integer)]]
grid r = [[(x,y) | x <- [-r..r]] |  y <- [r,(r-1)..(-r)]]

countPrefixSumBelow :: Num a => Ord a => a -> [a] -> Int
countPrefixSumBelow n = length . takeWhile (<n) . scanl (+) 0

charset :: [String]
charset = zipWith (\a c -> a ++ replicate 2 c ++ "\x1b[1;0m") ansiCodes "cdefhjkmnprtvwxy2345689"
  where ansiCodes = cycle $ map (\x -> "\x1b[1;" ++ show (fst x) ++ "m") [(31,41),(32,42),(33,43),(34,44),(35,45),(36,46)]

pieChart :: Integer -> [Double] -> String
pieChart r items = unlines $ map (concatMap draw) $ grid r
  where
    percentages =
      let s = sum items
      in map ((*100).(/s)) items

    draw p
      | inCircle (fromInteger r) (x,y) =
        let percent = (atan2 x y * (180/pi) + 180) / 3.6
        in charset !! (countPrefixSumBelow percent percentages - 1)
      | otherwise = "  "
      where
        x = fromInteger $ fst p
        y = fromInteger $ snd p


humanReadableSize :: Integer -> String
humanReadableSize size = printf "%.1f%s" base (["","K","M","G","T"] !! i)
  where
    i = length $ takeWhile (<size) $ drop 1 $ iterate (*1024) 1
    base = fromIntegral size / 1024**fromIntegral i :: Double

getSize :: FilePath -> IO Integer
getSize path = do
    b <- doesDirectoryExist path
    if b
        then sum <$> (listDirectory path >>= mapM (getSize . (addTrailingPathSeparator path ++)))
        else do
          b' <- doesFileExist path
          if b'
            then getFileSize path
            else return 0

display :: [(Integer,FilePath)] -> [String]
display = map (\(s,d) -> humanReadableSize s ++ "\t" ++ d)

main :: IO ()
main = do
    dirs <- listDirectory "."
    sizes <- mapM getSize dirs
    let l = take 8 $ sortBy (comparing (Down . fst)) $ zip sizes dirs
    mapM_ putStrLn $ zipWith (++) (map (++" - ") charset) (display l)
    putStrLn $ pieChart 14 (map (fromInteger . fst) l)

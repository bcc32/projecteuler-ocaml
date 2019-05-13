main :: IO ()
main = getNums >>= return . show . doSum >>= putStrLn

doSum :: [Integer] -> Integer
doSum = (`mod` (10^18)) . sum

getNums :: IO [Integer]
getNums = getContents >>= return . map read . lines

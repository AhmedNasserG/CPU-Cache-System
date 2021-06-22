data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec a = convertBinToDecHelper a 0
convertBinToDecHelper :: (Integral t1, Integral p, Integral t2) => t1 -> t2 -> p
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper x i = if mod x 10 == 1 then 2^i+ convertBinToDecHelper (div x 10) (i+1) else convertBinToDecHelper (div x 10) (i+1)

logBase2 :: Floating a => a -> a
logBase2 = logBase 2

-- getNumBits:: (Floating a1, Integral a, RealFrac a1, Foldable t) => a1 -> [Char] -> t a2 -> a
getNumBits _ "fullAssoc" _ = 0
getNumBits numOfBits "setAssoc" _ = logBase2 numOfBits
getNumBits _ "directMap" lst  = logBase2 ( fromIntegral x)
      where x = length lst


replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem x list idx = replaceIthItemHelper x list idx 0
replaceIthItemHelper :: (Eq t1, Num t1) => t2 -> [t2] -> t1 -> t1 -> [t2]
replaceIthItemHelper _ [] _ _ = []
replaceIthItemHelper x (h:t) idx currIdx =
  if idx == currIdx then x:t
  else h : replaceIthItemHelper x t idx (currIdx+1)

splitEvery _ [] = []
splitEvery n list = x1 : splitEvery n xs
  where (x1,xs) = splitAt n list
-------------- Set Associative --------------
getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache stringAddress cache "setAssoc" bitsNum = loop t 0 (cacheSets!!i)
  where
    setsNum = 2^bitsNum
    (t,i) = convertAddress (read stringAddress) setsNum "setAssoc"
    cacheSets = splitEvery (div (length cache) setsNum) cache

loop _ _ [] = NoOutput
loop t currHopsNum ((It (T t2) (D d) x _):xs) 
  | t2 == t && x = Out (d, currHopsNum)
  | otherwise = loop t (currHopsNum+1) xs

-- TODO: expected (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)
convertAddress :: (Integral a, Integral b) => a -> b -> String -> (a, a)
convertAddress binAddress bitsNum "setAssoc" =
  (tag,index) where
    tag = div binAddress (10^bitsNum)
    index = mod binAddress (10^bitsNum)


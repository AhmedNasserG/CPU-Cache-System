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
getDataFromCache stringAddress cache "setAssoc" bitsNum =
  getDataFromCacheHelper (t,i) cache setsNum where
    (t,i) = convertAddress stringAddress bitsNum "setAssoc"
    setsNum = 2^bitsNum

getDataFromCacheHelper (t,i) cache setsNum =
  loop (t,i) x (x+sz) 0 cache where
    x = getStIdx cache setsNum
    sz = div (length cache) setsNum

getStIdx cache setsNum =
  if mod (length cache) setsNum == 0
    then div (length cache) setsNum
  else
     div (length cache) setsNum + 1

loop (t,i) stIdx enIdx currHopsNum cache
  | stIdx > enIdx = NoOutput
  | stIdx <= enIdx && x && t2 == t = Out (getDataValue cache stIdx, currHopsNum)
  | otherwise = loop (t,i) (stIdx+1) enIdx (currHopsNum+1) cache
  where 
    (It (T t2) _ x _) = cache!!stIdx

getDataValue :: [Item a] -> Int -> a
getDataValue cache idx = d where (It _ (D d) _ _) = cache!!idx
-- traverse (t,i) currIdx currHopsNum cache 
--   | check t (cache!!currIdx) = Out (d, currHopsNum) 
--   where (It _ (D d) _ _) = cache!!currIdx

-- foo cache idx = d where (It _ (D d) _ _) = cache!!idx
-- check t (It (T tag) _ _ _) = t == tag


-- TODO: expected (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)
convertAddress :: (Integral a, Integral b) => a -> b -> String -> (a, a)
convertAddress binAddress bitsNum "setAssoc" =
  (tag,index) where
    tag = div binAddress (10^bitsNum)
    index = mod binAddress (10^bitsNum)

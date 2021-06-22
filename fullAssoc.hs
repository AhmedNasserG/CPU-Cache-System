{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec a = convertBinToDecHelper a 0
convertBinToDecHelper :: (Integral t1, Integral p, Integral t2) => t1 -> t2 -> p
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper x i = if mod x 10 == 1 then (2^i)+ convertBinToDecHelper (div x 10) (i+1) else convertBinToDecHelper (div x 10) (i+1)

logBase2 :: Floating a => a -> a
logBase2 = logBase 2

fillZeros :: (Eq t, Num t) => [Char] -> t -> [Char]
fillZeros s 0 = s
fillZeros s n = ['0'] ++ fillZeros s (n-1)

-- getNumBits:: (Floating a1, Integral a, RealFrac a1, Foldable t) => a1 -> [Char] -> t a2 -> a
getNumBits _ "fullAssoc" _ = 0
getNumBits numOfSets "setAssoc" _ = logBase2 numOfSets
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
splitEvery n list = x1 : (splitEvery n xs) 
  where (x1,xs) = splitAt n list

replaceInCache :: Int -> Int -> [a] -> [Item a] -> [Char] -> p2 -> (a, [Item a])
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = (dat,newCache)
     where 
       address = convertBinToDec tag
       dat = memory !! address
       itemToInsert = It (T tag) (D dat) True (-1)
       insertIdx = if haveTrash oldCache then getIdxOfTrash oldCache 0 else getIdxOfOldest oldCache
       tempCache = replaceIthItem itemToInsert oldCache insertIdx
       newCache = incrementCache tempCache

convertAddress :: (Integral a, Integral b) => a -> b -> [Char] -> (a, a)
convertAddress binAddress _ "fullyAssoc" = 
  (tag,idx)
  where 
  tag = binAddress
  idx = 0

haveTrash :: [Item a] -> Bool
haveTrash [] = False
haveTrash (It _ _ b _ :xs) = if b ==False then True else haveTrash xs 

getIdxOfTrash :: Num t => [Item a] -> t -> t
getIdxOfTrash (It _ _ b _ :xs) idx = if b == False then idx else getIdxOfTrash xs (idx+1)

getIdxOfOldestHelper :: Num a1 => [Item a2] -> a1 -> Int -> a1 -> a1
getIdxOfOldestHelper [] _ _ maxIdx = maxIdx
getIdxOfOldestHelper (It _ _ _ x :xs) idx max maxIdx = if x >= max then getIdxOfOldestHelper xs (idx+1) x idx else getIdxOfOldestHelper xs (idx+1) max maxIdx 

getIdxOfOldest :: Num a1 => [Item a2] -> a1
getIdxOfOldest (x:xs) = getIdxOfOldestHelper (x:xs) 0 (-1) 0
incrementCache :: [Item a] -> [Item a]
incrementCache [] = []
incrementCache ((It t d b x) :xs) = if b == True then ((It t d b (x+1)) :incrementCache xs) else ((It t d b x) :incrementCache xs)
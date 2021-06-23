{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

---------helper Functions------------
haveTrash :: [Item a] -> Bool
haveTrash [] = False
haveTrash (It _ _ b _ :xs) =  not b ||  haveTrash xs 

getIdxOfTrash :: Num t => [Item a] -> t -> t
getIdxOfTrash (It _ _ b _ :xs) idx = if not b then idx else getIdxOfTrash xs (idx+1)

getIdxOfOldestHelper :: Num a1 => [Item a2] -> a1 -> Int -> a1 -> a1
getIdxOfOldestHelper [] _ _ maxIdx = maxIdx
getIdxOfOldestHelper (It _ _ _ x :xs) idx max maxIdx = if x >= max then getIdxOfOldestHelper xs (idx+1) x idx else getIdxOfOldestHelper xs (idx+1) max maxIdx 

getIdxOfOldest :: Num a1 => [Item a2] -> a1
getIdxOfOldest (x:xs) = getIdxOfOldestHelper (x:xs) 0 (-1) 0
incrementCache :: [Item a] -> [Item a]
incrementCache [] = []
incrementCache ((It t d b x) :xs) = if b  then It t d b (x+1) :incrementCache xs else It t d b x :incrementCache xs 
------------------------------- full Assoc--------------


-------------- GetDataFromCache --------------
getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache stringAddress cache "setAssoc" bitsNum = loop t 0 (cacheSets!! convertBinToDec i)
  where
    setsNum = 2^bitsNum
    (t,i) = convertAddress (read stringAddress) setsNum "setAssoc"
    cacheSets = splitEvery (div (length cache) setsNum) cache
getDataFromCache stringAddress cache "fullyAssoc" bitsNum = loop t 0 cache 
  where
    t = read stringAddress
loop _ _ [] = NoOutput
loop t currHopsNum ((It (T t2) (D d) x _):xs) 
  | t2 == t && x = Out (d, currHopsNum)
  | otherwise = loop t (currHopsNum+1) xs

-- getDataFromCache stringAddress cache "directMap" bitsNum
--     |tag == (fromIntegral binTag) && validBit = Out (retrievedData,0)
--     |otherwise = NoOutput
--     where
--         (binTag,binIndex) = convertAddress (read stringAddress :: Integer) ( fromIntegral bitsNum) "directMap"
--         index = fromIntegral (convertBinToDec binIndex)
--         (It (T tag) (D retrievedData) validBit order) = cache!!index

-- TODO: expected (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)
convertAddress :: (Integral a, Integral b) => a -> b -> String -> (a, a)
convertAddress binAddress bitsNum "setAssoc" =
  (tag,index) where
    tag = div binAddress (10^bitsNum)
    index = mod binAddress (10^bitsNum)
convertAddress binAddress _ "fullyAssoc" = (tag,idx)
  where 
  tag = binAddress
  idx = 0    

------ReplaceInCache--------
replaceInCache :: Int -> Int -> [a] -> [Item a] -> [Char] -> p2 -> (a, [Item a])
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = (dat,newCache)
     where 
       address = convertBinToDec tag
       dat = memory !! address
       itemToInsert = It (T tag) (D dat) True (-1)
       insertIdx = if haveTrash oldCache then getIdxOfTrash oldCache 0 else getIdxOfOldest oldCache
       tempCache = replaceIthItem itemToInsert oldCache insertIdx
       newCache = incrementCache tempCache

-------implemented functions----------------
getData stringAddress cache memory cacheType bitsNum
  | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
  | otherwise = (getX x, cache)
  where
    x = getDataFromCache stringAddress cache cacheType bitsNum
    address = read stringAddress:: Int
    (tag, index) = convertAddress address bitsNum cacheType
    getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets =(d:prevData, finalCache)
  where
  bitsNum = round(logBase2 numOfSets)
  (d, updatedCache) = getData addr cache memory cacheType bitsNum
  (prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets



-------------------Direct-------------------
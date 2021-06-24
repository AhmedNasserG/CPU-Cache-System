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


fillZeros :: (Eq t, Num t) => [Char] -> t -> [Char]
fillZeros s 0 = s
fillZeros s n = ['0'] ++ fillZeros s (n-1)


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
getDataFromCache stringAddress cache "directMap" bitsNum 
  | x && tag == t = Out(d,0)
  | otherwise = NoOutput
  where 
    (tag,idx) = convertAddress (read stringAddress) bitsNum "directMap"
    (It (T t) (D d) x _) = cache!!(convertBinToDec idx)

getDataFromCache stringAddress cache "fullyAssoc" bitsNum = loop t 0 cache 
  where
    t = read stringAddress

getDataFromCache stringAddress cache "setAssoc" bitsNum = loop t 0 (cacheSets!! convertBinToDec i)
  where
    setsNum = 2^bitsNum
    (t,i) = convertAddress (read stringAddress) bitsNum "setAssoc"
    cacheSets = splitEvery (div (length cache) setsNum) cache

loop _ _ [] = NoOutput
loop t currHopsNum ((It (T t2) (D d) x _):xs) 
  | t2 == t && x = Out (d, currHopsNum)
  | otherwise = loop t (currHopsNum+1) xs



convertAddress :: (Integral a, Integral b) => a -> b -> String -> (a, a)
convertAddress binAddress bitsNum "directMap" = 
  (tag,index) where
    tag = div binAddress (10^bitsNum)
    index = mod binAddress (10^bitsNum)

convertAddress binAddress bitsNum "setAssoc" =
  (tag,index) where
    tag = div binAddress (10^bitsNum)
    index = mod binAddress (10^bitsNum)

convertAddress binAddress _ "fullyAssoc" = (tag,idx)
  where 
  tag = binAddress
  idx = 0    

------ReplaceInCache--------
replaceInCache:: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = (dat,newCache)
     where 
       address = convertBinToDec tag
       dat = memory !! address
       itemToInsert = It (T tag) (D dat) True (-1)
       insertIdx = if haveTrash oldCache then getIdxOfTrash oldCache 0 else getIdxOfOldest oldCache
       tempCache = replaceIthItem itemToInsert oldCache insertIdx
       newCache = incrementCache tempCache
replaceInCache tag idx memory oldCache "setAssoc" bitsNum = (dat,newCache)
  where  
    address = (show tag) ++ fillZeros (show idx) ( (fromIntegral bitsNum) - (length (show idx)))
    dat = memory !! (convertBinToDec (read address :: Int))
    newItem = It (T tag) (D dat) True (-1)
    numSets = 2^bitsNum
    splitedOldCache = splitEvery (div (length oldCache) numSets) oldCache
    selectedSet = splitedOldCache !! (convertBinToDec idx)
    indexToInsert = if haveTrash selectedSet then getIdxOfTrash selectedSet 0 else getIdxOfOldest selectedSet
    newSelecteSet = incrementCache (replaceIthItem newItem selectedSet indexToInsert)
    splitedNewCache = replaceIthItem newSelecteSet splitedOldCache (convertBinToDec idx)
    newCache = foldr (++) [] splitedNewCache
    


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



----------------- Replace in Cache  Set Assoc Test Cases --------------------

-- Examples:
-- > replaceInCache 0 1 ["100000","100001","100010", "100011","100100",
-- "100101","100110","100111"] [(It (T 00000) (D "10000") False 1),
-- (It (T 00000) (D "100000") True 0), (It (T 00010) (D "11100") False 3),
-- (It (T 00000) (D "11110") False 2)] "setAssoc" 1

-- > replaceInCache 11 1 ["100000","100001","100010", "100011","100100",
-- "100101","100110","100111"] [(It (T 00000) (D "10000") False 1),
-- (It (T 00000) (D "100000") True 0), (It (T 00001) (D "100011") True 0),
-- (It (T 00000) (D "100001") True 1)] "setAssoc" 1

-- > replaceInCache 0 0 ["100000","100001","100010", "100011","100100",
-- "100101","100110","100111"] [(It (T 00000) (D "10000") False 0),
-- (It (T 00000) (D "100000") True 0), (It (T 00010) (D "11100") False 3),
-- (It (T 00000) (D "11110") False 2)] "setAssoc" 1

-- > getData "000001" [It (T 0) (D "10000") False 1,
-- It (T 0) (D "100000") True 0, It (T 10) (D "11100") False 3,
-- It (T 0) (D "11110") False 2] ["100000","100001","100010",
-- "100011","100100","100101","100110","100111"] "setAssoc" 1

-- > getData "000001" [It (T 0) (D "10000") False 1,
-- It (T 0) (D "11000") True 0, It (T 10) (D "11100") False 3,
-- It (T 0) (D "11110") True 0] ["100000","100001","100010",
-- "100011","100100","100101","100110","100111"] "setAssoc" 1

-- > getData "000000" [It (T 0) (D "10000") False 1,
-- It (T 1) (D "11000") True 0, It (T 10) (D "11100") False 3,
-- It (T 0) (D "11110") True 0] ["100000","100001","100010",
-- "100011","100100","100101","100110","100111"] "setAssoc" 1

--runProgram ["000011","000100","000011","001100"] [It (T 0) (D "10000") False 0, It (T 0) (D "11000") False 0, It (T 1) (D "11100") False 3, It (T 1) (D "e") False 0] ["a", "b", "c", "d", "e", "f", "ab", "ac", "ad", "ae", "af", "a", "a", "a", "a", "aa", "a"] "setAssoc" 2

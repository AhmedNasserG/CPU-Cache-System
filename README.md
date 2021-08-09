# CPU_Cache_System

A simulation for data flow system between memory and cache using logic `Prolog` and functional `Haskel` Programming.

## Data Mapping Types
The program has 3 types for mapping data in Cache :
1) `Direct Mapping` : map every piece of data in specific location in Cache according to idx `part of the address in memory` 
2) `Set Associative` :map every piece of data to specific set (then data goes into any empty cell at this set` in Cache according to idx 
3) `Fully Associative` : map every piece of data to the first empty place in Cache (take much more time to retrieve data and replace it as it search in the whole Cache .

## Functions  :
 
 for every data Mapping type there are 3 kinds of functions (Predicates):
 1) `getDataFromCache` which search and retrieve the needed data from the Cache according to its Tag (id) and the type of Mapping
 2) `replaceInCache` which given the memory , cache and the address of the data in the memory it get the data from memory put it into the cache according to the type of mapping
 3) `convertAddress` which given the address in memory it convert it to Tag and Idx to be used in the above functions

## Running the program :
using `GHCI` for haskel and `swipl` for prolog you can run this program 

### Haskel function
`runProgram adressList oldCache memory cacheType numOfSets` given a list for required addresses of data , memory and oldCache it return a tuple containing the retrieved data and final state of Cache 

### Prolog equaivalent predicate
`runProgram(AdressList,OldCache,Mem,FinalCache,OutputDataList,StatusList,
directMap,NumOfSets)`

`Personal opinion` this project was much suitable for logic programming as it depends on searching and binding which made implementing it with logic programing easier

 
## Contributors

- [Shimaa Ahmed](https://github.com/ShimaaBetah)
- [Ahmed Nasser](https://github.com/AhmedNasserG)
- [Mohammad Omar](https://github.com/MohammadOTaha)
- [Ibrahim Abou Elenein](https://github.com/aboueleyes)

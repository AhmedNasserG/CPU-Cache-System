
convertBinToDec(A,B):-
  string_chars(A,A1),
  reverse(A1,A2),
  convertBinToDecHelper(A2,0,B).

convertBinToDecHelper([],_,0). 

convertBinToDecHelper([H|T],Idx,D):-
   atom_number(H,H1),
   H1 = 1,
   D1 is 2**Idx,
   Idx1 is Idx+1,
   convertBinToDecHelper(T,Idx1,D2),
   D is D1 + D2.

convertBinToDecHelper([H|T],Idx,D):-
   atom_number(H,H1),
   H1 = 0,
   Idx1 is Idx+1,
   convertBinToDecHelper(T,Idx1,D2),
   D is D2.

% -----------------------------------

replaceIthItem(X,L,Idx,R):- replaceIthItemHelper(X,L,Idx,0,R).

replaceIthItemHelper(_,[],_,_,[]).

replaceIthItemHelper(X,[_|T],Idx,Idx,[X|T]).

replaceIthItemHelper(X,[H|T],Idx,Ac,[H|R]):-
	Idx \== Ac,
	Ac2 is Ac + 1,
	replaceIthItemHelper(X,T,Idx,Ac2,R).

% -----------------------------------

splitEvery(N,L,R):- splitEveryHelper(N,0,L,[],R).
splitEveryHelper(_,_,[],Tmp,[TmpReversed]):-
   reverse(Tmp,TmpReversed).
splitEveryHelper(N,C,[H|T],Tmp,R):-
   N \= C,
   C1 is C + 1,
   splitEveryHelper(N,C1,T,[H|Tmp],R).

splitEveryHelper(N,N,L,Tmp,[TmpReversed|R]):-
   L \== [],
   splitEveryHelper(N,0,L,[],R),
   reverse(Tmp,TmpReversed).
% -----------------------------------
logBase2(1,0).
logBase2(N,Ans):-
    N > 1,
    N1 is N/2,
    logBase2(N1, A),
    Ans is A + 1.
	
% -----------------------------------

getNumBits(_,fullyAssoc,_,0).
getNumBits(NumofSets,setAssoc,_,NumBits):-
   logBase2(NumofSets,NumBits).
getNumBits(_,directMap,L,NumBits):-
   length(L, S),
   logBase2(S,NumBits).
% -----------------------------------

fillZeros(String,0,String).
fillZeros(String,N,Res):-
   N > 0,
   N1 is N-1,
   fillZeros(String,N1,Res1),
   string_concat("0", Res1, Res).
   
% -----------------------------------
getDataFromCache(StringAddress, L, Data, HopsNum, directMap, BitsNum):-
	atom_number(StringAddress, Address),
	convertAddress(Address, BitsNum, Tag, Idx, directMap),
	convertBinToDec(Idx, DecIdx),
	nth0(DecIdx, L, item(tag(StrTag), data(Data), 1,_)),
	atom_number(StrTag, Tag),
	HopsNum is 0.
	
%-----------------------------------
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
    Tag is Bin // (10**BitsNum),
    Idx is Bin - (Tag*(10**BitsNum)).


% replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
% replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
   atom_string(Tag,StrTag),
   convertBinToDec(StrTag,DecAdress),
   convertBinToDec(Idx, DecIdx),
   atom_string(Idx,StrIdx),
   string_concat(DecAdress,StrIdx,StringAddress),
   convertBinToDec(StringAddress,DecAdd),
   nth0(DecAdd,Mem,ItemData),
   zerosNeeded(OldCache,StrTag,Zeros),
   fillZeros(StrTag,Zeros,InsertTag),
   replaceIthItem(item(tag(InsertTag),data(ItemData),1,0),OldCache,DecIdx,NewCache).



    
%-----------------------------------
getDataFromCache(StringAddress, Cache, Data,HopsNum,fullyAssoc,BitsNum):-
	atom_number(StringAddress, Address),
	convertAddress(Address, BitsNum, Tag, Idx, fullyAssoc),
	atom_number(StrTag, Tag),
    length(Cache,L),
    traverse(0,L,Cache,Tag,0,HopsNum,Data).
   

%-----------------------------------
convertAddress(Bin,BitsNum,Tag,_,fullyAssoc):-
   Bin = Tag.
    
%-----------------------------------
incrementIfNotTrash([],[]).
incrementIfNotTrash([item(tag(Tag), data(Data), 1,Curr)|T],[item(tag(Tag), data(Data), 1,NewCurr)|S]):-
   NewCurr is Curr + 1,
   incrementIfNotTrash(T,S).
incrementIfNotTrash([item(tag(Tag), data(Data), 0,Curr)|T],[item(tag(Tag), data(Data), 0,Curr)|S]):-
   incrementIfNotTrash(T,S).
%-----------------------------------
getIdxOfTrash([item(tag(_), data(_), 0,_)|_],Idx,Idx).
getIdxOfTrash([item(tag(_), data(_), 1,_)|T],Idx,Res):-
   NewIdx is Idx +1,
   getIdxOfTrash(T,NewIdx,Res).
%-----------------------------------

zerosNeeded([item(tag(Tag),_,_,_)|_],StrTag,Res):-
   string_length(Tag, L1),
   string_length(StrTag,L2),
   Res is L1 -L2.
%-----------------------------------
getIdxOfOldest([],_,_,Acc,Acc).

getIdxOfOldest([item(tag(_),data(_),_,X)|T],MaxSoFar,Idx,Acc,Res):-
   X >= MaxSoFar,
   NewIdx is Idx+1,
   getIdxOfOldest(T,X,NewIdx,Idx,Res).

getIdxOfOldest([item(tag(_),data(_),_,X)|T],MaxSoFar,Idx,Acc,Res):-
   MaxSoFar > X,
   NewIdx is Idx+1,
   getIdxOfOldest(T,X,NewIdx,Acc,Res).



   
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
   atom_string(Tag,StrTag),
   convertBinToDec(StrTag,DecAdress),
   nth0(DecAdress,Mem,ItemData),
   zerosNeeded(OldCache,StrTag,Zeros),
   fillZeros(StrTag,Zeros,InsertTag),
   getIdxOfTrash(OldCache,0,InsertIdx),
   replaceIthItem(item(tag(InsertTag),data(ItemData),1,-1),OldCache,InsertIdx,TempCache),
   incrementIfNotTrash(TempCache,NewCache).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
   atom_string(Tag,StrTag),
   convertBinToDec(StrTag,DecAdress),
   nth0(DecAdress,Mem,ItemData),
   zerosNeeded(OldCache,StrTag,Zeros),
   fillZeros(StrTag,Zeros,InsertTag),
   \+ getIdxOfTrash(OldCache,0,InsertIdx),
   getIdxOfOldest(OldCache,0,0,0,IdxToInsert),
   replaceIthItem(item(tag(InsertTag),data(ItemData),1,-1),OldCache,IdxToInsert,TempCache),
   incrementIfNotTrash(TempCache,NewCache).

% --------- Set Associative ---------
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	atom_number(StringAddress, Address),
	convertAddress(Address, SetsNum, Tag, Idx, setAssoc),
	length(Cache, L),
	((PartitionSize is L // SetsNum, 0 is L mod (SetsNum)); (PartitionSize is (L // SetsNum) + 1, \+ (0 is L mod (SetsNum)))),
	convertBinToDec(Idx, DecIdx),
	DecIdx2 is DecIdx * PartitionSize,
	End is DecIdx2 + PartitionSize,
	traverse(DecIdx2, End, Cache, Tag, 0, HopsNum, Data).
	
	
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
   getNumBits(SetsNum,setAssoc,_,NumBits),
   Tag is Bin // (10**NumBits),
   Idx is Bin mod (10**NumBits).

traverse(Start,End,Cache,TargetTag,HopsAc,HopsAc,Data):- 
	Start =< End,
	nth0(Start,Cache,item(tag(StSrTag),data(Data),1,_)),
	atom_number(StrTag,TargetTag).
	
traverse(Start,End,Cache,TargetTag,HopsAc,HopsNum,Data):-
	Start =< End,
	Start2 is Start + 1,
	HopsAc2 is HopsAc + 1,
	traverse(Start2,End,Cache,TargetTag,HopsAc2,HopsNum,Data).

% -----------------------------------

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
   string_concat(Tag, Idx, StringAddress),
   atom_number(StringAddress,Address),
   convertBinToDec(Address,DecAdress),
   nth0(DecAdress,Mem,ItemData),

   convertBinToDec(Idx,DecIdx),
   splitEvery(SetsNum,OldCache,OldCacheSplited),
   nth0(DecIdx,OldCacheSplited,Set),

   atom_string(Tag,StrTag),
   zerosNeeded(OldCache,StrTag,Zeros),
   fillZeros(StrTag,Zeros,InsertTag),

   (
   getIdxOfTrash(Set,0,InsertIdx);
   (\+getIdxOfTrash(Set,0,InsertIdx),getIdxOfOldest(Set,0,0,0,InsertIdx))
   ),
   
   replaceIthItem(item(tag(InsertTag),data(ItemData),1,-1),Set,InsertIdx,TempNewSet),
   incrementIfNotTrash(TempNewSet,NewSet),
   replaceIthItem(NewSet,OldCacheSplited,DecIdx,NewCacheSplited),
   splitEvery(SetsNum,NewCache,NewCacheSplited).
% -----------------------------------




% ----- Implemented Predicates ------
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
   getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
   NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
   \+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
   atom_number(StringAddress,Address),
   convertAddress(Address,BitsNum,Tag,Idx,Type),
   replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
   getNumBits(NumOfSets,Type,OldCache,BitsNum),
   getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,Status),
   runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
   Type,NumOfSets).

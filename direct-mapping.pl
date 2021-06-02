getDataFromCache(_,[item(_,data(N),1,_) | _],Data,HopsNum,directMap):-
    Data is N,
    HopsNum is 0.

getDataFromCache(_,[item(_,_,0,_) | T],Data,HopsNum,directMap):-
    getDataFromCache(_,T,Data,HopsNum,directMap).


convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
    Tag is Bin // (10**BitsNum),
    Idx is Bin - (Tag*(10**BitsNum)).


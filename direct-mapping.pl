getDataFromCache(_,[item(_,data(N),1,_) | _],Data,HopsNum,directMap):-
    Data is N,
    HopsNum is 0.

getDataFromCache(_,[item(_,_,0,_) | T],Data,HopsNum,directMap):-
    getDataFromCache(_,T,Data,HopsNum,directMap).

pow(_,0,1).

pow(X,Y,Z) :- Y1 is Y - 1,
              pow(X,Y1,Z1), Z is Z1*X.

convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
    Tag is Bin // (10**BitsNum),
    Idx is Bin - (Tag*(10**BitsNum)).


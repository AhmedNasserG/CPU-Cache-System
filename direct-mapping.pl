getDataFromCache(_,[item(tag(_),data(N),1,_) | T],Data,HopsNum,directMap):-
    Data is N,
    HopsNum is 0.

getDataFromCache(_,[item(tag(_),data(N),0,_) | T],Data,HopsNum,directMap):-
    getDataFromCache(_,T,Data,HopsNum,directMap).

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

splitEvery(N,L,R):- splitEveryHelper(N,L,0,[],R).

splitEveryHelper(_,[],_,Tmp,[Tmp]).

splitEveryHelper(N,L,N,Tmp,R2):-
	L \= [],
	splitEveryHelper(N,L,0,[],R1),
	append([Tmp],R1,R2).

splitEveryHelper(N,[H|T],Ac,Tmp,R):-
	N \== Ac,
	append(Tmp,[H],Tmp2),
	Ac2 is Ac + 1,
	splitEveryHelper(N,T,Ac2,Tmp2,R).

% -----------------------------------
logBase2(1,0).
logBase2(N,Ans):-
    N > 1,
    N1 is N/2,
    logBase2(N1, A),
    Ans is A + 1.

getNumBits(_,fullyAssoc,_,0).
getNumBits(NumofSets,setAssoc,_,NumBits):-
   logBase2(NumofSets,NumBits).
getNumBits(_,directMap,L,NumBits):-
   length(L, S),
   logBase2(S,NumfBits).

fillZeros(String,0,String).
fillZeros(String,N,Res):-
   N > 0,
   N1 is N-1,
   fillZeros(String,N1,Res1),
   string_concat("0", Res1, Res).
   


   
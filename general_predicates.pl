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

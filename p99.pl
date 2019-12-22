%% P01 (*): Find the last element of a list
%% > ?-mylast(X, [1,2,3,4,5]).
%% X = 5

mylast(X, [X]) :- !.
mylast(X, [_|L]) :- mylast(X, L).

%% P02 (*): Find the last but one element of a list
%% > ?-last_but_one(X, [1,2,3,4,5]).
%% X = 4

last_but_one(X, [X,_]) :- !.
last_but_one(X, [_,Y|Ys]) :- last_but_one(X, [Y|Ys]).

% P03 (*): Find the K'th element of a list.
% The first element in the list is number 1.
%% > ?-element_at(X, [1,2,three,4,5], 3).
%% X = three
element_at(X,[X|_],1) :- !.
element_at(X,[_|L],K) :- K > 1,
   K1 is K - 1, element_at(X,L,K1).

% P04 (*): Find the number of elements of a list.
%% > ?-length([1,2,3,4,5], L).
%% L = 5

length([],0) :- !.
length([_|L],N) :- length(L,N1),
                  N is N1 + 1.

% P05 (*): Reverse a list.
%% > ?-reverse([1,2,3,4],L).
%% L = [4,3,2,1]

reverse(L1,L2) :- my_rev(L1,L2,[]).

my_rev([],L2,L2) :- !.
my_rev([X|Xs],L2,Acc) :- my_rev(Xs,L2,[X|Acc]).

% P06 (*): Find out whether a list is a palindrome
%% > ?-is_palindrome([s,u,b,i,d,u,r,a,a,r,u,d,i,b,u,s]).
%% yes

%% Ignore spaces in tests for palindrome. Palindromes
%% in Latin (Roman did not use spaces):
%  subi dura a rudibus -- endure rudeness from peasants
%  ablata at alba -- secluded but pure
%  roma tibi subito motibus ibit amor -- 
%    in Rome quickly with its bustle you will find love
%  in girum imus nocte et consumimur igni --
%     we go about in the night and are consumed by fire
  
is_palindrome(L) :- reverse(L,L).


% P07 (**): Flatten a nested list structure.
%% > ?-flatten([3,4,[a,[b]], 5], L).
%% L = [3,4,a,b,5]

append([],L,L) :- !.
append([H|T], L, [H|U]) :- append(T,L,U).

flatten([],[]) :- !.
flatten([X|Xs],Zs) :- !, flatten(X,Y),
   flatten(Xs,Ys), append(Y,Ys,Zs).
flatten(X, [X]).

% P08 (**): Eliminate consecutive duplicates of list elements.
%% > ?-compress([2,3,3,3,4,4,5], L).
%% L = [2,3,4,5]
compress([],[]) :- !.
compress([X],[X]) :- !.
compress([X,X|Xs],Zs) :-  !, compress([X|Xs],Zs).
compress([X,Y|Ys],[X|Zs]) :-  compress([Y|Ys],Zs).


% P09 (**):  Pack consecutive duplicates of list elements.
%% > ?-pack([2,3,3,3,4,4,5], L).
%% L = [[2],[3,3,3],[4,4],[5]]

pack([],[]) :- !.
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed and transfered to Z

transfer(X,[],[],[X]) :- !.
transfer(X,[X|Xs],Ys,[X|Zs]) :- !, transfer(X,Xs,Ys,Zs).
transfer(X,[Y|Ys],[Y|Ys],[X]).


% P10 (*):  Run-length encoding of a list
%% > ?-encode([a,a,a,b,b,b,b,b,c,d], Ans).
%% Ans = [[3,a],[5,b],[1,c],[1,d]]

encode(L1,L2) :- pack(L1,L), transform(L,L2).

transform([],[]) :- !.
transform([[X|Xs]|Ys],[[N,X]|Zs]) :- length([X|Xs],N),
                             transform(Ys,Zs).

% P11 (*):  Modified run-length encoding
%% > ?-encode_modified([1,1,2,2,2,2,2,3,3,4], G).
%% G = [[2,1],[5,2],[2,3],4]

encode_modified(L1,L2) :- encode(L1,L), strip(L,L2).

strip([],[]) :- !.
strip([[1,X]|Ys],[X|Zs]) :-  !, strip(Ys,Zs).
strip([[N,X]|Ys],[[N,X]|Zs]) :- N > 1, strip(Ys,Zs).


% P12 (**): Decode a run-length compressed list.
%% > ?-encode([a,a,a,b,b,b,b,b,c,d], Ans), decode(Ans, G).
%% Ans = [[3,a],[5,b],[1,c],[1,d]]
%%G = [a,a,a,b,b,b,b,b,c,d]

decode([],[]) :- !.
decode([[1,X]|Ys],[X|Zs]) :-  !, decode(Ys,Zs).
decode([[N,X]|Ys],[X|Zs]) :- N > 1, !, N1 is N - 1,
           decode([[N1,X]|Ys],Zs).
decode([X|Ys],[X|Zs]) :-  decode(Ys,Zs).
%%bug? decode([X|Ys],[X|Zs]) :- \+ is_list(X), decode(Ys,Zs).
%%where: is_list([H|T]).
% P13 (**): Run-length encoding of a list (direct solution) 
%% > ?-encode_direct([1,1,2,2,2,2,2,3,3,4], G).
%% G = [[2,1],[5,2],[2,3],4]

encode_direct([],[]) :- !.
encode_direct([X|Xs],[Z|Zs]) :- count(X,Xs,Ys,1,Z),
                     encode_direct(Ys,Zs).

count(X,[],[],1,X) :- !.
count(X,[],[],N,[N,X]) :- N > 1, !.
count(X,[Y|Ys],[Y|Ys],1,X) :- X \= Y.
count(X,[X|Xs],Ys,K,T) :-  !,
   K1 is K + 1, count(X,Xs,Ys,K1,T).
count(X,[Y|Ys],[Y|Ys],N,[N,X]).
%%bug? count(X,[Y|Ys],[Y|Ys],N,[N,X]) :- N > 1.
% P14 (*): Duplicate the elements of a list
%% > ?-dupli([1,2,2,3,3,3,5], L).
%% L = [1,1,2,2,2,2,3,3,3,3,3,3,5,5]

dupli([],[]) :- !.
dupli([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).

% P15 (**): Duplicate the elements of a list agiven number of times
%% > ?-dupli([1,2,2,3,3,3,5], 4,  L).
%% L = [1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,5,5,5,5]

dupli(L1,N,L2) :- dupli(L1,N,L2,N).

dupli([],_,[],_) :- !.
dupli([_|Xs],N,Ys, 0) :-  !, dupli(Xs,N,Ys,N).
dupli([X|Xs],N,[X|Ys],K) :- K > 0, K1 is K - 1,
                dupli([X|Xs],N,Ys,K1).

% P16 (**):  Drop every N'th element from a list
%% > ?-drop([1,2,3,4,5,6], 3, R).
%% R = [1,2,4,5]

drop(L1,N,L2) :- drop(L1,N,L2,N).

drop([],_,[],_) :- !.
drop([X|Xs],N,[X|Ys],K) :- K > 1, !,
       K1 is K - 1, drop(Xs,N,Ys,K1).
drop([_|Xs],N,Ys, K) :-  drop(Xs,N,Ys,N).

% P17 (*): Split a list into two parts
%% > ?- mysplit([2,5,3,8,1], 3, X, Y).
%% X = [2,5,3]
%% Y = [8,1]

mysplit([X|Xs],N,[X|Ys],Zs) :- N > 0, !, N1 is N - 1,
                 mysplit(Xs,N1,Ys,Zs).
mysplit(L,0,[],L).


% P18 (**):  Extract a slice from a list
%% > ?- myslice([1,2,3,4,5,6,7], 2,4, R).
%% R = [2,3,4]

myslice([X|_],1,1,[X]) :- !.
myslice([X|Xs],1,K,[X|Ys]) :- K > 1, !, 
   K1 is K - 1, myslice(Xs,1,K1,Ys).
myslice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, myslice(Xs,I1,K1,Ys).


% P19 (**): Rotate a list N places to the left 
%% > ?-rotate([1,2,3,4,5,6,7], 2, R).
%% R = [3,4,5,6,7,1,2]

rotate(L1,N,L2) :- N >= 0, !, 
   length(L1,NL1), N1 is N mod NL1,
   rotate_left(L1,N1,L2).
rotate(L1,N,L2) :- 
   length(L1,NL1), N1 is NL1 + (N mod NL1),
   rotate_left(L1,N1,L2).

rotate_left(L,0,L) :- !.
rotate_left(L1,N,L2) :- 
    mysplit(L1,N,S1,S2), append(S2,S1,L2).

% P20 (*): Remove the K'th element from a list.
%% > ?-remove_at(X, [1,2,3,4,5,6], 3, L).
%% X = 3
%% L = [1,2,4,5,6]

remove_at(X,[X|Xs],1,Xs) :- !.
remove_at(X,[Y|Xs],K,[Y|Ys]) :-  
   K1 is K - 1, remove_at(X,Xs,K1,Ys).


% P21 (*): Insert an element at a given position into a list
%% > ?-insert_at(a,[1,2,3,4,5], 3, L).
%% L = [1,2,a,3,4,5]

insert_at(X,L,K,R) :- remove_at(X,R,K,L).


% P22 (*):  Create a list containing all integers within a given range.
%% > ?-range(4,9,L).
%% L = [4,5,6,7,8,9]

range(I,K,[I]) :- I >= K, !.
range(I,K,[I|L]) :- I < K, 
    I1 is I + 1, range(I1,K,L).


% P23 (**): Extract a given number of randomly
% selected elements from a list.
%% (load "rnd.lisp") the Lisp function
%% that generates random numbers.
%% Study file "rnd.lisp" to learn how to
%% implement new primitives.
%% > ?- rnd_select([1,2,3,4,5,6], 3, L).
%% L = [6,2,4]

rnd_select(_,K,[]) :- K < 1, !.
rnd_select(Xs,N,[X|Zs]) :-  N > 0,
    length(Xs,L),
    random(R, L), I is R + 1,
    remove_at(X,Xs,I,Ys),
    N1 is N - 1,
    rnd_select(Ys,N1,Zs).


% P24 (*): Lotto: Draw N different random numbers from  1..M
%% > ?-lotto(6,49,L).
%% L = [44,36,39,5,28,34]

lotto(N,M,L) :- range(1,M,R), rnd_select(R,N,L).


% P25 (*):  Generate a random permutation of the elements of a list
%% > ?-rnd_permu([1,2,3,4,5], L).
%% L = [2,4,3,1,5]

rnd_permu(L1,L2) :- length(L1,N), rnd_select(L1,N,L2).


% P26 (**):  Generate the combinations of k distinct objects
%            chosen from the n elements of a list.

% combination(K,L,C) :- C is a list of K distinct elements 
%    chosen from the list L
%% > ?- combination(3, [a,b,c,d], L).
%% L = [a,b,c]
%% ?;
%% L = [a,b,d]
%% ?;
%% L = [a,c,d]
%% ?;
%% L = [b,c,d]
%% ?;
%%
%% no.

combination(0,_,[]).
combination(K,L,[X|Xs]) :- K > 0,
   el(X,L,R), K1 is K-1, combination(K1,R,Xs).

% Find out what the following predicate el/3 exactly does.

el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).

atm(X) :- atomic(X).
atm(X) :- var(X).

var_memberchk(A0, [A1|_]) :- 
     A0 == A1, !, atm(A0).
var_memberchk(A0, [_|R]) :- 
    var_memberchk(A0, R).

\+(P) :- call(P), !, fail.
\+(P).

subtract([], _, []).
subtract([A|C], B, D) :-
    var_memberchk(A, B), 
    subtract(C, B, D).
subtract([A|B], C, [A|D]) :-
    \+ var_memberchk(A,C),
    subtract(B, C, D).


% P27 (**) Group the elements of a set into disjoint subsets.

% Problem a)

%% > ?-group3([aldo, beat, carla, david,evi, flip, gary, hugo, ida], G1,G2,G3).
% group3(G,G1,G2,G3) :- distribute G into G1, G2, and G3,
% so that G1, G2 and G3 have 2,3 & 4 elements respectively
%% > ?-group3([aldo, beat, carla, david,
%%              evi, flip, gary, hugo, ida], G1,G2,G3).
%% G1 = [aldo,beat]
%% G2 = [carla,david,evi]
%% G3 = [flip,gary,hugo,ida]
%% ?;
%% G1 = [aldo,beat]
%% G2 = [carla,david,flip]
%% G3 = [evi,gary,hugo,ida]
%% ?;
group3(G,G1,G2,G3) :- 
   selectN(2,G,G1),
   subtract(G,G1,R1),
   selectN(3,R1,G2),
   subtract(R1,G2,R2),
   selectN(4,R2,G3),
   subtract(R2,G3,[]).

% selectN(N,L,S) :- select N elements of list L
%    and put them in set S. Via backtracking return
%    all posssible selections, but avoid permutations;
%    i.e. after generating S = [a,b,c] do not return
%    S = [b,a,c], etc.

selectN(0,_,[]) :- !.
selectN(N,L,[X|S]) :- N > 0, 
   el(X,L,R), 
   N1 is N-1,
   selectN(N1,R,S).

% Problem b): Generalization

% group(G,Ns,Gs) :- distribute G into the groups Gs.
%    The group sizes are given in the list Ns.
%% > ?- group([aldo,beat,carla,david,evi,
%%             flip,gary,hugo,ida],[2,2,5],Gs).
%% Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
%% ?;
%% Gs = [[aldo,beat],[carla,evi],[david,flip,gary,hugo,ida]]
%% ?y

group([],[],[]).
group(G,[N1|Ns],[G1|Gs]) :- 
   selectN(N1,G,G1),
   subtract(G,G1,R),
   group(R,Ns,Gs).

% P28 (**) Sorting a list of lists according to length
%
% a) length sort
%
% lsort(InList,OutList) :- it is supposed that the
% elements of InList  are lists themselves. Then
% OutList is obtained from InList by sorting 
% its elements according to their length.
% lsort/2 sorts ascendingly, lsort/3 allows for
% ascending or descending sorts.
% (list_of_lists,list_of_lists), (+,?)

lsort(InList,OutList) :- lsort(InList,OutList,asc).

% sorting direction Dir is either asc or desc

partition(LP, [], [], []) :- !.
partition(Lp-P, [L-X|Xs], [L-X|G], S) :-
	L < Lp, !, partition(Lp-P, Xs, G, S).
partition(P, [LX|Xs], G, [LX|S]) :-
	partition(P, Xs, G, S).

keysort([], []) :- !.
keysort([X], [X]) :- !.
keysort([P|Xs], Ys) :- partition(P, Xs, Smaller, Greater),
     keysort(Smaller, S),
     keysort(Greater, G),
     append(S, [P|G], Ys).

lsort(InList,OutList,Dir) :-
   add_key(InList,KList,Dir),
   keysort(KList,SKList),
   rem_key(SKList,OutList).

add_key([],[],_).
add_key([X|Xs],[L-p(X)|Ys],asc) :- !, 
	length(X,L), add_key(Xs,Ys,asc).
add_key([X|Xs],[L-p(X)|Ys],desc) :- 
	length(X,L1), L is -L1, add_key(Xs,Ys,desc).

rem_key([],[]).
rem_key([_-p(X)|Xs],[X|Ys]) :- rem_key(Xs,Ys).

% b) length frequency sort
%
% lfsort (InList,OutList) :- it is supposed that the
% elements of InList are lists themselves. Then OutList
% is obtained from InList by sorting its elements according
% to their length frequency; i.e. in the default,  where
% sorting is done ascendingly, lists with rare lengths are
% placed first, other with more frequent lengths come later.
%
% Example:
% ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
% L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
%
% Note that the first two lists in the Result have length 4 and 1, both
% length appear just once. The third and forth list have length 3 which
% appears, there are two list of this length. And finally, the last
% three lists have length 2. This is the most frequent length.

lfsort(InList,OutList) :- lfsort(InList,OutList,asc).

% sorting direction Dir is either asc or desc

lfsort(InList,OutList,Dir) :-
	add_key(InList,KList,desc),
   keysort(KList,SKList),
   pak(SKList,PKList),
   lsort(PKList,SPKList,Dir),
   flatten(SPKList,FKList),
   rem_key(FKList,OutList).
   
pak([],[]).
pak([L-X|Xs],[[L-X|Z]|Zs]) :- trnsf(L-X,Xs,Ys,Z), pak(Ys,Zs).

% transf(L-X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of length L are removed and transfed to Z

notequal(X,X) :- !, fail.
notequal(X, P).

trnsf(_,[],[],[]).
trnsf(L-_,[K-Y|Ys],[K-Y|Ys],[]) :- notequal(L, K).
trnsf(L-_,[L-X|Xs],Ys,[L-X|Zs]) :- trnsf(L-X,Xs,Ys,Zs).

%% Remove if period.pl is loaded
=(X,X).

test :-
   L= [[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],
   write('L = '), write(L), nl,
   lsort(L,LS),
   write('LS = '), write(LS), nl,
   lsort(L,LSD,desc),
   write('LSD = '), write(LSD), nl,
   lfsort(L,LFS),
   write('LFS = '), write(LFS), nl.

% P31 (**) Determine whether a given integer number is prime. 

% is_prime(P) :- P is a prime number
%    (integer) (+)

is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

% P32 (**) Determine the greatest common divisor of
% two positive integers.
%% > ?- gcd(36,63, F).
%% F = 9
%% ?;

gcd(X,0,X).
gcd(X,Y,G) :- Y > 0, Z is X mod Y, gcd(Y,Z,G).


% P33 (*) Determine whether two positive integer
% numbers are coprime. Two numbers are coprime if
% their greatest common divisor equals 1.
%% > ?- coprime(35,64).
%% yes.

coprime(X,Y) :- gcd(X,Y,1).

% P34 (**) Calculate Euler's totient function phi(m).
%%% ?- totient_phi(10, P).

totient_phi(1,1) :- !.
totient_phi(M,Phi) :- t_phi(M,Phi,1,0).

% t_phi(M,Phi,K,C) :- Phi = C + N, where N is the
% number of integers R such that K <= R < M and R
% is coprime to M.
%    (integer,integer,integer,integer) (+,-,+,+)

t_phi(M,Phi,M,Phi) :- !.
t_phi(M,Phi,K,C) :- 
   K < M, coprime(K,M), !, 
   C1 is C + 1, K1 is K + 1,
   t_phi(M,Phi,K1,C1).
t_phi(M,Phi,K,C) :- 
   K < M, K1 is K + 1,
   t_phi(M,Phi,K1,C).


% P35 (**) Determine the prime factors of a given
% positive integer. 
%%%% > ?- prime_factors(315, L).
%%%% L = [3,3,5,7]
%
% prime_factors(N, L) :- N is the list of prime
% factors of N.
%    (integer,list) (+,?)

prime_factors(N,L) :- N > 0,  prime_factors(N,L,2).

% prime_factors(N,L,K) :- L is the list of prime
% factors of N. It is  known that N does not have
% any prime factors less than K.

prime_factors(1,[],_) :- !.
prime_factors(N,[F|L],F) :-  % N is multiple of F
   R is N // F, N =:= R * F, !, prime_factors(R,L,F).
prime_factors(N,L,F) :- 
   next_factor(N,F,NF),
   prime_factors(N,L,NF). % N is not multiple of F
   

% next_factor(N,F,NF) :- when calculating the prime
% factors of N and if F does not divide N then NF
% is the next larger candidate to be a factor of N.

next_factor(_,2,3) :- !.
next_factor(N,F,NF) :- F * F < N, !, NF is F + 2.
next_factor(N,_,N).   % F > sqrt(N)


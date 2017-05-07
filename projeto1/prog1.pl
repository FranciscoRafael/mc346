allpermutation(L, Z) :- findall(X, permutation(L, X), Z).

hardcoder(L) :- maplist(assert, L).

add_ini([],_, []). 
add_ini([C|R], IT, L):- add_ini(R, IT, Z), append_ini(C, IT, NL), append([NL], Z, L).

add_end([],_, []). 
add_end([C|R], IT, L):- add_end(R, IT, Z), append_end(C, IT, NL), append([NL], Z, L).

remove([],_,[]).
remove([C|R], C, R). 
remove([C|R], K, RE) :- remove(R, K, RES), append([C], RES, RE).

append_end(L, A, X) :- append(L, [A], X).
append_ini(L, A, X):- append([A], L, X).


calc_custo([_|[]], ACC, V):- V = ACC. 
calc_custo([C|R], ACC, V):- [P|_] = R, arco(C, P, Z), VPP is ACC + Z, calc_custo(R, VPP, V). 

calc_maior([], M, V, ACC, T):- V = M, T = ACC.
calc_maior([C|R], M, V, ACC, T):- (calc_custo(C, 0, X) -> (X < M -> calc_maior(R, X, V, C, T); calc_maior(R, M, V, ACC, T)); calc_maior(R, M, V, ACC, T)).



main :- read(X),
		read(Y),
		read(Z),
		hardcoder(Y),
		remove(X, Z, W), 
		allpermutation(W, G), 
		add_end(G, Z, K),
		add_ini(K, Z, LSS),
		sort(LSS, SLSS),
		calc_maior(SLSS, 999999999999999999999999999999999999999, V, [], T),
		(T = [] -> print(nada), nl; print(V), nl, print(T), nl).


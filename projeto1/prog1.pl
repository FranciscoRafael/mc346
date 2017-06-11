/* Francisco Rafael Capiteli Carneiro - RA: 157888 */

/* Gera todas as permutações de uma lista */

allpermutation(L, Z) :- findall(X, permutation(L, X), Z).

/* gera todos os arcos */
hardcoder(L) :- maplist(assert, L).

/* adiciona um nó no inicio */
add_ini([],_, []). 
add_ini([C|R], IT, L):- add_ini(R, IT, Z), append_ini(C, IT, NL), append([NL], Z, L).


/* adiciona um nó no final */
add_end([],_, []). 
add_end([C|R], IT, L):- add_end(R, IT, Z), append_end(C, IT, NL), append([NL], Z, L).


/* remove um nó */
remove([],_,[]).
remove([C|R], C, R). 
remove([C|R], K, RE) :- remove(R, K, RES), append([C], RES, RE).


append_end(L, A, X) :- append(L, [A], X).
append_ini(L, A, X):- append([A], L, X).

/* calcula o custo de uma lista */

calc_custo([_|[]], ACC, V):- V = ACC. 
calc_custo([C|R], ACC, V):- [P|_] = R, arco(C, P, Z), VPP is ACC + Z, calc_custo(R, VPP, V). 

/* guarda o menor custo de uma lista e seu nó */
calc_maior([], M, V, ACC, T):- V = M, T = ACC.
calc_maior([C|R], M, V, ACC, T):- (calc_custo(C, 0, X) -> (X < M -> calc_maior(R, X, V, C, T); calc_maior(R, M, V, ACC, T)); calc_maior(R, M, V, ACC, T)).


/* na main ocorre a leitura dos dados de entrada. Após é gerado os dados dos arcos. Então remove o nó incial da lista e gera-se todas as permutações, após isso */
/* adiciona-se o nó inicial no incio e no fim, ordena os elementos e então calcula o maior */

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


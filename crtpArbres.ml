type ab_int =
			Vide
			| Noeud of ab_int*int*ab_int;;

let arbre_01 = Noeud(Noeud(Vide,7,Noeud(Vide,13,Vide)), 15 , Noeud(Vide,33,Vide));;

let arbre_02 = Noeud(Noeud(Noeud(Vide,13,Vide),7,Vide), 15 , Noeud(Vide,33,Vide));;

let rec somme (arb:ab_int) : int =
	match arb with
		| Vide -> 0
		| Noeud(Vide , n , Vide) -> n
		| Noeud(g , n , Vide) -> n + somme g
		| Noeud(Vide , n , d) -> n + somme d
		| Noeud(g, n , d) -> n + somme g + somme d
;;


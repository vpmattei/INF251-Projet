(*définition des types utilisés par la suite dans les profils de fonction à implanter *)

type cellule = char * int;; 

type abCode =
        Feuille of cellule                              (*Feuilles*)
     |  Noeud   of abCode*cellule*abCode ;;             (*Noeuds internes*)

type liste_code = abCode list;;
type liste_binaire = bool list;; 

(* fonctions permettant d'implanter les entrées sorties avec les fichiers. 
 *
 * Dans la suite du programme vous utiliserez 
 * 
 * 1) lecture_fichier qui est une fonction qui rend la liste des caractères 
 * contenus dans le fichier dont le nom est donné en paramètre : 
   Par exemple l'expression : lecture_fichier "nomfic" 
   
   2) ecriture_fichier qui prend une liste de caractères et un nom de fichier 
   et qui écrit la liste de caractères dans le fichier ainsi nommé. 
   Par exemple l'expression : ecriture_fichier "nomfic" liste écrit la liste 
   'liste' dans "nomfic".  *)

let rec lire_fichier (ic:in_channel) : char list = 
      try
         let car=(input_char ic) in car::(lire_fichier ic) 
      with End_of_file -> close_in;[]  ;;

let lecture_fichier (input_fichier : string) : char list = 
     let ic = (open_in input_fichier)
     in lire_fichier ic;;

let rec ecrire_fichier (l : char list) (oc:out_channel) : unit = match l with
     [] ->  flush oc ; close_out oc 
    |  a::l -> begin 
               (output_char oc) a; 
               ecrire_fichier l oc
             end;;

let ecriture_fichier (output_fichier:string) (phrase : char list) : unit =
      let oc=(open_out output_fichier) in ecrire_fichier  phrase oc;;


(* Pour l'écriture et la relecture de l'arbre de codage, on pourra utiliser les 
 * fonctions suivantes : 
 *  1) (put_codage a "nomfic") = écrit l'arbre de codage a dans le fichier "nomfic"
 *  2) (get_codage "nomfich)= l'arbre de codage écrit dans "nomfic"
 *  *)


let put_codage (a:abCode) (s: string) : unit =
   let oo=open_out s in 
        begin 
           (output_value oo) a;
           flush oo
        end;;

let get_codage (s : string) :  abCode =
    let oi=open_in s in 
         let result = (input_value oi:abCode) in 
         begin 
           close_in oi; 
           result
         end;;
          

(* Profil de fonctions importantes du projet *)


let rec statistique (l: char list) : abCode list =
        (*(statistique l) = produit la liste des arbres qui sont des feuilles 
         * et qui contiennent les couples formés des lettres apparaissant dans l avec leur 
         * fréquence. 
         *
         * Par exemple 
         *  statistique ['x';'y';'x';'z';'x';'y'] =
                 *  [(Feuille('x'),3);(Feuille('y'),2);(Feuille('z'),1)]*)
	match l with
		| [] -> []
		| e::s -> Feuille (e,)

let rec	compteur (l : char list) (c:char) : int =
	match l with
		| [] -> 0
		| e::s -> if e=c then 1 + compteur s c else compteur s c
;;
	
let rec huffman (lc : abCode list) : abCode = 
        (*(huffman l)=l'arbre de codage qui correspond à la liste de 
         * couples (caractère,fréquence) vu comme des feuilles 
         * ordonnée par ordre croissant.*) ;;

let rec compression (ab:abCode) (lc: char list)=
        (*compression ab l= la liste l compressée en utilisant l'arbre de codage ab.*);;

let lecture_liste_compressee (n : string) : char list = 
        (* lecture_liste_compressee "nomfic" va récupérer la liste des caractères 
         *  contenue dans nomfic puis la transformée en la liste des caractères qui 
         *  correspondaient à la compression des caractères du fichier d'origine. 
         *  *);;

let rec decompression (ab:abCode) (ld : char list)=
        (*décompression ab ld = décompresse la liste de caractère ld en utilisant l'arbre ab *);;
 
(* Front end du projet. Il y a une fonction de compression qui prend 
 * le nom d'un fichier "nomfic" et engengre deux fichier "nomfic.huf"
 * et "nomfic.cod". "nomfic.huf" contient le fichier d'origine compressé
 * suivant la méthode d'Huffman. Le fichier nomfic.cod contient l'arbre 
 * de codage. 
 * La fonction de décompression prend le nom du fichier (sans l'extension 
 * .huf ou .cod), par exemple "nomfic" elle va chercher dans "nomfic.huf" et 
 * "nomfic.cod" les caractères du fichier compressé d'une part et l'ardre de 
 * codage d'autre part. *)

let compresse (s: string)  : unit = 
     let huf=s^".huf" in 
     let cod=s^".cod" in 
     let liste = lecture_fichier s in 
     let ab = huffman (statistique liste) in 
         begin
          ecriture_fichier huf (compression ab l);
          put_codage ab cod
         end;;

let decompresse (s: string) : unit = 
     let huf=s^".huf" in 
     let cod=s^".cod" in 
     let decode = s^".decod" in 
     let ab=get_codage (cod) in 
     let l=lecture_liste_compressee huf in 
               ecriture_fichier decode (decompression ab l);;
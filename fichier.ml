(* Quelques fonctions de test pour le traitement des fichiers sous
   ocaml *)


(* pour ouvrir en lecture un fichier il faut utiliser la commande 
 * open_in  qui prend comme argument un string et rend un �l�ment du 
 * type inchannel. Typiquement on �crit la commande suivante
 *  il faut que le fichier existe sinon une exception est lev�e 
 * pour r�cup�rer la liste des caract�res d'un fichier on utilisera 
 * la fonction lecture fichier *)

let rec lire_fichier (ic:in_channel) : char list = 
      try
         let car=(input_char ic) in car::(lire_fichier ic) 
      with End_of_file -> close_in;[]  ;;

let lecture_fichier (input_fichier : string) : char list = 
     let ic = (open_in input_fichier)
     in lire_fichier ic;;

(* vous pouvez tester cette fonction sur le fichier A.txt*)

let lettres_de_A = (lecture_fichier "A.txt");;

(* pour �crire dans un fichier c'est la m�me chose mais en utilisant la 
 * commande open_out. Si l'arguement donn� a open_out est le nom d'un 
 * fichier qui existe d�j� alors le fichier existant sera �cras� 
 * l'�criture dans un fichier d'une liste de caract�res se fera par la 
 * fonction ecriture_fichier. On notera 
 *  qu'une fois l'�criture faite le canal d'�criture dans le fichier 
 *  est ferm� *)

let o_c = open_out "testons";;


let phrase_test=['c';'e';'c';'i';' ';'e';'s';'t';' ';
            'u';'n';' ';'f';'i';'c';'h';'i';'e';'r'];;


let rec ecrire_fichier (l : char list) (oc:out_channel) : unit = match l with
     [] ->  flush oc ; close_out oc 
    |  a::l -> begin 
               (output_char oc) a; 
               ecrire_fichier l oc
             end;;

let ecriture_fichier (output_fichier:string) (phrase : char list) : unit =
      let oc=(open_out output_fichier) in ecrire_fichier  phrase oc;;


(* exemple d'�criture de la phrase dans le fichier ouvert en �criture *)

ecriture_fichier "testons" phrase_test;;

(*ouvrez le fichier "testons" pour voir ce qui est �crit dedans*) 

(* Exemple d'ouverture et d'�criture dans un fichier : nous donnons l'implantation
 * d'une fonction qui lit l'int�gralit� des caract�res d'un fichier "f" puis 
 * les �crit dans un fichier "f.cod" en ajoutant "fin" � la fin du fichier *)

let ajoute_fin (fichier_entree : string) : unit = 
    let liste_entree= (lecture_fichier fichier_entree) in
    let liste_sortie= liste_entree@['f';'i';'n'] in 
    let nom_fichier_sortie = fichier_entree^".cod" in 
        ecriture_fichier nom_fichier_sortie liste_sortie;;

(* On peut tester cette fonction sur le fichier "testons" *)

ajoute_fin "testons";;
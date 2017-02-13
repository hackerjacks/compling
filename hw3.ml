(* 1. *)
type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Muliply of expr * expr
  | Divide of expr * expr
  | Raise of expr * expr

let rec (^.) b n =
  match n with
  | 0 -> 1
  | _ -> b * (b ^. (n-1))

let rec evaluate e =
  match e with
  | Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Muliply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
  | Raise (e, e') -> evaluate e ^. evaluate e'

  (* I attest that I did this problem in conformity with the Code of Academic
  Integrity *)

(* 2. *)
type trie = Trie of (bool * arcs)
and arcs = (char * trie) list

let emptytrie = Trie (false,[])

let rec enter w t = match (w,t) with
     [],Trie(_,l) -> Trie(true,l)
| k::ks,Trie(b,l) ->
    let subtrie =
      try
        List.assoc k l (* Q. do we already have an arc labeled 'k' ? *)
      with Not_found -> emptytrie (* A. No, make up some default new one *)
    in
    let tprime = enter ks subtrie in
      Trie(b,(k,tprime)::(List.remove_assoc k l))

(* borrowed from
  http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let rec explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec count_tail = function
  | [] -> 0
  | (c, t)::tl -> 1 + count t + count_tail tl
and
count = function
  Trie(b,l) -> match l with
              | [] -> 0
              | (c, t)::tl -> 1 + count t + count_tail tl

(* The set {unalloyed, unimpressed, unafraid} would take 23 nodes to represent
  as a trie according to my count function. The set {skittish, clownish, reddish}
  would also take 23 nodes to represent. In this case, 23 is the sum of the
  letters in each word (8 + 8 + 7). It takes less proprtionate memory cost to
  represent the first set because of the common prefixes shared by its members. *)

(* 3. *)

type tree = Branch of (string * (tree list)) | Leaf of string
type address = int list

exception Tree_out_of_bounds

let rec at (tre:tree) (adr:address) =
  let str = match tre with Branch (s, t_l) -> s | Leaf s -> s in
  let tr_lst = match tre with Branch (s, t_l) -> t_l | Leaf s -> [] in
  match adr with
  | [] -> tre
  | hd::tl -> try
                match (List.nth tr_lst hd) with
                | Branch n -> at (Branch n) tl
                | Leaf s -> Leaf s
              with
              | _ -> raise Tree_out_of_bounds

(*      tree1:   S
                / \
               /   \
            Name   VP
             /     /\
            /     /  \
          John   V   PP
                /    / \
               /    /   \
            went   to   NP
                        /\
                       /  \
                      D    N
                     /      \
                    /        \
                  the      cinema

          at tree1 [1;1;1;1;1] = cinema

*)


let rec add (elt:tree) (tre:tree) (adr:address) =
  failwith "unimplemented"

let qform_transform tre =
  let john = [0;0] in
  let was = [1;0;0] in
  let inter1 = at tre john in
  let temp1 =
  match inter1 with
  | Leaf s -> Leaf "Was" in
  let inter2 = at tre was in
  let temp2 =
  match inter2 with
  | Leaf s -> Leaf "John" in
  add inter1 tre john;
  add inter2 tre was;


(* This function relies on the assumption (which holds in English) that
swapping the Noun and Verb of a sentence that is made up of a name and a verb
phrase changes the sentence from a declaration to a question. *)
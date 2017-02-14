type remainder = string list


(* now, upgrade the recognizer to a parser by changing the type*)
type 'a combinatorparser = remainder -> ('a * remainder) list

type tree = Branch of (string * (tree list)) | Leaf of string
let unary label child = Branch (label,[child])
let binary label (child1,child2) = Branch (label,[child1;child2])
let ternary label ((child1,child2),child3) = Branch (label,[child1;child2;child3])
let fourary label (((child1,child2),child3),child4) = Branch (label,[child1;child2;child3;child4])
let multiple label kids = Branch (label,kids)

(* 1. succeeds if next word is myword. the value returned is the leaf node of a tree *)
let terminal myword = function
  x::xs when x=myword -> [((Leaf myword),xs)]
  | _ -> ([] : (tree * remainder) list)

(* 2. alternative. both disjuncts should return the same type of result *)
let ( |. ) (p : 'a combinatorparser) (q : 'a combinatorparser) xs = List.append (p xs) (q xs)

(* 3. sequencing defined with a list comprehension: *)
(* feed the output of p into q. pair up the results *)
let ( &. ) (p : 'a combinatorparser) (q : 'b combinatorparser) xs =
  [? List:((r1,r2),zs) | (r1,ys) <- List:(p xs); (r2,zs) <- List:(q ys) ?]

(* 4. "gives". apply f to all the results of combinatorparser p *)
let ( >. ) (p : 'a combinatorparser) f xs =
  [? List:(f x,ys) | (x,ys) <- List:(p xs) ?]


let rec s words = (np &. vp2 >. binary "S") words
and s' words = (np &. vp3 >. binary "S") words
and np words = (d &. n >. binary "NP" ) words
and vp2 words = ((v &. np) |. (v &. s) |. (v &. s') >. binary "VP") words
and vp3 words = (v &. np &. pp >. ternary "VP") words
and pp words = (p &. np >. binary "PP") words
and d = (terminal "the" |. terminal "a") >. unary "D"
and n = (terminal "dog" |. terminal "cat" |. terminal "garden" |.
  terminal "gardener" |. terminal "girl" |. terminal "butler") >. unary "N"
and v = (terminal "chased" |. terminal "saw" |. terminal "claimed" |.
  terminal "thought" |. terminal "said") >. unary "V"
and p = (terminal "into") >. unary "P"


(*
  3.1.3.1.
  Since the grammar contains recursive structures, the longest grammatically
  correct sentence that can be generated is infinitely long. "The butler claimed
  the girl claimed the butler claimed the girl claimed..."

  3.1.3.2
  The dog said the cat into the cat.
                          S
                         / \
                        /   \
                       /     \
                      NP     VP
                     /\      /|\
                    /  \    / | \
                   /    \  /  |  \
                  D     N V   NP  \
                  |     | |  /\    \
                 The   dog| /  \   PP
                          |D    N  |\
                          ||    |  | \
                          |the cat |  \
                          |        P  NP
                        said       |   |\
                                   |   | \
                                  into |  \
                                       D   N
                                       |   |
                                      the cat

  A garden thought a garden.
                          S
                         / \
                        /   \
                       /     \
                      NP     VP
                     /\      /\
                    /  \    /  \
                   /    \  /    \
                  D     N V     NP
                  |     | |     /\
                 A  garden|    /  \
                          |   D    N
                          |   |    |
                          |   a  garden
                          |
                        thought




 s ["the";"butler";"said";"the";"girl";"thought";"the";"dog";"chased";"the";"cat"]

- : (tree * remainder) list =                                                                            [(Branch                                                                                                    ("S",
    [Branch ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "butler"])]);
     Branch
      ("VP",
       [Branch ("V", [Leaf "said"]);
        Branch
         ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "girl"])])])]),
  ["thought"; "the"; "dog"; "chased"; "the"; "cat"]);
 (Branch
   ("S",
    [Branch ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "butler"])]);
     Branch
      ("VP",
       [Branch ("V", [Leaf "said"]);
        Branch
         ("S",
          [Branch
            ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "girl"])]);
           Branch
            ("VP",
             [Branch ("V", [Leaf "thought"]);
              Branch
               ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "dog"])])])])])]),
  ["chased"; "the"; "cat"]);
 (Branch
   ("S",
    [Branch ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "butler"])]);
     Branch
      ("VP",
       [Branch ("V", [Leaf "said"]);
        Branch
         ("S",
          [Branch
            ("NP", [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "girl"])]);
           Branch
            ("VP",
             [Branch ("V", [Leaf "thought"]);
              Branch
               ("S",
                [Branch
                  ("NP",
                   [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "dog"])]);
                 Branch
                  ("VP",
                   [Branch ("V", [Leaf "chased"]);
                    Branch
                     ("NP",
                      [Branch ("D", [Leaf "the"]); Branch ("N", [Leaf "cat"])])])])])])])]),
*)


(*
  Partial grammar of Spanish

  S         -> NPintrans VP
  NPintrans -> D Nmale
  NPintrans -> D Nfemale
  VP        -> Vtrans NPtrans
  VP        -> Vintrans PP
  NPtrans   -> Pmale Nmale
  NPtrans   -> Pfemale D Nfemale
  PP        -> P Pmale Nmale
  PP        -> P Pfemale D Nfemale
  D         -> el, la
  Nmale     -> chico, parque
  Nfemale   -> chica, biblioteca
  Vtrans    -> beso
  Vintrans  -> llego
  Pmale     -> al
  Pfemale   -> a
  P         -> cerca


  The basic idea that a sentence is composed of a noun phrase and verb phrase
  can be preserved from the English grammar. Also, verb phrases can contain
  verbs and noun phrases or prepositional phrases - just as in English. The way
  transitive and intransitive verb phrases are handled differs, though, because
  of the gendered nature of the language. This key difference also adds
  complexity to the handling of the prepositional phrase.
*)


let rec s_s words = (npintrans_s &. vp_s >. binary "S") words
and npintrans_s words = ((d_s &. nmale_s) |. (d_s &. nfemale_s) >. binary "NPintrans" ) words
and vp_s words = ((vtrans_s &. nptrans_s2) |. (vtrans_s &. nptrans_s3)
  |. (vintrans_s &. pp_s3) |. (vintrans_s &. pp_s4) >. binary "VP") words
and nptrans_s2 words = (pmale_s &. nmale_s >. binary "NPtrans") words
and nptrans_s3 words = (pfemale_s &. d_s &. nfemale_s >. ternary "NPtrans") words
and pp_s3 words = (p_s &. pmale_s &. nmale_s >. ternary "PP") words
and pp_s4 words = (p_s &. pfemale_s &. d_s &. nfemale_s >. fourary "PP") words
and d_s = (terminal "el" |. terminal "la") >. unary "D"
and nmale_s = (terminal "chico" |. terminal "parque") >. unary "N"
and nfemale_s = (terminal "chica" |. terminal "biblioteca") >. unary "N"
and vtrans_s = (terminal "beso") >. unary "V"
and vintrans_s = (terminal "llego") >. unary "V"
and pmale_s = (terminal "al") >. unary "P"
and pfemale_s = (terminal "a") >. unary "P"
and p_s = (terminal "cerca") >. unary "P"
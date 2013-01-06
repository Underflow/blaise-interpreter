(* ast.mli *)

(* Les expressions *)
type expr =
  (* valeur entiere directe *)
  | Int of int
  (* variable *)
  | Var of string
  (* Operateur binaire *)
  | BinOp of expr * string * expr
  (* Operateur unaire *)
  | UniOp of string * expr
(* Appel de fonction *)
  | Call of string * expr list

(* Les instructions *)
type statement =
  (* Affectation d'une variable *)
  | Assign of string * expr
  (* Une expression seule *)
  | Expr of expr
  (* if-then-else *)
  | If of expr * statement list * statement list
  (* boucle while *)
  | While of expr * statement list
  (* return *)
  | Return of expr

(* Declaration de fonctions *)
type funcdecl =
    {
      (* Le nom de la fonction *)
      fname : string;
      (* Le corps de la fonction *)
      fbody : statement list;
      (* La liste des parametres *)
      fparams : string list;
      (* La liste des variables *)
      fvars : string list;
    }

(* Le point d'entree du programme *)
type entry =
    {
      (* Les variables du bloc principal *)
      mainvars : string list;
      (* le corps principal *)
      mainbody : statement list;
    }

(* Le programme *)
type prg =
    {
      (* Les fonctions *)
      func : funcdecl list;
      (* Le point d'entree *)
      main : entry;
    }

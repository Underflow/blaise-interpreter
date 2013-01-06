(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

  let h = Hashtbl.create 29
  let _ =
    List.iter (fun (k,t) -> Hashtbl.add h k t)
      [
        ("+",ADD); ("*",MUL);
        ("-",MIN); ("/",DIV);
        ("<",LT); (">",GT);
        ("<=",LEQ); (">=",GEQ);
        ("==",CMPEQ);(",",COM);
        ("!",NOT);("&&",AND);("||",OR);
        ("{",LB); ("}",RB);
        ("=",EQ); (";",SC);
        ("(",LPAREN); (")",RPAREN);
      ]

  let keys = Hashtbl.create 13
  let _ =
    List.iter (fun (k,t) -> Hashtbl.add keys k t)
      [
        ("main",MAIN);
        ("if",IF);
        ("while",WHILE);
        ("else",ELSE);
        ("return",RETURN);
        ("vars",VARS);
      ]

}
let ident = ['a'-'z']['a'-'z' '_' '0'-'9']*

let white = [' ' '\t' '\n']

let num = '-'?['0'-'9']['0'-'9']*

let op = "+" | "*" | "-" | "/" | "<" | ">" | ">=" | "<=" | "=="
  | "," | "{" | "}" | "=" | ";" | "(" | ")" | "!" | "||" | "&&"

rule token = parse
    white+         { token lexbuf }     (* skip blanks *)
  | num  as n      { INT (int_of_string n) }
  | ident as ato   {
    try Hashtbl.find keys ato with
      | Not_found -> VAR ato
  }
  | op             { Hashtbl.find h (Lexing.lexeme lexbuf)}
  | eof            { EOF }

open Ast

let rec format_args args f =
    match args with
          e::[] -> f e
        | e::l -> f e ^ "," ^ format_args l f
        | [] -> assert false

let rec format_expr expr =
    match expr with
          Int i -> string_of_int i
        | Var var -> var
        | BinOp (e1, op, e2) -> format_expr e1 ^ " " ^ op ^ " " ^ format_expr e2
        | UniOp (op, e) -> op ^ " " ^ format_expr e
        | Call (f, a) -> f ^ "(" ^ (format_args a format_expr) ^ ")"

let rec print_statement statement =
    match statement with
          Assign (var, e) -> Printf.printf"%s = %s;\n" var (format_expr e)
        | Expr e -> Printf.printf "%s;\n" (format_expr e)
        | If (e, t, f) -> (
            Printf.printf "if (%s)\n{\n" (format_expr e);
            List.iter print_statement t;
            Printf.printf "}\nelse\n{\n";
            List.iter print_statement f;
            Printf.printf"}\n";
        )
        | While (e, s) -> (
            Printf.printf "while (%s)\n{\n" (format_expr e);
            List.iter print_statement s;
            Printf.printf "}\n";
        )
        | Return e -> Printf.printf "return %s;\n" (format_expr e)

let id x = x

let print_fun func =
    begin
        Printf.printf "%s(%s)\n{\n" func.fname (format_args func.fparams
        id);
        if (func.fvars != []) then
            Printf.printf "vars %s;\n" (format_args func.fvars id);
        List.iter print_statement func.fbody;
        Printf.printf "}\n";
    end

let print_main main =
    begin
        Printf.printf "main()\n{\n";
        if (main.mainvars != []) then
            Printf.printf "vars %s;\n" (format_args main.mainvars id);
        List.iter print_statement main.mainbody;
        Printf.printf "}\n";
    end

let prettyprint ast =
    begin
        List.iter print_fun ast.func;
        print_main ast.main;
    end;

open Ast

let rec std_write v =
    Printf.printf "%d\n" v

let rec format_args args f =
    match args with
          e::[] -> f e
        | e::l -> f e ^ "," ^ format_args l f
        | [] -> assert false

let iob b =
    if b then 1 else 0

let boi i =
    if i = 0 then false else true

let eval_binop e1 op e2 =
    match op with
          "+" -> e1 + e2
        | "-" -> e1 - e2
        | "*" -> e1 * e2
        | "/" -> e1 / e2
        | ">" -> iob (e1 > e2)
        | "<" -> iob (e1 < e2)
        | ">=" -> iob (e1 >= e2)
        | "<=" -> iob (e1 <= e2)
        | "==" -> iob (e1 == e2)
        | "||" -> iob (boi e1 || boi e2)
        | "&&" -> iob (boi e1 && boi e2)
        |  _   -> failwith "This operator doesn't exist"

let eval_uniop op e =
    match op with
          "-" -> -e
        | "!" -> iob (not (boi e))
        | _ -> failwith "This operator doesn't exist"

let call_function env fct name args =
    match name with
          "write" -> (std_write (List.hd args); 1)
        |  name   -> let func = (Hashtbl.find fct name) in
        func args


let rec eval_expr env fct expr =
    match expr with
          Int i ->i
        | Var var -> Hashtbl.find env var
        | BinOp (e1, op, e2) ->
                eval_binop (eval_expr env fct e1) op (eval_expr env fct e2)
        | UniOp (op, e) ->eval_uniop op (eval_expr env fct e)
        | Call (f, a) -> call_function env fct f (List.map (eval_expr env fct) a)


let rec eval_statement env fct statement =
    match statement with
          Assign (var, e) -> Hashtbl.replace env var (eval_expr env fct e)
        | Expr e -> let _ = eval_expr env fct e in ()
        | If (e, t, f) ->
                if (eval_expr env fct e) <> 0 then 
                    List.iter (eval_statement env fct) t
    else
        List.iter (eval_statement env fct) f
        | While (e, s) ->
                while (eval_expr env fct e) <> 0 do
                    List.iter (eval_statement env fct) s
                done;
        | Return e -> failwith "Return non implemented"

let id x = x

let rec eval_fun_statements env fct sl =
    match sl with
          (Return e)::l -> eval_expr env fct e;
        | (Assign (var, e))::l -> Hashtbl.replace env var (eval_expr env fct e);
        eval_fun_statements env fct l
        | (Expr e)::l -> let _ = eval_expr env fct e in (); eval_fun_statements
        env fct l
        | (If (e, t, f))::l ->
                if (eval_expr env fct e) <> 0 then 
                    eval_fun_statements env fct (t@l)
                else
                    eval_fun_statements env fct (f@l)
        | (While (e, s))::l ->
                let ret = ref min_int in
                while (!ret == min_int && (eval_expr env fct e) <> 0) do
                    ret := eval_fun_statements env fct s
                done;
                if(!ret != min_int) then
                    !ret
                else
                    eval_fun_statements env fct l
        | [] -> min_int

let eval_fun fd fct args =
    let env = Hashtbl.create (List.length fd.fparams + List.length fd.fvars) in
    List.iter2 (Hashtbl.replace env) fd.fparams args;
    eval_fun_statements env fct fd.fbody


let make_funcalltable fdl =
    let add_fun fct fd =
        Hashtbl.replace fct fd.fname (eval_fun fd fct) in
    let fct = Hashtbl.create (List.length fdl) in
    let _ = List.iter (add_fun fct) fdl in
    fct

let eval_main fct main =
    begin
        let env = Hashtbl.create (List.length main.mainvars) in
        List.iter (eval_statement env fct) main.mainbody;
    end

let eval ast =
    begin
        let fct = make_funcalltable ast.func in
        eval_main fct ast.main;
    end;

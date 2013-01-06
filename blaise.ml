let main () =
  begin
    if (Sys.argv.(1) = "-eval") then
        let cin = open_in Sys.argv.(2) in
        let ast = Parser.prg Lexer.token (Lexing.from_channel cin) in
        Eval.eval ast;
        exit 0;
    else
        let cin = open_in Sys.argv.(1) in
        let ast = Parser.prg Lexer.token (Lexing.from_channel cin) in
        Prettyprinter.prettyprint ast;
        exit 0;
  end
 
let _ = main ()

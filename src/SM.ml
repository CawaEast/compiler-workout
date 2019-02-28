open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)

let exec i config = match i, config with
  | BINOP op, (y::x::etc, cfg)                  -> (Syntax.Expr.strToFunc op x y)::etc, cfg
  | CONST n,  (stack,     cfg)                  -> n::stack, cfg
  | READ,     (stack,     (state, a::inp, out)) -> a::stack, (state, inp, out)
  | WRITE,    (a::etc,    (state, inp,    out)) -> etc,      (state, inp, out @ [a])
  | LD x,     (stack,     (state, inp,    out)) -> (state x)::stack, (state, inp, out)
  | ST x,     (a::etc,    (state, inp,    out)) -> etc, (Syntax.Expr.update x a state, inp, out)
  | _ -> failwith "Found unknown instruction on interpreting"

let rec eval cfg prog = match prog with
  | a::etc -> eval (exec a cfg) etc
  | []     -> cfg

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)


let rec compileExpr expr = match expr with
  | Syntax.Expr.Const a -> [CONST a]
  | Syntax.Expr.Var x -> [LD x]
  | Syntax.Expr.Binop (op, x, y) -> (compileExpr x) @ (compileExpr y) @ [BINOP op]


let rec compile st = match st with
  | Syntax.Stmt.Read x -> [READ; ST x]
  | Syntax.Stmt.Write e -> (compileExpr e) @ [WRITE]
  | Syntax.Stmt.Assign (x, e) -> (compileExpr e) @ [ST x]
  | Syntax.Stmt.Seq (st1, st2) -> (compile st1) @ (compile st2)

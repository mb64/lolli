functor FolLexFun(structure Tokens: Fol_TOKENS
			   structure Interface: INTERFACE) : LEXER=
   struct
    structure UserDeclarations =
      struct

structure Tokens = Tokens
structure Interface = Interface
open Interface

type pos = Interface.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val eof = fn () => Tokens.EOF(!line,!line)

fun makeInt (s : string) = 
  let val ord0 = ord #"0"
      fun rec_mi nil = 0
        | rec_mi (h::t) = ((ord h) - ord0) + (10 * (rec_mi t))
  in rec_mi (rev (explode s)) end

fun strip_quotes (s:string) =
      let val ( _ :: tail) = (explode s) 
        in let val ( _ :: middle) = (rev tail)
             in (implode (rev middle)) end end 

fun strip_carats (s:string) =
  let fun rec_sc nil = nil
	| rec_sc (#"^"::quoted_char::tail) = (quoted_char::(rec_sc tail))
	| rec_sc (head::tail) = (head::(rec_sc tail))
  in (implode (rec_sc (explode s))) end

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\005\005\005\005\005\005\005\005\005\093\095\005\093\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\093\092\089\005\005\088\087\082\081\080\071\071\079\073\072\071\
\\069\069\069\069\069\069\069\069\069\069\066\065\063\059\057\005\
\\005\034\034\034\034\034\034\034\034\034\034\034\047\041\034\034\
\\034\034\034\034\034\034\034\034\034\034\034\040\039\038\037\034\
\\005\009\009\009\009\024\018\009\009\016\009\009\009\009\009\009\
\\009\009\009\009\012\009\009\009\009\009\009\008\007\006\005\005\
\\005"
),
 (3, 
"\096\096\096\096\096\096\096\096\096\096\097\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (11, 
"\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\013\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\014\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\015\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\017\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\019\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\020\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\021\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\022\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\023\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\030\010\010\010\010\010\025\010\010\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\026\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\027\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\028\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\029\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\031\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\032\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\033\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (36, 
"\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\042\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\043\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\044\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (44, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\045\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\046\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (47, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\052\035\035\035\035\035\048\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\049\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (49, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\050\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (50, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\051\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (52, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\053\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (53, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\054\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\055\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (55, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\000\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\056\035\035\035\035\035\035\035\035\000\000\000\036\035\
\\000\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\
\\035\035\035\035\035\035\035\035\035\035\035\000\000\000\000\000\
\\000"
),
 (57, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\058\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\062\000\058\000\061\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\060\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (63, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\064\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (66, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\068\000\000\
\\000\000\000\000\000\000\000\000\000\000\067\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (69, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\070\070\070\070\070\070\070\070\070\070\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\011\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (73, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\076\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\075\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\074\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (76, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\078\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\077\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (82, 
"\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\084\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\083\083\083\083\085\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083"
),
 (83, 
"\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\084\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083"
),
 (85, 
"\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\086\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\085\
\\085"
),
 (89, 
"\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\091\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090"
),
 (93, 
"\000\000\000\000\000\000\000\000\000\094\000\000\094\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\094\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [(N 1),(N 147)], trans = 1},
{fin = [(N 1),(N 147)], trans = 1},
{fin = [], trans = 3},
{fin = [], trans = 3},
{fin = [(N 158)], trans = 0},
{fin = [(N 53),(N 158)], trans = 0},
{fin = [(N 72),(N 158)], trans = 0},
{fin = [(N 51),(N 158)], trans = 0},
{fin = [(N 147),(N 158)], trans = 9},
{fin = [(N 147)], trans = 9},
{fin = [], trans = 11},
{fin = [(N 147),(N 158)], trans = 12},
{fin = [(N 147)], trans = 13},
{fin = [(N 147)], trans = 14},
{fin = [(N 85),(N 147)], trans = 9},
{fin = [(N 147),(N 158)], trans = 16},
{fin = [(N 115),(N 147)], trans = 9},
{fin = [(N 147),(N 158)], trans = 18},
{fin = [(N 147)], trans = 19},
{fin = [(N 147)], trans = 20},
{fin = [(N 147)], trans = 21},
{fin = [(N 147)], trans = 22},
{fin = [(N 92),(N 147)], trans = 9},
{fin = [(N 147),(N 158)], trans = 24},
{fin = [(N 147)], trans = 25},
{fin = [(N 147)], trans = 26},
{fin = [(N 147)], trans = 27},
{fin = [(N 147)], trans = 28},
{fin = [(N 99),(N 147)], trans = 9},
{fin = [(N 147)], trans = 30},
{fin = [(N 147)], trans = 31},
{fin = [(N 147)], trans = 32},
{fin = [(N 80),(N 147)], trans = 9},
{fin = [(N 139),(N 147),(N 158)], trans = 34},
{fin = [(N 139),(N 147)], trans = 34},
{fin = [], trans = 36},
{fin = [(N 158)], trans = 11},
{fin = [(N 57),(N 158)], trans = 0},
{fin = [(N 45),(N 158)], trans = 0},
{fin = [(N 55),(N 158)], trans = 0},
{fin = [(N 139),(N 147),(N 158)], trans = 41},
{fin = [(N 139),(N 147)], trans = 42},
{fin = [(N 139),(N 147)], trans = 43},
{fin = [(N 139),(N 147)], trans = 44},
{fin = [(N 139),(N 147)], trans = 45},
{fin = [(N 12),(N 139),(N 147)], trans = 34},
{fin = [(N 139),(N 147),(N 158)], trans = 47},
{fin = [(N 139),(N 147)], trans = 48},
{fin = [(N 139),(N 147)], trans = 49},
{fin = [(N 139),(N 147)], trans = 50},
{fin = [(N 25),(N 139),(N 147)], trans = 34},
{fin = [(N 139),(N 147)], trans = 52},
{fin = [(N 139),(N 147)], trans = 53},
{fin = [(N 139),(N 147)], trans = 54},
{fin = [(N 139),(N 147)], trans = 55},
{fin = [(N 19),(N 139),(N 147)], trans = 34},
{fin = [(N 115),(N 158)], trans = 57},
{fin = [(N 115)], trans = 0},
{fin = [(N 115),(N 158)], trans = 59},
{fin = [], trans = 57},
{fin = [(N 70)], trans = 0},
{fin = [], trans = 57},
{fin = [(N 115),(N 158)], trans = 63},
{fin = [(N 28)], trans = 0},
{fin = [(N 39),(N 158)], trans = 0},
{fin = [(N 158)], trans = 66},
{fin = [(N 123)], trans = 0},
{fin = [(N 31)], trans = 0},
{fin = [(N 126),(N 147),(N 158)], trans = 69},
{fin = [(N 126),(N 147)], trans = 69},
{fin = [(N 120),(N 158)], trans = 0},
{fin = [(N 43),(N 158)], trans = 0},
{fin = [(N 120),(N 158)], trans = 73},
{fin = [(N 67)], trans = 0},
{fin = [(N 60)], trans = 0},
{fin = [], trans = 76},
{fin = [(N 64)], trans = 0},
{fin = [(N 35)], trans = 0},
{fin = [(N 37),(N 158)], trans = 0},
{fin = [(N 49),(N 158)], trans = 0},
{fin = [(N 47),(N 158)], trans = 0},
{fin = [(N 158)], trans = 82},
{fin = [], trans = 83},
{fin = [(N 156)], trans = 0},
{fin = [], trans = 85},
{fin = [(N 152),(N 156)], trans = 0},
{fin = [(N 41),(N 158)], trans = 0},
{fin = [(N 5),(N 158)], trans = 0},
{fin = [(N 158)], trans = 89},
{fin = [], trans = 89},
{fin = [(N 130)], trans = 0},
{fin = [(N 74),(N 158)], trans = 0},
{fin = [(N 1),(N 158)], trans = 93},
{fin = [(N 1)], trans = 93},
{fin = [(N 3)], trans = 0},
{fin = [(N 162)], trans = 0},
{fin = [(N 160)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val COMMENT = STARTSTATE 3;
val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput = 
let 
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref 1		(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let val yytext = substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lex())
| 115 => (Tokens.INFIXOP (yytext,!line,!line))
| 12 => (Tokens.MODULE(!line,!line))
| 120 => (Tokens.LEFTINFIXOP (yytext,!line,!line))
| 123 => (Tokens.RIGHTINFIXOP (yytext,!line,!line))
| 126 => (Tokens.INT (makeInt yytext,!line,!line))
| 130 => (Tokens.STRING (strip_quotes yytext,!line,!line))
| 139 => (Tokens.UCID (strip_carats yytext,!line,!line))
| 147 => (Tokens.LCID (strip_carats yytext,!line,!line))
| 152 => (Tokens.UCID (strip_quotes yytext,!line,!line))
| 156 => (Tokens.LCID (strip_quotes yytext,!line,!line))
| 158 => (error ("ignoring illegal character" ^ yytext,
			   !line,!line); lex())
| 160 => (next_line(); YYBEGIN INITIAL; lex())
| 162 => (lex())
| 19 => (Tokens.LINEAR(!line,!line))
| 25 => (Tokens.LOCAL(!line,!line))
| 28 => (Tokens.BACKDOUBLE(!line,!line))
| 3 => (next_line(); lex())
| 31 => (Tokens.BACKLOLLI(!line,!line))
| 35 => (Tokens.DCGARROW(!line,!line))
| 37 => (Tokens.COMMA(!line,!line))
| 39 => (Tokens.SEMICOLON(!line,!line))
| 41 => (Tokens.AMPERSAND(!line,!line))
| 43 => (Tokens.DOT(!line,!line))
| 45 => (Tokens.BACKSLASH(!line,!line))
| 47 => (Tokens.LPAREN(!line,!line))
| 49 => (Tokens.RPAREN(!line,!line))
| 5 => (YYBEGIN COMMENT; lex())
| 51 => (Tokens.LCURLY(!line,!line))
| 53 => (Tokens.RCURLY(!line,!line))
| 55 => (Tokens.LBRACKET(!line,!line))
| 57 => (Tokens.RBRACKET(!line,!line))
| 60 => (Tokens.ARROW(!line,!line))
| 64 => (Tokens.LOADLOLLI(!line,!line))
| 67 => (Tokens.LOLLIPOP(!line,!line))
| 70 => (Tokens.DOUBLEARROW(!line,!line))
| 72 => (Tokens.BAR(!line,!line))
| 74 => (Tokens.EXCL(!line,!line))
| 80 => (Tokens.TRUE(!line,!line))
| 85 => (Tokens.ONE(!line,!line))
| 92 => (Tokens.FORALL(!line,!line))
| 99 => (Tokens.EXISTS(!line,!line))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(String.sub(!yyb,l))
		val NewState = if NewChar<128 then Char.ord(String.sub(trans,NewChar)) else Char.ord(String.sub(trans,128))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
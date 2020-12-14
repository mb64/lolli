    (******************************************************************)
    (*                                                                *)
    (*  FILE: interpreter.sml                                         *)
    (*        The core of the system, this covers the logic and the   *)
    (*        built-ins.                                              *)
    (*                                                                *)
    (*  AUTHOR: Joshua S. Hodas (hodas@saul.cis.upenn.edu)            *)
    (*  DATE: 10/19/92                                                *)
    (*                                                                *)
    (*  Portions of this and the other files in the system are based  *)
    (*  code originally developed by Frank Pfenning and Conal Elliott *)
    (*                                                                *)
    (*  Modified for improved resource management                     *)
    (*  Iliano Cervesato and Frank Pfenning                           *)
    (*  3/24/95                                                       *)
    (******************************************************************)

(* Version of INTERPRETER for first-order linear logic fragment 
   with meta-variables *)

signature INTERPRETER =
  sig
    structure Terms_To_Props : TERMS_TO_PROPS
    val solve : Terms_To_Props.Props.gform 
		 -> Terms_To_Props.Props.prog
		 -> Terms_To_Props.Props.Terms.term list
	         -> Terms_To_Props.Props.Terms.substitution 
		 -> (Terms_To_Props.Props.Terms.substitution 
		      -> Terms_To_Props.Props.prog -> bool -> unit)
		 -> unit
    val ll_file : string -> unit
    val ll : unit -> unit

    structure Parse : PARSE
end

functor Interpreter (structure Terms_To_Props : TERMS_TO_PROPS
		     structure Parse : PARSE
	               sharing Terms_To_Props.Props.Terms = 
                               Parse.Absyn.Terms) : INTERPRETER =
struct

structure Terms_To_Props = Terms_To_Props
structure Parse = Parse

structure Props = Terms_To_Props.Props
structure Terms = Terms_To_Props.Props.Terms


open Terms_To_Props
open Props
open Terms

exception Illegal of string
exception Flexible of term
exception POP
exception POPALL
exception POPAUX
exception EXIT
exception ABORT
exception UnexpectedReturn;

val ll_version = "0.94, Mar 18, 1995 -ic,fp";

val current_out_stream = ref TextIO.stdOut
val current_in_stream = ref TextIO.stdIn
val all_solutions = ref false


val init_prog = (Prog ((0,0),Done))

fun built_in "write"  		= true
  | built_in "write_raw" 	= true
  | built_in "write_clause" 	= true
  | built_in "write_sans" 	= true
  | built_in "nl"     		= true
  | built_in "read"   		= true
  | built_in "telling" 		= true
  | built_in "seeing" 		= true
  | built_in "timing" 		= true
  | built_in "load"   		= true
  | built_in "cd"    		= true
  | built_in "system"  		= true
  | built_in "top"    		= true
  | built_in "fail"   		= true
  | built_in "pop"    		= true
  | built_in "popall" 		= true
  | built_in "abort"  		= true
  | built_in "exit"   		= true
  | built_in "bye"    		= true
  | built_in "is"     		= true
  | built_in "="      		= true
  | built_in "=:="    		= true
  | built_in "=\\="   		= true
  | built_in "=<"     		= true
  | built_in ">="     		= true
  | built_in ">"      		= true
  | built_in "<"      		= true
  | built_in "explode" 		= true
  | built_in "explode_words"	= true
  | built_in "var"    		= true
  | built_in "nonvar" 		= true
  | built_in "generalize"	= true
  | built_in   _      		= false

fun one_like_builtin "write"  		= true
  | one_like_builtin "write_raw" 	= true
  | one_like_builtin "write_clause" 	= true
  | one_like_builtin "write_sans" 	= true
  | one_like_builtin "nl"     		= true
  | one_like_builtin "read"   		= true
  | one_like_builtin "cd"    		= true
  | one_like_builtin "system"  		= true
  | one_like_builtin "is"     		= true
  | one_like_builtin "="      		= true
  | one_like_builtin "=:="    		= true
  | one_like_builtin "=\\="   		= true
  | one_like_builtin "=<"     		= true
  | one_like_builtin ">="     		= true
  | one_like_builtin ">"      		= true
  | one_like_builtin "<"      		= true
  | one_like_builtin "explode" 		= true
  | one_like_builtin "explode_words"	= true
  | one_like_builtin "var"    		= true
  | one_like_builtin "nonvar" 		= true
  | one_like_builtin "generalize"	= true
  | one_like_builtin   _      		= false

fun func_symbol_of (Appl (f,_)) = func_symbol_of f
  | func_symbol_of (Const (Name funct)) = funct
  | func_symbol_of (v as Uvar _) = makestring_term v 
  | func_symbol_of (v as Evar _) = raise Flexible v

fun d_scan (Done) subst        = (Prog((0,0),Done))
  | d_scan (Dtensor (c1,c2)) subst = 
	let val (Prog((s1,t1),d1)) = d_scan (c1) subst (* t1 = 0 *)
	    val (Prog((s2,t2),d2)) = d_scan (c2) subst (* t2 = 0 *)
	in (Prog ((s1+s2,0), Dtensor (d1,d2)))
	end
  | d_scan (Dbang R) subst     = (Prog ((0,0), Dbang   (r_scan R subst)))
  | d_scan (Dstrict R) subst   = (Prog ((1,0), Dstrict (r_scan R subst)))
  | d_scan (Dflex M) subst =
	(case Terms_To_Props.term_to_dform subst M
             of Dflex(M1) => raise Illegal
		     (": Dformula with flexible head (" ^ 
			makestring_term M ^ ") cannot be assumed.")
	      | d => d_scan d subst)

and r_scan (Ratom M) subst = 
	if (built_in (func_symbol_of M))
	  then raise Illegal(": Cannot assume clause with special predicate ("^
				makestring_term M ^ ") as head.")
	  else (Ratom M)
  | r_scan (Rwith (R1,R2)) subst = 
			Rwith (r_scan R1 subst, r_scan R2 subst)
  | r_scan (Rlinimpl (G,R)) subst = Rlinimpl (G, r_scan R subst)
  | r_scan (Rall (V,R)) subst = Rall (V, r_scan R subst)
  | r_scan (Rflex M) subst =
	(case Terms_To_Props.term_to_rform subst M
             of Rflex(M1) => raise Illegal
		     (": Rformula with flexible head (" ^ 
			makestring_term M ^ ") cannot be assumed.")
	      | r => r_scan r subst)

fun load_module (f1,uvars) =
      let val modname = ((func_symbol_of f1) 
		handle Flexible _  =>
		   raise Illegal (": Unbound variable as module name in --o ("
				  ^ makestring_term f1 ^ ").")
		     | Illegal _ =>
		   raise Illegal (": non-name as module name in --o ("
				  ^ makestring_term f1 ^ ")."))
	  val (modname2,params,locals,clauses) = 
					Parse.file_parse (modname ^ ".ll")
	  val rawclauses = clauses_to_dform clauses
	  val newlocals = map Terms.Varbind locals
	  val newuvars = map new_uvar newlocals
	  val newparams = map Terms.Varbind params
	  val newevars = map (fn p => new_evar p uvars) newparams
	  fun dsublist nil nil d = d
	    | dsublist (t::ts) (x::xs) d = dsubst t x (dsublist ts xs d)
	  val newclauses = dsublist newuvars newlocals 
				   (dsublist newevars newparams rawclauses)
	  fun make_term (modname::nil) = modname
	    | make_term (h::t) = Appl(make_term t,h)
	  val modhead = make_term (rev ((Const (Name modname2))::newevars))
      in (modhead,newclauses,newuvars) end


(* strict D --- checks if all strict linear resources have been consumed *)
fun strict (Prog((0,_),_)) = true
  | strict (Prog((s,_),_)) = false

(* erase D --- erases all consumable resources in D *)
fun erase (Prog((0,0),d)) = (Prog((0,0),d))
  | erase (Prog((s,t),d)) =
    let fun rec_erase (Done)           = Done
	  | rec_erase (Dtensor(d1,d2)) = Dtensor(rec_erase(d1),rec_erase(d2))
	  | rec_erase (Dbang(r))       = Dbang(r)
	  | rec_erase (Dstrict(r))     = Dconsumed(r)
	  | rec_erase (Dresource(r))   = Dconsumed(r)
	  | rec_erase (Dconsumed(r))   = Dconsumed(r)
    in (Prog((0,0),rec_erase(d)))
    end

(* erase_strict D --- erases all strict resources in D *)
fun erase_strict (Prog((0,t),d)) = (Prog((0,t),d))
  | erase_strict (Prog((s,t),d)) =
    let fun rec_es (Done)           = Done
	  | rec_es (Dtensor(d1,d2)) = Dtensor(rec_es(d1),rec_es(d2))
	  | rec_es (Dbang(r))       = Dbang(r)
	  | rec_es (Dstrict(r))     = Dconsumed(r)
	  | rec_es (Dresource(r))   = Dresource(r)
	  | rec_es (Dconsumed(r))   = Dconsumed(r)
    in (Prog((0,t),rec_es(d)))
    end

(* make_non_strict D --- makes every strict resource in D consumable *)
fun make_non_strict (Prog((0,t),d)) = (Prog((0,t),d))
  | make_non_strict (Prog((s,t),d)) =
    let fun rec_mns (Done)           = Done
	  | rec_mns (Dtensor(d1,d2)) = Dtensor(rec_mns(d1),rec_mns(d2))
	  | rec_mns (Dbang(r))       = Dbang(r)
	  | rec_mns (Dstrict(r))     = Dresource(r)
	  | rec_mns (Dresource(r))   = Dresource(r)
	  | rec_mns (Dconsumed(r))   = Dconsumed(r)
    in (Prog((0,s+t),rec_mns(d)))
    end

(* intersect (D1,D2) --- intersects available resources in D1 and D2
   (there shouldn't be strict resources in D2) *)
fun intersect (Prog((0 ,t1),d1), Prog((0,t2),d2)) = Prog((0,t2),d2)
  | intersect (Prog((s1,t1),d1), Prog((0,0 ),d2)) = Prog((0,0 ),d2)
  | intersect (Prog(_,d1), Prog((0,_),d2)) = 
    let fun rec_i (Done, Done)                 = (Prog ((0,0), Done))
	  | rec_i (Dtensor(d1,d2), Dtensor(d1',d2')) =
		let val (Prog((s1,t1),d'))  = rec_i (d1,d1') (* s1 = 0 *)
		    val (Prog((s2,t2),d'')) = rec_i (d2,d2') (* s2 = 0 *)
		in (Prog((0,t1+t2),Dtensor(d',d'')))
		end
	  | rec_i (Dbang(r),Dbang(r'))         = (Prog((0,0), Dbang     (r)))
	  | rec_i (Dstrict(r),Dresource(r'))   = (Prog((0,0), Dconsumed (r)))
	  | rec_i (Dstrict(r),Dconsumed(r'))   = (Prog((0,0), Dconsumed (r)))
	  | rec_i (Dresource(r),Dresource(r')) = (Prog((0,1), Dresource (r)))
	  | rec_i (Dresource(r),Dconsumed(r')) = (Prog((0,0), Dconsumed (r)))
	  | rec_i (Dconsumed(r),Dconsumed(r')) = (Prog((0,0), Dconsumed (r)))
    in rec_i (d1,d2)
    end

(* intersect_input (D1,D2) --- intersects the resources in D1 and D2 and makes
   the result of the same type as the resources appear in D1 (there shouldn't
   be strict resources in D2) *)
fun intersect_input (Prog((0 ,t1),d1), Prog((0,t2),d2)) = Prog((0,t2),d2)
  | intersect_input (Prog((s1,t1),d1), Prog((0,0 ),d2)) = Prog((0,0 ),d2)
  | intersect_input (Prog(_,d1), Prog((0,_),d2)) = 
    let fun rec_ii (Done,Done)                  = (Prog((0,0), Done))
	  | rec_ii (Dtensor(d1,d2),Dtensor(d1',d2')) =
		let val (Prog((s1,t1),d'))  = rec_ii (d1,d1')
		    val (Prog((s2,t2),d'')) = rec_ii (d2,d2')
		in (Prog ((s1+s2,t1+t2),Dtensor(d',d'')))
		end
	  | rec_ii (Dbang(r),Dbang(r'))         = (Prog((0,0), Dbang(r)))
	  | rec_ii (Dstrict(r),Dresource(r'))   = (Prog((1,0), Dstrict(r)))
	  | rec_ii (Dstrict(r),Dconsumed(r'))   = (Prog((0,0), Dconsumed(r)))
	  | rec_ii (Dresource(r),Dresource(r')) = (Prog((0,1), Dresource(r)))
	  | rec_ii (Dresource(r),Dconsumed(r')) = (Prog((0,0), Dconsumed(r)))
	  | rec_ii (Dconsumed(r),Dconsumed(r')) = (Prog((0,0), Dconsumed(r)))
    in rec_ii (d1,d2)
    end

(* strict_difference_and_erase (D1,D2) --- changes every resource of D1 that
   is not a resource in D2 into a strict resource, restores the strict
   resources of D1, erases the remaining resources *)
fun strict_difference_and_erase (Prog((s1,0),d1),Prog((0,0),d2)) =
	Prog((s1,0),d1)
  | strict_difference_and_erase (Prog((s1,t1),d1),Prog((0,t2),d2)) =
    let fun rec_sdae  (Done,Done)                 = Done
	  | rec_sdae (Dtensor(d1,d2),Dtensor(d1',d2')) =
		Dtensor(rec_sdae(d1,d1'), rec_sdae(d2,d2'))
	  | rec_sdae (Dbang(r),Dbang(r'))         = Dbang(r)
	  | rec_sdae (Dstrict(r),Dconsumed(r'))   = Dstrict(r)
	  | rec_sdae (Dresource(r),Dresource(r')) = Dconsumed(r)
	  | rec_sdae (Dresource(r),Dconsumed(r')) = Dstrict(r)
	  | rec_sdae (Dconsumed(r),Dconsumed(r')) = Dconsumed(r)
    in Prog((s1+t1-t2,0),rec_sdae(d1,d2))
    end

(* strict_difference_and_keep (D1,D2) --- changes every resource of D1 that
   is not a resource in D2 into a strict resource, keeps the remaining
   resources *)
fun strict_difference_and_keep (Prog((s1,0),d1),Prog((0,0),d2)) =
	Prog((s1,0),d1)
  | strict_difference_and_keep (Prog((s1,t1),d1),Prog((0,t2),d2)) =
    let fun rec_sdak (Done,Done)                  = Done
	  | rec_sdak (Dtensor(d1,d2),Dtensor(d1',d2')) =
		Dtensor(rec_sdak(d1,d1'),rec_sdak(d2,d2'))
	  | rec_sdak (Dbang(r),Dbang(r'))         = Dbang(r)
	  | rec_sdak (Dstrict(r),Dconsumed(r'))   = Dstrict(r)
	  | rec_sdak (Dresource(r),Dresource(r')) = Dresource(r)
	  | rec_sdak (Dresource(r),Dconsumed(r')) = Dstrict(r)
	  | rec_sdak (Dconsumed(r),Dconsumed(r')) = Dconsumed(r)
    in (Prog((s1+t1-t2,t2),rec_sdak(d1,d2)))
    end

fun augment (Prog((s,t),d1),d2,subst) =
	let val Prog((s',t'),d') = d_scan d2 subst (* t' = 0 *)
	in 
	    Prog((s+s',t),Dtensor(d',d1))
	end

fun take_back (Prog((s,t),Dtensor(d1,d2))) = (Prog((0,t),d2)) (* s = 0 *)

(* pick_resource (prog:dform) (sc:prog -> rform -> unit)          *)
(* Non-deterministically picks resource r from prog and calls sc   *)
(* on remaining resources and r.                                   *)
fun pick_resource (Prog((s,t),d)) sc =
      let fun rec_pick s t (Done) sc = ()
            | rec_pick s t (Dtensor(d1,d2)) sc =
		(rec_pick s t d1
			  (fn s'  =>
			  (fn t'  =>
			  (fn d1' =>
			  (fn r'  => sc s' t' (Dtensor(d1',d2)) r')))); 
		 rec_pick s t d2
			  (fn s'  =>
			  (fn t'  =>
			  (fn d2' =>
			  (fn r'  => sc s' t' (Dtensor(d1,d2')) r')))))
	    | rec_pick s t (Dbang(r)) sc     = sc s t (Dbang(r)) r
	    | rec_pick s t (Dstrict(r)) sc   = sc (s-1) t (Dconsumed(r)) r
	    | rec_pick s t (Dresource(r)) sc = sc s (t-1) (Dconsumed(r)) r
	    | rec_pick s t (Dconsumed(r)) sc = ()
       in rec_pick s t d (fn s' =>
			 (fn t' =>
			 (fn d' =>
			 (fn r' => sc (Prog((s',t'),d')) r'))))
       end


fun solve (Gone) prog uvars subst sc = 
	if strict(prog)
	   then sc subst prog false
	   else ()
  | solve (Gtrue) prog uvars subst sc = sc subst (erase_strict prog) true
  | solve (Gbang(g)) prog uvars subst sc =
      if strict(prog)
	 then solve g (erase prog) uvars subst
		(fn newsubst =>
		(fn newprog =>
		(fn newflag =>
			sc newsubst prog false)))
	   else ()
  | solve (Goplus(g1,g2)) prog uvars subst sc =
      ( solve g1 prog uvars subst sc ; solve g2 prog uvars subst sc )
  | solve (Gtensor(g1,g2)) prog uvars subst sc =
      solve g1 (make_non_strict prog) uvars subst
	(fn newsubst =>
	(fn newprog =>
	(fn false =>
		solve g2 (intersect_input(prog,newprog)) uvars newsubst sc
	  | true => solve g2 newprog uvars newsubst
			(fn newsubst2 =>
			(fn newprog2 =>
			(fn newflag2 =>
			  sc newsubst2 (intersect(prog,newprog2)) true))))))
  | solve (Gwith(g1,g2)) prog uvars subst sc =
      solve g1 prog uvars subst
	(fn newsubst =>
	(fn newprog =>
	(fn false => solve g2 (strict_difference_and_erase(prog,newprog))
			uvars newsubst
                        (fn newsubst2 =>
			(fn newprog2 =>
                        (fn newflag2 =>
				sc newsubst2 newprog false)))
	  | true => solve g2 (strict_difference_and_keep(prog,newprog))
		uvars newsubst sc)))
  | solve (Glinimpl(d,g)) prog uvars subst sc =
      solve g (augment(prog,d,subst)) uvars subst
		(fn newsubst =>
		(fn newprog  =>
		(fn newflag  => sc newsubst (take_back(newprog)) newflag)))
  | solve (Gatom(a)) prog uvars subst sc =
        if built_in (func_symbol_of a) 
          then if one_like_builtin(func_symbol_of a)
		then if strict(prog)
			then execute_built_in a prog uvars subst sc
			else ()
		else execute_built_in a prog uvars subst sc
	  else pick_resource prog
	         (fn newprog => (fn r => backchain a r newprog uvars subst sc))
  | solve (Gexists(x,g)) prog uvars subst sc =
      solve (gsubst (new_evar x uvars) x g) prog uvars subst sc
  | solve (Gall(x,g)) prog uvars subst sc =
      let val uvar = new_uvar x
       in solve (gsubst uvar x g) prog (uvar::uvars) subst sc
       end
  | solve (Gguard(guard,g1,g2)) prog uvars subst sc =
   let exception guard_success of Props.Terms.substitution * Props.prog * bool
	in ( solve guard (make_non_strict prog) uvars subst
		  (fn newsubst => fn newprog => fn newflag =>
		     raise guard_success(newsubst,newprog,newflag)) ;
	     solve g2 prog uvars subst sc )
		handle guard_success(newsubst,newprog,newflag) => 

	     if (newflag)
		then solve g1 newprog uvars newsubst
			(fn newsubst2 =>
			(fn newprog2 =>
			(fn newflag2 =>
			  sc newsubst2 (intersect(prog,newprog2)) true))) 
		else solve g1 (intersect_input(prog,newprog)) uvars newsubst sc
	end
  | solve (Glinload(f,g)) prog uvars subst sc =
      let val f1 = dereference f subst
          val (modhead,newclauses,newuvars) = load_module(f1,uvars)
      in
	unify modhead f1
	      (fn newsubst => 
        	 solve g (augment(prog, newclauses, newsubst))
		    (newuvars @ uvars) newsubst
			(fn newsubst1 =>
			(fn newprog =>
			(fn newflag =>
				 sc newsubst1 (take_back(newprog)) newflag))))
	      subst
      end
  | solve (Gflex(M)) prog uvars subst sc =
	(case Terms_To_Props.term_to_gform subst M
         of Gflex(M1) => raise Illegal (": Unbound variable as goal (" ^
					makestring_term M ^ ").")
	  | g => solve g prog uvars subst sc)

and backchain a r prog uvars subst sc =
      let fun rec_bc (Ratom(a1)) prog sc =
		if (strict prog)
                   then unify a1 a (fn newsubst => sc newsubst prog false)
			subst
		   else ()
            | rec_bc (Rwith(r1,r2)) prog sc =
	        (rec_bc r1 prog sc; rec_bc r2 prog sc)
	    | rec_bc (Rlinimpl(g,r)) prog sc =
		rec_bc r (make_non_strict prog)
		  (fn newsubst =>
		  (fn newprog =>
		  (fn false => 
		          solve g (intersect_input(prog,newprog))
				uvars newsubst sc
	  	    | true =>
		   	  solve g newprog uvars newsubst
				(fn newsubst2 =>
				(fn newprog2 =>
				(fn newflag2 =>
			 	    sc newsubst2 (intersect(prog,newprog2))
					true))))))
	    | rec_bc (Rall(x,r)) prog sc =
		rec_bc (rsubst (new_evar x uvars) x r) prog sc
      in rec_bc r prog sc end


and execute_built_in (Appl (Const (Name "write"),t)) prog uvars subst sc =
        (print_term (!current_out_stream) (dereference t subst) ; 
	 TextIO.flushOut (!current_out_stream)  ; 
         sc subst prog false) 
  | execute_built_in (Appl (Const (Name "write_clause"),t)) 
		     prog uvars subst sc =
      	(print_clause (!current_out_stream) (dereference t subst) ; 
	 TextIO.flushOut (!current_out_stream) ; 
         sc subst prog false) 
  | execute_built_in (Appl (Const (Name "write_sans"),t)) prog uvars subst sc =
	((case (dereference t subst)
	    of (Const (String s)) => TextIO.output(!current_out_stream, s)
	     | (Const (Name n)) => TextIO.output(!current_out_stream, n)
	     | s => print_term (!current_out_stream) (dereference t subst)) ;
	 TextIO.flushOut (!current_out_stream)  ; 
         sc subst prog false)
  | execute_built_in (Appl (Const (Name "write_raw"),t)) prog uvars subst sc =
      	(print_raw (!current_out_stream) (dereference t subst) ; 
	 TextIO.flushOut (!current_out_stream) ; 
         sc subst prog false) 
  | execute_built_in (Const (Name "nl")) prog uvars subst sc =
        (TextIO.output((!current_out_stream),"\n") ; sc subst prog false)
  | execute_built_in (Appl (Const (Name "read"),t)) prog uvars subst sc =
      let fun subnew nil nil t = t
           | subnew (evar::evars) (x::xs) t = 
           	  subnew evars xs (Terms.subst evar x t)
	  val (varnames,s) = (Parse.stream_term_parse (!current_in_stream))
				 handle Parse.Parse_EOF => 
					(nil, Const (Name "end_of_file"))
          val xs = map Terms.Varbind varnames
          val evars = map (fn vname => Terms.new_evar vname nil) xs
      in unify (subnew evars xs s) t 
	       (fn newsubst => sc newsubst prog false) subst
      end
  | execute_built_in (Appl (Appl (Const (Name "telling"), fname), Goal))
                     prog uvars subst sc =
      (case (dereference fname subst) 
	of (Const (String filename)) =>
		( current_out_stream := TextIO.openOut filename ;
		  solve (Terms_To_Props.term_to_gform subst Goal) 
			prog uvars subst
			(fn newsubst => fn newprog => fn newflag =>
				( current_out_stream := TextIO.stdOut ;
				  sc newsubst newprog newflag )) ;
		  current_out_stream := TextIO.stdOut )
	 | s as (Evar _) => raise Illegal 
				(": Filename in telling unbound (" ^
			 	 makestring_term s ^ ").")
	 | s 		 => raise Illegal 
				(": Filename in telling is not a string (" ^
			 	 makestring_term s ^ ")."))
  | execute_built_in (Appl (Appl (Const (Name "seeing"), fname), Goal)) 
                     prog uvars subst sc =
      (case (dereference fname subst) 
	of (Const (String filename)) =>
		( current_in_stream := TextIO.openIn filename ;
		  solve (Terms_To_Props.term_to_gform subst Goal) 
			prog uvars subst
			(fn newsubst => fn newprog => fn newflag =>
				( current_in_stream := TextIO.stdIn ;
				  sc newsubst newprog newflag )) ;
		  current_in_stream := TextIO.stdIn )
	 | s as (Evar _) => raise Illegal 
				(": Filename in seeing unbound (" ^
			 	 makestring_term s ^ ").")
	 | s 		 => raise Illegal 
				(": Filename in seeing is not a string (" ^
			 	 makestring_term s ^ ")."))
(*  | execute_built_in (Appl (Appl (Const (Name "timing"), Goal), time))
		     prog uvars subst sc = 
	    let
	      val start = Timer.startRealTimer ()
	    in
	      solve (Terms_To_Props.term_to_gform subst Goal) 
		    prog uvars subst
		    (fn newsubst => fn newprog => fn newflag =>
			let 
			  val realtime = Timer.checkRealTimer start 
(*			  val gc_time = check_timer_gc start 
			  val {sec=s,usec=u} =
				 add_time(non_gc_time,gc_time) --cs *)
			    
			in
(*			  unify time (Const(Int (s * 1000000 + u)))
				(fn newsubst2 => sc newsubst2 newprog newflag)
				newsubst  -- cs*)
			  unify time (Const(Int (Real.toInt IEEEReal.TO_NEAREST (1000000.0 * (Time.toReal realtime)))))
				(fn newsubst2 => sc newsubst2 newprog newflag)
				newsubst
			end)
	    end *)
  | execute_built_in (Appl (Appl (Const (Name "system"), cmd), result))
		     prog uvars subst sc =
	(case (dereference cmd subst)
	  of (Const (String command)) => 
		unify (Const (Int (OS.Process.system command))) result
		      (fn newsubst => sc newsubst prog false) subst
	   | s as (Evar _) => raise Illegal 
				(": command in system unbound (" ^
			 	 makestring_term s ^ ").")
	   | s 		 => raise Illegal 
				(": command in system is not a string (" ^
			 	 makestring_term s ^ ")."))
   | execute_built_in (Appl (Const (Name "cd"), dname)) prog uvars subst sc =
	(case (dereference dname subst)
	  of (Const (String dirname)) => 
		if (OS.FileSys.isDir dirname)
		  then (OS.FileSys.chDir dirname ; sc subst prog false)
		  else raise Illegal (": Directory name in cd is invalid (" ^
			 		dirname ^ ").")
	   | s as (Evar _) => raise Illegal 
				(": Directory name in cd unbound (" ^
			 	 makestring_term s ^ ").")
	   | s 		 => raise Illegal 
				(": Directory name in cd is not a string (" ^
			 	 makestring_term s ^ ")."))
  | execute_built_in (Appl (Const (Name "var"),t)) prog uvars subst sc =
      	(case (dereference t subst)
	   of s as (Evar _) => sc subst prog false
	    | _ => ())
  | execute_built_in (Appl (Const (Name "nonvar"),t)) prog uvars subst sc =
      	(case (dereference t subst)
	   of s as (Evar _) => ()
	    | _ => sc subst prog false)
  | execute_built_in (Appl (Appl (Const (Name "generalize"), t), s)) 
                     prog uvars subst sc =
	unify (generalize t subst) s 
	      (fn newsubst => sc newsubst prog false) subst 
  | execute_built_in (Appl (Appl (Const (Name "explode"), str), lst)) 
                     prog uvars subst sc =
	(case (dereference str subst) 
	   of (Const (String s)) =>
		let val l = explode s
                    fun cvt nil      = (Const (Name "nil"))
                      | cvt (c::cs)  = (Appl (Appl (Const (Name "::"),
						   (Const (Name (Char.toString c)))),
					     cvt cs))
		in unify (cvt l) lst 
			 (fn newsubst => sc newsubst prog false) subst 
		end
	    | s as (Evar _) =>
		(case (dereference lst subst)
		   of (l as Evar _) => raise Illegal 
			(": Both arguments to explode unbound (" ^
			 makestring_term s ^ "," ^ makestring_term l ^ ").")
		    | l =>
			let fun cvt (Const (Name "nil")) = ""
                      	      | cvt (Appl (Appl (Const (Name "::"),
						(Const (Name c))),cs)) =
			  		c ^ (cvt cs) 
		              | cvt _ = raise Illegal 
					(": 2nd argument to explode contains" ^
					 " a non-atomic constant (" ^
					 makestring_term l ^ ").")
			in unify (Const (String (cvt l))) s
		         	 (fn newsubst => sc newsubst prog false) subst 
			end)
	    | s => raise Illegal (": 1st argument to explode is not a string ("
			 	  ^ makestring_term s ^ ")."))
  | execute_built_in (Appl (Appl (Const (Name "explode_words"), str), lst)) 
                     prog uvars subst sc =
	(case (dereference str subst) 
	   of (Const (String s)) =>
		let fun explode_words nil "" = nil
		      | explode_words nil word = word::nil
		      | explode_words (#" "::cs) "" = explode_words cs "" 
		      | explode_words (#"\t"::cs) "" = explode_words cs "" 
		      | explode_words (#" "::cs) word = 
			  word::(explode_words cs "")
		      | explode_words (#"\t"::cs) word = 
			  word::(explode_words cs "")
		      | explode_words (c::cs) word = explode_words cs (word^ (Char.toString c)) 
		    val l = (explode_words (explode s) "")
                    fun cvt nil      = (Const (Name "nil"))
                      | cvt (w::ws)  = (Appl (Appl (Const (Name "::"),
						   (Const (Name w))),
					     cvt ws))
		in unify (cvt l) lst 
			 (fn newsubst => sc newsubst prog false) subst 
		end
	    | s as (Evar _) =>
		(case (dereference lst subst)
		   of (l as Evar _) => raise Illegal
			(": Both arguments to explode_words unbound (" ^
			 makestring_term s ^ "," ^ makestring_term l ^ ").")
		    | l =>
		let fun cvt (Const (Name "nil")) = ""
                      | cvt (Appl (Appl (Const (Name "::"),Const (Name w)),
				   Const (Name "nil"))) = w
                      | cvt (Appl (Appl (Const (Name "::"),Const (Name w)),
				   ws)) = w ^ " " ^ (cvt ws)
		      | cvt _ = raise Illegal 
				(": 2nd argument to explode_words contains" ^
			 	 " a non-atomic constant (" ^
				 makestring_term l ^ ").")
		in unify (Const (String (cvt l))) s
		         (fn newsubst => sc newsubst prog false) subst 
		end)
	    | s => raise Illegal (": 1st argument to explode_words is not a" ^
				  " string (" ^ makestring_term s ^ ")."))
  | execute_built_in (Appl (Appl (Const (Name "="), t1), t2)) 
		     prog uvars subst sc = 
                	unify t1 t2 (fn newsubst => sc newsubst prog false) 
			      subst
  | execute_built_in (Appl (Appl (Const (Name "is"), t), exp))
		     prog uvars subst sc = 
	let val result = (eval_exp (dereference exp subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `is' contains "
					   ^ msg)
	in unify t result (fn newsubst => sc newsubst prog false) subst
	end
  | execute_built_in (Appl (Appl (Const (Name "=:="), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `=:=' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `=:=' contains "
					   ^ msg))
	in if i1 = i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Appl (Const (Name "=\\="), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `=\\=' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `=\\=' contains "
					   ^ msg))
	in if i1 <> i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Appl (Const (Name ">="), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `>=' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `>=' contains "
					   ^ msg))
	in if i1 >= i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Appl (Const (Name "=<"), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `=<' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `=<' contains "
					   ^ msg))
	in if i1 <= i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Appl (Const (Name ">"), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `>' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `>' contains "
					   ^ msg))
	in if i1 > i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Appl (Const (Name "<"), t1), t2)) 
		     prog uvars subst sc= 
	let val i1 = term_to_int(
			(eval_exp (dereference t1 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 1st argument of `<' contains "
					   ^ msg))
            val i2 = term_to_int(
			(eval_exp (dereference t2 subst))
			  handle Terms.Illegal(msg) =>
			    raise Illegal (": 2nd argument of `<' contains "
					   ^ msg))
	in if i1 < i2 
	     then sc subst prog false
   	     else ()
	end
  | execute_built_in (Appl (Const (Name "load"),t))
		     prog uvars subst sc =
		(print ("loading " ^ makestring_term t ^ "...\n") ;
		 solve (Glinload(t,Gatom(Const (Name "top"))))
     		       prog uvars subst sc )
  | execute_built_in (Const (Name "top")) prog uvars subst sc =
      (top_level "?- " prog uvars subst
       handle POP => (print "Returning to previous top level...\n" ;
		      raise POPAUX))
  | execute_built_in (Const (Name "fail")) prog uvars subst sc = ()
  | execute_built_in (Const (Name "pop")) prog uvars subst sc = raise POP
  | execute_built_in (Const (Name "popall")) prog uvars subst sc = raise POPALL
  | execute_built_in (Const (Name "abort")) prog uvars subst sc = raise ABORT
  | execute_built_in (Const (Name "exit")) prog uvars subst sc = raise EXIT
  | execute_built_in (Const (Name "bye")) prog uvars subst sc = raise EXIT
  | execute_built_in a prog uvars subst sc =
	 raise Illegal (": Built in predicate " ^ func_symbol_of a 
		  	^ " called with the wrong number of arguments ("
			^ makestring_term a ^ ").")


and catch_errors func =
    	func ()
	   handle ABORT  => raise ABORT
		| POP 	 => raise POP
		| POPAUX => raise POPAUX
		| POPALL => raise POPALL
		| EXIT   => raise EXIT
		| Parse.Parse_EOF => raise POP 
		| Parse.Parse_Error (msg) => print(msg ^ "\n")
		| IO.Io (msg) => print("I/O Error: " ^ #name(msg) ^ "\n")
		| Illegal (msg) => 
			print("Lolli Error" ^ msg ^ "\n")
	        | Terms.Illegal(msg) => 
			print("Lolli Error" ^ msg ^ "\n")
	        | Terms.Internal(msg) => 
			print("Internal Error" ^ msg ^ "\n")
		| Terms_To_Props.Illegal (msg) => 
			print("Lolli Error" ^ msg ^ "\n")
		| Terms_To_Props.Internal (msg) => 
			print("Internal Error" ^ msg ^ "\n")
		| exn => print("Unrecognized Exception Error: " 
			 ^ (exnMessage exn) ^ "\n")

and handle_interrupt (infiniteLoop : unit -> unit) =
  let val oldHandler = Signals.inqHandler(Signals.sigINT)
  in ( SMLofNJ.Cont.callcc (fn k =>
	 ( Signals.setHandler(Signals.sigINT,Signals.HANDLER(fn _ => k)) ;
	   infiniteLoop () ;
	   raise UnexpectedReturn )) ;
       print ("\nInterrupted...\n") ;
       infiniteLoop () ;
       raise UnexpectedReturn )
     handle exn => (Signals.setHandler(Signals.sigINT, oldHandler) ;
		     raise exn )
 end

and top_level (promptstr:string) prog uvars subst =
     let exception Query_Complete
	 fun read_eval () = 
	       let val (evars,g) = query_to_goal 
                                     (Parse.stream_query_parse TextIO.stdIn)
		   val _ = all_solutions := false
		   val initial_sc =
			(fn newsubst =>
			(fn newprog =>
			(fn newflag =>
			let val ps = Terms.project_substitution evars newsubst
			in
		          ( if ((evars = nil) orelse 
				(is_empty_substitution ps))
			      then print "solved"
			      else Terms.print_substitution ps ;
			    if more_solutions ()
			      then ()  (* This backtracks *)
			      else raise Query_Complete )
			end)))
	        in (solve g prog uvars subst initial_sc ;
		     print "no\n" ) 
		   handle Query_Complete => print "yes\n"
                        | ABORT => print"\naborted...\n"
			| POPAUX => ()
  	        end
	 fun prompt () = ( print promptstr ; TextIO.flushOut TextIO.stdOut )
	 fun loop () = ( prompt () ; catch_errors(read_eval) ; loop () )
      in handle_interrupt(loop) end

and more_solutions () =
    if (!all_solutions) then ( print ";\n" ; true )
    else let val input = TextIO.inputLine TextIO.stdIn
	 in	
	     if input = "\n" then false
	     else if String.substring (input,0,1) = ";" then true
		  else if String.substring (input,0,1) = "*"
			   then ( all_solutions := true ; true )
		       else false

	 end

and ll_outer_level prog uvars =
	 ((top_level "?- " prog uvars Terms.empty_substitution
	     handle POP => (print ("You are now at the top level. " ^ 
				     "Use 'bye' to leave Lolli.\n") ;
			    ll_outer_level prog uvars))
	     handle POPALL => (print "Returning to topmost level.\n" ;
			       ll_outer_level prog uvars))
	     handle EXIT => (print "Closing Lolli. \n"; ())

and ll_file filename = print "Starting with a file loaded not currently supported.\n Use --o from within the system.\n"

and ll () = ( (* System.Control.Print.signatures := 0 ;  --cs *)
	      Compiler.Control.Print.printDepth := 15; 
	      (* System.Control.Runtime.gcmessages := 0 ;   --cs *)
	      print ("Starting Lolli version " ^ ll_version ^ 
		     "\n  (built with " ^ "SML/NJ 110" ^ ")...\n") ;
	      ll_outer_level init_prog nil)


(* comment this out if not building c code *)

(*
val _ = ll();
*)

end  (* functor Interpreter *)

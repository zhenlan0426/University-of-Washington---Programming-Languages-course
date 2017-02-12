(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn str =>Char.isUpper(String.sub(str,0)))

val longest_string1 = List.foldl (fn (a,b) => if String.size(a) > String.size(b)
					      then a
					      else b) ""

val longest_string2 = List.foldl (fn (a,b) => if String.size(a) >= String.size(b)
					      then a
					      else b) ""

fun longest_string_helper f =
	List.foldl (fn (a,b) => if f(String.size(a),String.size(b))
				then a
				else b) ""

val longest_string3 = longest_string_helper (fn (x,y) => x>y)
val longest_string4 = longest_string_helper (fn (x,y) => x>=y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string =  String.implode o rev o String.explode 

fun first_answer f lst = 
	case lst of 
		[] => raise NoAnswer
	  | hd::tl => case f hd of
	  				  SOME v => v
	  				| NONE => first_answer f tl

fun all_answers f lst =
	case lst of 
		[] => SOME []
	  | hd::tl =>case (f hd, all_answers f tl) of
		  				 (NONE,_) => NONE
		  			   | (_,NONE) => NONE
		  			   | (SOME p, SOME q) => SOME (p @ q)

fun all_answers2 f lst =
	let fun helper(acc,lst)=
			case lst of
				[] => SOME acc
			  | hd::tl => case f hd of
			  				NONE => NONE
			  			  | SOME v => helper(acc@v, tl)
	in helper([],lst)
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (String.size)

fun count_some_var(str,p)=
	g (fn _ => 0) (fn x => if x=str then 1 else 0) p



fun check_pat p =
	let fun extractStr(p)=
			case p of
			    Wildcard          => []
			  | Variable x        => [x]
			  | TupleP ps         => List.foldl (fn (p,i) => extractStr(p)@i) [] ps
			  | ConstructorP(_,p) => extractStr(p)
			  | _                 => []

		fun repeat(strLst)=
			case strLst of 
				[] => false
			  | hd::tl => repeat(tl) orelse List.exists (fn x => x=hd) tl

	in repeat (extractStr p)
	end

fun match(v,p)=
	case (v,p) of 
		(_,Wildcard) => SOME []
	  | (Unit,UnitP) => SOME []
	  | (Const i,ConstP j) => if i=j then SOME [] else NONE
	  | (v, Variable s) => SOME [(s,v)]
	  | (Tuple v, TupleP p) => if List.length v = List.length p 
	  						   then all_answers2 match (ListPair.zip(v,p))
	  						   else NONE
	  | (Constructor(s2,v), ConstructorP (s1,p)) => if s1=s2 then match(v,p) else NONE
	  | _ => NONE

fun first_match(v,pLst)=
	SOME (first_answer (fn p=>match(v,p)) pLst) handle NoAnswer => NONE

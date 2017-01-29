fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(str,strList)=
	case strList of
		[] => NONE
	  | hd::tl => if same_string(str,hd)
	  			  then SOME tl
	  			  else case all_except_option(str,tl) of
	  			  			NONE => NONE
	  			  		  | SOME ls => SOME (hd::ls)

fun get_substitutions1(strListList, str)=
	case strListList of
		[] => []
	  | hd::tl => case all_except_option(str,hd) of
	  				   NONE => get_substitutions1(tl,str)
	  				 | SOME lst => lst @ get_substitutions1(tl,str)

fun get_substitutions2(strListList, str)=
	let fun helper(lst,str,acc)=
		case lst of
			[] => acc
		  | hd::tl => case all_except_option(str,hd) of
		  				   NONE => helper(tl,str,acc)
		  				 | SOME xs => helper(tl,str,acc@xs)
	in helper(strListList,str,[])
	end

fun similar_names(strListList, {first=f,middle=m,last=l})=
	let fun helper(nameList,m,l)=
			case nameList of
				[] => []
			  | hd::tl => {first=hd,middle=m,last=l} :: helper(tl,m,l)
    in helper(f::get_substitutions2(strListList,f),m,l)
    end



datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit,_)=
	case suit of
		Spades => Black
	  | Clubs => Black
	  | _ => Red

fun card_value(_,rank)=
	case rank of
		Ace => 11
	  | Num n => n
	  | _ =>10

fun remove_card(cs,c,e)=
	case cs of 
		[] => raise e
	  | hd::tl => if hd = c
	  			  then tl
	  			  else hd::(remove_card(tl,c,e))

fun all_same_color(cardList)=
	case cardList of 
		[] => true
	  | _::[] => true
	  | hd::sh::tl => card_color(hd) = card_color(sh) andalso all_same_color(sh::tl)

fun sum_cards(cardList)=
	let fun helper(cardList,acc)=
		case cardList of
			[] => acc
		  | hd::tl => helper(tl,acc+card_value(hd))
	in helper(cardList,0)
	end

fun score(cardList,goal)=
	let val sum = sum_cards(cardList)
		val sameColor = if all_same_color(cardList)
						then 2
						else 1
	in if sum > goal
	   then 3*(sum-goal) div sameColor
	   else (goal-sum) div sameColor
	end

fun officiate(cardList,moveList,goal)=
	let fun helper(cardList,moveList,goal,heldList)=
		case (cardList,moveList) of
		    (_,[]) => score(heldList,goal)
		  | (_,Discard c::tl) => helper(cardList,tl,goal,remove_card(heldList,c,IllegalMove))
		  | ([],Draw::_) => score(heldList,goal)
		  | (hd1::tl1, Draw::tl2) => let val newHeld = hd1::heldList
		  							 in
			  							 if sum_cards(newHeld) > goal
			  							 then score(newHeld,goal)
			  							 else helper(tl1,tl2,goal,newHeld)
			  						 end
	in helper(cardList,moveList,goal,[])
	end

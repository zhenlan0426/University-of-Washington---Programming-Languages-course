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


def LossMask(Y,P,mask):
    # Y should have shape (n,N), P (N,d) and mask (n,N)
    return (np.dot(Y*mask,np.log(P)) + np.dot((1-Y)*mask,np.log(1-P)))/np.sum(mask,1,keepdims=True)

def dataGenMask(n,N,p1,p2,P):
    # P should be of shape (N,d) and serves as hyper-para
    # p1 is the prob for Y, p2 for mask
    mask = np.broadcast_to(np.random.rand(1,N)>p2,(n,N))
    while True:
        Y = np.random.rand(n,N)>p1
        C = LossMask(Y,P,mask)
        yield (C,mask)

def LossY(Y,P):
    # Y should have shape (n,N) and P (N,d)
    return (np.dot(Y,np.log(P)) + np.dot(1-Y,np.log(1-P)))/P.shape[0]

def dataGenY(n,N,p,P):
    # P should be of shape (N,d)
    while True:
        Y = np.random.randn(n,N)>p
        C = LossY(Y,P)
        yield (C,Y)

logLoss = lambda y,p: np.mean(y*np.log(p) + (1-y)*np.log(1-p))

def sigmoid(x):
    return 1/(1+np.exp(-x))
    
class GBM_k_BinaryOutput(BaseEstimator, ClassifierMixin):
    
    def __init__(self,BaseEst,M_est,learnRate,BasePara,K,baseline):
        self.BaseEst=BaseEst
        self.M_est=M_est
        self.learnRate=learnRate
        self.estimator_=[]
        self.BasePara=BasePara
        self.K = K
        self.baseline = baseline
        
    def fit(self,dataGen,restart=True,M_add=None):
          
        if M_add==None:
            M_add=self.M_est
            
        if restart==True:
            self.estimator_=[]

        for m in range(M_add):
            X,Y = dataGen.next()
            yp = self.predict_proba(X)
            print "iteration:{}, logLoss:{}".format(m,logLoss(Y,yp))
            self.estimator_.append(self.BaseEst(**self.BasePara).fit(X,Y-yp))

        self.M_est=len(self.estimator_)
        return self
        
        
    def predict_raw(self,X):
        yhat=np.copy(self.baseline)
        for m in self.estimator_:
            yhat=yhat+m.predict(X)
        return yhat       
        
    def predict_class(self,X):
        return self.predict_raw(X)>0

    def predict_proba(self,X):
        return sigmoid(self.learnRate*self.predict_raw(X))
         
    def plot_MLE(self,X,y):
        accr=np.zeros(self.M_est)        
        y_raw=np.copy(self.baseline)
            
        for m in range(self.M_est):
            y_raw=y_raw + self.learnRate*self.estimator_[m].predict(X)
            yp=sigmoid(y_raw)
            accr[m]=logLoss(y,yp)
        plt.plot(accr)
	
Model=GBM_k_BinaryOutput(ExtraTreeRegressor,100,0.01,\
                         {'max_depth':8,'splitter':'random','max_features':0.9},\
                        N,np.zeros(N))

Model.fit(dataGenY(1000,N,p,P_beta))

exception SyntaxErrorException;;
exception OutOfBoundsException;;
exception WrongParameterException;;

type ide = string;;
type integer = int;;

type bVal = 
	| Int of integer
	| Tuple of bVal list
	| Bool of bool
and fVal =
	|Fun of ide * exp
and vVal =
	|BVal of bVal
	|FVal of fVal

and exp = 
	(*Identificatori*)
	| Ide of ide 
	(*Valori*)
	| Val of vVal 
	(*Operazioni binarie*)
	| Add of exp * exp 
	| Sub of exp * exp
	| Mul of exp * exp
	| Div of exp * exp
	(*Operatori booleani*)
	| And of exp * exp 
	| Or of exp * exp
	| Not of exp 
	(*Confronto*)
	| Eq of exp * exp
	| Leq of exp * exp
	| Lt of exp * exp
	(*If-then-else*)
	| IfThenElse of exp * exp * exp
	(*Let-in*)
	| Let of exp * exp * exp
	(*Applicazione di funzione*)
	| Apply of exp * exp
	| Function of exp * exp * exp

	(*Funzioni su tuple*)
	| Access of int * bVal
	| Append of bVal * bVal
	| In of exp * bVal
	| IsEmpty of bVal
	| Slice of int * int * bVal
	| For of exp * bVal * exp

;;

let emptyEnv = [];;
let emptyFEnv = [];;

let rec lookup env x =
    match env with 
    | [] -> failwith ("Unbound variable")
    | (y, v)::r -> 
    	match y with
    	| Ide(a) -> if x = y then v else lookup r x
    	| _ -> raise SyntaxErrorException
;;

let rec map f x = 
	match x with
		| [] -> []
		| h::t -> (f h)::(map f t)
;;
let rec fold_right f lst r = 
	match lst with
		| [] -> r
		| h::t -> f h (fold_right f t r)
;;
let filter f lst = (fold_right (fun x y -> if (f x) then x::y else y) lst []);;

let length t =
	let rec aux n = function
		| [] -> n
		| _::tail -> aux (n+1) tail
	in aux 0 t;;

let getFun iden fenv = match iden with
	| Ide(x) -> 
		let rec aux i e = match e with
			| [] -> failwith ("Unbound function")
			| (id, v)::tail -> if i = id then v else aux i tail
		in aux iden fenv
	| _ -> raise WrongParameterException
;;

let rec appendT v tup = match tup with
	| [] -> v::[]
	| head::tail -> head::appendT v tail
;;

let rec reverse t = match t with
	| [] -> []
	| y::ys -> (reverse ys)@[y]
;;

let rec acc n t  = match t with
	| [] -> raise OutOfBoundsException
	| head::tail -> if n = 0 then head else acc (n-1) tail
;;
let access n t = if n>=0 then acc n t else 
	let m = -1 - n in 
		let trev = reverse(t) in 
			acc m trev
;;

let rec contains e t = match t with
	| head::tail -> if e = head then Bool(true) else contains e tail
	| [] -> Bool(false)
;;

let rec sliceAux l r t = match t with
	| [] -> []
	| head::tail -> 
		if r < l then [] else
		if ((l = 0) && (r = 0)) then head::[] else
		if l = 0 then head::sliceAux 0 (r-1) tail 
		else sliceAux (l-1) (r-1) tail
;;
let slice l r t =
	if ((l >= length (t)) || (-l > length (t))) then
		raise OutOfBoundsException else
	if ((r >= length (t)) || (-r > length (t))) then
		raise OutOfBoundsException else
	if ((l < 0 && r >= 0) || (l >= 0 && r < 0)) then
		raise OutOfBoundsException else
	if ((l >= 0) && (r >= 0)) then
		sliceAux l r t
	else (*SLICE DESTRA-SINISTRA*)
		let ll = -1 - l in
			let rr = -1 - r in
				let trev = reverse t in
					sliceAux ll rr trev
;;

let rec eval (ex:exp) env fenv = match ex with
	| Ide(ide) -> lookup env ex
	| Val (i) -> (match i with | BVal(ii) -> ii | _ -> raise SyntaxErrorException)
	| And (e1, e2) -> if((eval e1 env fenv) = Bool(true)) && ((eval e2 env fenv) = Bool(true)) then Bool(true) else Bool(false)
	| Or (e1, e2) -> if((eval e1 env fenv) = Bool(true) || (eval e2 env fenv) = Bool(true)) then Bool(true) else Bool(false)
	| Not (e1) -> applyNot e1 env fenv
	| Add (e1, e2) -> intOp (eval e1 env fenv) (eval e2 env fenv) "add"
	| Sub (e1, e2) -> intOp (eval e1 env fenv) (eval e2 env fenv) "sub"
	| Mul (e1, e2) -> intOp (eval e1 env fenv) (eval e2 env fenv) "mul"
	| Div (e1, e2) -> intOp (eval e1 env fenv) (eval e2 env fenv) "div"
	| Eq (e1, e2) -> Bool((eval e1 env fenv) = (eval e2 env fenv))
	| Leq (e1, e2) -> Bool((eval e1 env fenv) <= (eval e2 env fenv))
	| Append (e1, e2) -> (match e2 with | Tuple(k) -> Tuple(appendT e1 k) | _ -> raise WrongParameterException) 
	| In (e, t) -> (match t with | Tuple(k) -> contains (eval e env fenv) k | _ -> raise WrongParameterException)
	| IsEmpty (t) -> (match t with | Tuple(k) -> isEmpty k | _ -> raise WrongParameterException)
	| Slice (l, r, t) -> (match t with | Tuple(k) -> Tuple(slice l r k)	| _ -> raise WrongParameterException)
	| Access (n, t) -> (match t with | Tuple(k) -> access n k | _ -> raise WrongParameterException)
	| IfThenElse(g, e1, e2) -> if(eval g env fenv) = Bool(true) then eval e1 env fenv else eval e2 env fenv
	| Let (ide, e1, e2) -> let newenv = bind ide e1 env in eval e2 newenv []
	| Apply (i, e) -> (match i with | Ide(k) -> applyF i e env fenv | _ -> raise WrongParameterException)
	| For (id, tup, e) -> (match tup with | Tuple(k) -> Tuple(forT id k e env fenv)| _ -> raise WrongParameterException)
	| _ -> raise SyntaxErrorException

and intOp  e1 e2 o= match (e1, e2, o) with
	| (Int(v1),Int(v2),"add") -> Int(v1+v2)
	| (Int(v1),Int(v2),"sub") -> Int(v1-v2)
	| (Int(v1),Int(v2),"mul") -> Int(v1*v2)
	| (Int(v1),Int(v2),"div") -> Int(v1/v2)
	| (_,_,_) -> failwith ("Invalid operator")

and isEmpty t = match t with
	| [] -> Bool(true) 
	| _ -> Bool(false)

and applyNot e1 env fenv =
	let e =(eval e1 env fenv) in
		if e = Bool(true) then 
			Bool(false) else 
		if e = Bool(false) then 
			Bool(true) 
		else raise SyntaxErrorException

and forT i t ex en fen = match i with
	| Ide(iden) ->(
		match t with
		| [] -> failwith ("Empty Tuple")
		| _ -> (*fa il bind della variabile con head, valuta l'espressione e nel nuovo ambiente, richiama se stessa su tail*)
			let rec auxfor id tup e env fenv =(
				match tup with
					| [] -> []
					| head::tail -> let newEnv = bind id (Val(BVal(head))) env in eval e newEnv fenv :: auxfor id tail e env fenv)
				in auxfor i t ex en fen)
	| _ -> raise WrongParameterException

and applyF i e env fenv =
	let evalued = eval e env [] in
		let (param, body, newFenv) = getFun i fenv in
			let newEnv = bind param (Val(BVal(evalued))) newFenv in
				eval body newEnv fenv
	

and bind i e env = 
	match i with
		| Ide(x) -> (i, eval e env [])::env
		| _ -> raise WrongParameterException
;;

let bindFenv i e env =
	match i with
		| Ide(x) -> (i, e)::env
		| _ -> raise WrongParameterException
;;
let funBinder (ex: exp) env fenv = match ex with
	|Function (n, p, body) -> (
		match n with 
		| Ide(k) -> bindFenv n (p, body, env) fenv
		| _ -> raise WrongParameterException)
	| _ -> raise SyntaxErrorException
;;
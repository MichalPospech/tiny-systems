// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyBool|TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyVariable(var) -> var.Equals vcheck
  | TyFunction(t1, t2)|TyTuple(t1,t2) -> occursCheck vcheck t1 || occursCheck vcheck t2

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyBool | TyNumber -> ty
  | TyVariable(v) -> if subst.ContainsKey v then subst.[v] else TyVariable(v)
  | TyList(tv) -> TyList(substType subst tv)
  | TyFunction(t1,t2) -> TyFunction(substType subst t1, substType subst t2)
  | TyTuple(t1,t2) -> TyTuple(substType subst t1, substType subst t2)


let substConstrs subst cs = 
  List.map (fun cons -> substType subst (fst cons), substType subst (snd cons)) cs 
 
let rec solve cs =
  match cs with 
  | [] -> Map.empty
  | (TyNumber, TyNumber)::cs | (TyBool,TyBool)::cs -> solve cs
  | (t, TyVariable(tv))::cs | (TyVariable(tv), t)::cs ->
    if occursCheck tv t then failwith "Cannot be solved (occurs check)"
    let cons = substConstrs (Map [(tv, t )])  cs
    let subs = solve cons
    let t = substType subs t 
    subs.Add(tv, t)
  | (TyList(t1), TyList(t2))::cs -> solve ((t1,t2)::cs)
  | (TyFunction(ta1, tr1), TyFunction(ta2,tr2))::cs -> solve ((ta1, ta2)::(tr1,tr2)::cs)
  | (TyTuple(t11,t12), TyTuple(t21,t22))::cs -> solve ((t11,t21)::(t12,t22)::cs)
  | (_,_)::cs -> failwith "Substitution not possible"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) | Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      // TODO: Similar to the case for '+' but returns 'TyBool'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]


  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      // TODO: Just get the type of the variable from 'ctx' here.
      ctx.[v] ,[]
  | If(econd, etrue, efals) ->
      // TODO: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let tcond, scond = generate ctx econd
      let ttrue, strue = generate ctx etrue
      let tfals, sfals = generate ctx efals
      ttrue, scond @ strue @ sfals @ [tcond, TyBool ; ttrue , tfals]

  | Let(v, e1, e2) ->
      // TODO: Generate type & constraints for 'e1' first and then
      // add the generated type to the typing context for 't2'.
    let t1, s1 = generate ctx e1
    let t2, s2 = generate (ctx.Add (v, t1)) e2
    t2, s1 @ s2 
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      // TODO: We do not know what the type of the variable 'v' is, so we 
      // generate a new type variable and add that to the 'ctx'. The
      // resulting type will be 'TyFunction' with 'targ' as argument type.
      let t, s =  generate (ctx.Add(v, targ)) e
      TyFunction(targ, t), s 

  | Application(e1, e2) -> 
      // TODO: Tricky case! We cannot inspect the generated type of 'e1'
      // to see what the argument/return type of the function is. Instead,
      // we have to generate a new type variable and add a constraint.

    let t2, s2 = generate ctx e2
    let t1, s1 = generate ctx e1
    let tres = newTyVariable()
    tres, s1 @ s2 @ [TyFunction(t2, tres), t1]

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
    let t1, s1 = generate ctx e1
    let t2, s2 = generate ctx e2
    TyTuple(t1,t2), s1 @ s2

  | TupleGet(b, e) ->
    let t, s = generate ctx e
    t, s @ [TyTuple(newTyVariable(), newTyVariable()), t]
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer

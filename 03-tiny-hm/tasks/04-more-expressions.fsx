// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyBool|TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyVariable(var) -> var.Equals vcheck
  | TyFunction(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyBool | TyNumber -> ty
  | TyVariable(v) -> if subst.ContainsKey v then subst.[v] else TyVariable(v)
  | TyList(tv) -> TyList(substType subst tv)
  | TyFunction(t1,t2) -> TyFunction(substType subst t1, substType subst t2)

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
  | (_,_)::cs -> failwith "Substitution not possible"
  // TODO: Add case matching TyFunction(ta1, tb1) and TyFunction(ta2, tb2)
  // This generates two new constraints, equating the argument/return types.



// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType subst typ
  typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a 
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] -> 
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
|> infer 

// let f = fun x -> x*2 in (f 20) + (f 1)
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f 
// This does not type check due to occurs check
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer

// fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer

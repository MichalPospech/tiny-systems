// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with
  | TyBool|TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyVariable(var) -> var.Equals vcheck
 
let rec substType (subst:Map<string, Type>) ty = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyBool | TyNumber -> ty
  | TyVariable(v) -> if subst.ContainsKey v then subst.[v] else TyVariable(v)
  | TyList(tv) -> TyList(substType subst tv)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  List.map (fun cons -> substType subst (fst cons), substType subst (snd cons)) cs 
 
let rec solve cs =
  match cs with 
  | [] -> Map.empty
  | (TyNumber, TyNumber)::cs | (TyBool,TyBool)::cs -> solve cs
  | (t, TyVariable(tv))::cs | (TyVariable(tv), t)::cs ->
    if occursCheck tv t then failwith "Cannot be solved (occurs check)"
    let cons = substConstrs (Map [(tv, t )]) cs
    let subs = solve cons
    let t = substType subs t 
    subs.Add(tv, t)
  | (TyList(t1), TyList(t2))::cs -> solve ((t1,t2)::cs)
  | (_,_)::cs -> failwith "Substitution not possible"


  // TODO: Fill in the remaining cases! You can closely follow the
  // example from task 1 - the logic here is exactly the same.


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]

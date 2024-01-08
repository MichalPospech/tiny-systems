// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  // NOTE: Added 'Call' as a special kind of predicate
  | Call of Term * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Predicate(p, terms) -> Predicate(p, substituteTerms subst terms)
  | Variable(x) ->
      match subst.TryFind(x) with
      | Some(t) -> t
      | None -> Variable(x)
  | x -> x

and substituteSubst (newSubst: Map<string, Term>) (subst: list<string * Term>) =
  List.map (fun (v, t) -> v, substitute newSubst t) subst

and substituteTerms subst (terms: list<Term>) = List.map (substitute subst) terms
let rec unifyLists l1 l2 =
  match l1, l2 with
  | [], [] -> Some []
  | h1 :: t1, h2 :: t2 ->
      unify h1 h2
      |> Option.bind (fun s1 ->
          (unifyLists (substituteTerms (Map.ofList s1) t1) (substituteTerms (Map.ofList s1) t2))
          |> Option.map (fun s2 -> s2 @ (substituteSubst (Map.ofList s2) s1)))
  | _, _ -> None


  // TODO: This is where we need a clever trick to handle 'Call'!
  // Unification can succeed if we have a predicate and a call with a
  // corresponding predicate as the first argument. So we can unify:
  //
  //   Predicate(p1, args1) ~ Call(Predicate(p2, args2a), args2b)
  //
  // When 'p1 = p2' and when we can unify 'args1 ~ args2a @ args2b'.
and unify t1 t2 =
  match t1, t2 with
  | Atom(x), Atom(y) when x = y -> Some []
  | Predicate(x, terms1), Predicate(y, terms2) when x = y -> unifyLists terms1 terms2
  | Variable(x), y
  | y, Variable(x) -> Some [ (x, y) ]
  | Call(Predicate(x, terms1), terms2), Predicate(y, terms3)
  | Predicate(y, terms3), Call(Predicate(x, terms1), terms2)  when x = y -> unifyLists terms3 (terms1 @ terms2)
  | _, _ -> None
// ----------------------------------------------------------------------------
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term =
  match term with
  | Atom("zero") -> Some(0)
  | Predicate("succ", [ x ]) ->
      match x with
      | Number x -> Some(x + 1)
      | _ -> None
  | _ -> None


let rec (|List|_|) term : option<list<Term>> =
  match term with
  | Atom("empty") -> Some([])
  | Predicate("cons", [ h; tl ]) ->
      match tl with
      | List l -> Some(h :: l)
      | _ -> None
  | _ -> None
  
  
let rec formatTerm term =
  match term with
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | List items -> sprintf "[%s]" (String.concat "; " <| List.map formatTerm items)
  | Predicate(p, items) -> sprintf "%s(%s)" p (String.concat ", " <| List.map formatTerm items)
  | Call(term, args) -> formatTerm (Predicate("call", term::args))
  
// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term =
  match term with
  | Atom(_) -> []
  | Variable(x) -> [ x ]
  | Predicate(_, terms) -> List.collect freeVariables terms
  | Call(term, terms) -> List.collect freeVariables <| term::terms
  

let withFreshVariables (clause: Clause) : Clause =
  let n = nextNumber()

  let subs =
      List.collect freeVariables <| [ clause.Head ] @ clause.Body
      |> List.distinct
      |> List.map (fun x -> (x, Variable(sprintf "__var_%s_%d" x n)))

  { Head = substitute (Map.ofList subs) clause.Head
    Body = substituteTerms (Map.ofList subs) clause.Body }
  
let query (program: list<Clause>) (query: Term) : list<Clause * list<string * Term>> =

  program
  |> List.map withFreshVariables
  |> List.choose (fun c -> unify c.Head query |> Option.map (fun s -> (c, s)))


let rec solve program subst goals : seq<list<string * Term>> =
  seq {

      match goals with
      | g :: goals ->

          let matches = query program g

          for clause, newSubst in matches do
              let newGoals = substituteTerms (Map.ofList newSubst) (goals @ clause.Body)
              let subst' = substituteSubst (Map.ofList newSubst) subst
              yield! solve program (newSubst @ subst') newGoals

      | [] -> yield subst

  }
  

let run program query =
  let vars = Set.ofSeq (freeVariables query)

  for subst in solve program [] [ query ] do
      let subst' = subst |> List.filter (fst >> vars.Contains)

      for var, term in subst' do
          printfn "%s -> %s" var (formatTerm term)

      printfn ""

  printfn "---"

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n =
  match n with
  | 0 -> Atom("zero")
  | x -> Predicate("succ", [ num (x - 1) ])

let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term =
  match l with
  | [] -> Atom("empty")
  | h :: t -> Predicate("cons", [ h; makeList t ])

let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// ----------------------------------------------------------------------------
// Call and maplist
// ----------------------------------------------------------------------------

// The Prolog 'call' operation takes a term and a list of arguments
// and supplies the arguments as additional arguments to the term.
// So, for example, calling 'call(add(1), 2, X)' becomes 'add(1, 2, X)'
run nums (Call(Predicate("add", [num 1]), [num 2; Variable "X"]))
run nums (Call(Predicate("add", [num 1; Variable "X"]), [num 5]))

// This can be used to implement the 'maplist' function:
// $ maplist(_, [], []).
// $ maplist(G,[X|Xs],[Y|Ys]) :- maplist(G,Xs,Ys), call(G,X,Y).
let maplist = [
  fact (Predicate("maplist", [ Variable("_"); Atom("empty"); Atom("empty") ]))
  rule (Predicate("maplist", [ 
    Variable("G")
    Predicate("cons", [ Variable("X"); Variable("Xs") ])
    Predicate("cons", [ Variable("Y"); Variable("Ys") ]);  
  ])) [
    Predicate("maplist", [ Variable("G"); Variable("Xs"); Variable("Ys") ])
    Call(Variable("G"), [ Variable("X"); Variable("Y") ])
  ]
]

// Query: maplist(add(10), l1to9, Y)
// Returns: Y -> [11; 12; ..; 19]
run (nums @ maplist) (Predicate("maplist", 
  [ Predicate("add", [num 10]); l1to9; Variable("Y") ]))
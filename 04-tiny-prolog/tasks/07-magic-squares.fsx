// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst: Map<string, Term>) term =
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

and unify t1 t2 =
  match t1, t2 with
  | Atom(x), Atom(y) when x = y -> Some []
  | Predicate(x, terms1), Predicate(y, terms2) when x = y -> unifyLists terms1 terms2
  | Variable(x), y
  | y, Variable(x) -> Some [ (x, y) ]
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


// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber =
  let mutable n = 0

  fun () ->
      n <- n + 1
      n

let rec freeVariables term =
  match term with
  | Atom(_) -> []
  | Variable(x) -> [ x ]
  | Predicate(_, terms) -> List.collect freeVariables terms


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

let permutation =
  append @ [
    fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
    rule (Predicate("perm", [ Variable("L"); Predicate("cons", [Variable("H"); Variable("T")]) ])) [
      Predicate("append", [ Variable("V"); Predicate("cons", [Variable("H"); Variable("U")]); Variable("L") ])
      Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
      Predicate("perm", [ Variable("W"); Variable("T") ])
    ]
  ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [l1to4; Variable("X")]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [a; b])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will 
// manage to generate a magic square sooner or later...
let l = [ 2;7;6; 1;3;4; 5;8;9 ]

let magic = permutation @ nums @ [
  rule (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ])) [
    Predicate("add", [ var "A"; var "B"; var "T" ])
    Predicate("add", [ var "T"; var "C"; var "S" ])
  ]
  rule (Predicate("magic", [ var "S"; var "X" ])) [
    yield Predicate("perm", [makeList [ for i in l -> num i ]; var "X"])
    yield var "X" .=. makeList [ var "A1"; var "A2"; var "A3"; var "B1"; 
      var "B2"; var "B3"; var "C1"; var "C2"; var "C3" ]    
    for a, b, c in [ 
      ("A1","A2","A3"); ("B1","B2","B3"); ("C1","C2","C3") 
      ("A1","B1","C1"); ("A2","B2","C2"); ("A3","B3","C3")
      ("A1","B2","C3"); ("A3","B2","C1") ] do
      yield Predicate("add3", [var a; var b; var c; var "S"]) 
  ]
]

run magic (Predicate("magic", [num 15; var "X"]))

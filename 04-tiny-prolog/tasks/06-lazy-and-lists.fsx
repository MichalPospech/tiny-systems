// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
// ----------------------------------------------------------------------------

type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list

type Clause = { Head: Term; Body: Term list }

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
    // TODO: Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
    //
    // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
    // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then
    // it returns a concatentated list 's1 @ s2'. Modify the code so that:
    //
    // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
    // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
    //
    // You can look at your ML type inference code. The structure is very similar!
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
    // TODO: If the term represents a list, this should return the
    // elements of the list collected in an ordinary F# list.
    // If the term is 'Atom("empty")' return Some([])
    // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
    // a term representing a list 'l', return Some(h::l).
    match term with
    | Atom("empty") -> Some([])
    | Predicate("cons", [ h; tl ]) ->
        match tl with
        | List l -> Some(h :: l)
        | _ -> None
    | _ -> None


let rec formatTerm term =
    // TODO: Add a case for 'List(items)' - pretty print this as a list
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
    let n = nextNumber().ToString()

    let subs =
        List.collect freeVariables <| [ clause.Head ] @ clause.Body
        |> List.distinct
        |> List.map (fun x -> (x, Variable(x + n)))

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
// Querying the British royal family
// ----------------------------------------------------------------------------

let family =
    [ fact (Predicate("male", [ Atom("William") ]))
      fact (Predicate("female", [ Atom("Diana") ]))
      fact (Predicate("male", [ Atom("Charles") ]))
      fact (Predicate("male", [ Atom("George") ]))
      fact (Predicate("parent", [ Atom("Diana"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("Charles"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("William"); Atom("George") ]))
      rule
          (Predicate("father", [ Variable("X"); Variable("Y") ]))
          [ Predicate("parent", [ Variable("X"); Variable("Y") ])
            Predicate("male", [ Variable("X") ]) ] ]

// Queries from previous step (now called using 'run')
run family (Predicate("father", [ Variable("X"); Atom("William") ]))
run family (Predicate("father", [ Variable("X"); Variable("Y") ]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
    match n with
    | 0 -> Atom("zero")
    | x -> Predicate("succ", [ num (x - 1) ])
// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums =
    [ fact (Predicate("add", [ Atom("zero"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "add",
              [ Predicate("succ", [ Variable("X") ])
                Variable("Y")
                Predicate("succ", [ Variable("Z") ]) ]
          ))
          [ Predicate("add", [ Variable("X"); Variable("Y"); Variable("Z") ]) ]
      fact (Predicate("eq", [ Variable("X"); Variable("X") ])) ]

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [ num 2; num 3; Variable("X") ]))
run nums (Predicate("add", [ num 2; Variable("X"); num 5 ]))
run nums (Predicate("add", [ num 2; Variable("Y"); Variable("X") ]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term =
    // TODO: Write a helper that generates a term representing a list.
    // This should return Atom("empty") when 'l' is [] and otherwise
    // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
    match l with
    | [] -> Atom("empty")
    | h :: t -> Predicate("cons", [ h; makeList t ])


// TinyProlog code to represent 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append =
    [ fact (Predicate("append", [ Atom("empty"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "append",
              [ Predicate("cons", [ Variable("X"); Variable("Y") ])
                Variable("Z")
                Predicate("cons", [ Variable("X"); Variable("W") ]) ]
          ))
          [ Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ]) ] ]

let l1to4 = makeList [ for i in 1..4 -> num i ]
let l5to9 = makeList [ for i in 5..9 -> num i ]
let l1to9 = makeList [ for i in 1..9 -> num i ]

// TODO: Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [ l1to4; l5to9; Variable "X" ]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [ l1to4; Variable "X"; l1to9 ]))

// Query: append(X, Y, [1..9])
// Return:
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [ Variable "Y"; Variable "X"; l1to9 ]))

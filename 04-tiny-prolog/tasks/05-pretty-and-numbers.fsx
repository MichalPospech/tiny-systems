// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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


let rec formatTerm term =
    match term with
    // Simple cases for number, atom and variable are done already...
    | Number n -> string n
    | Atom s -> s
    | Variable v -> v
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
    // TODO: Get a list of distinct variables in the clause (using
    // 'freeVariables' and 'List.distinct'), generate a substitution
    // that append a number 'n' obtained by 'nextNumber()' to the end
    // of all the variable names, and apply the substitutions to the
    // head and body of the clause.
    //
    // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
    // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
    //
    // This may not be correct if the user-provided names of variables
    // had numbers in them in a certain format, but that's OK for now!
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

let rec solve program subst goals =
    match goals with
    | g :: goals ->
        // TODO: We need to solve the goal (term) 'g'. To do so, find all
        // matching clauses in the 'program' using 'query' and iterate over
        // the returned list using 'for clause, newSubst in matches do'.
        // For each possible solution, we need to add the 'clause.Body' to
        // the list of 'goals' and apply the substitution 'newSubst' to the
        // new concatentated list of 'goals'. Then we need to apply the
        // substitution 'newSubst' to the substitution 'subst' we have so far,
        // append the two and call 'solve' recursively with this new substitution
        // to solve the new goals.
        let matches = query program g

        for clause, newSubst in matches do
            let newGoals = substituteTerms (Map.ofList newSubst) (goals @ clause.Body)
            let subst' = substituteSubst (Map.ofList newSubst) subst
            solve program (newSubst @ subst') newGoals

    | [] ->
        for var, term in subst do
            printfn "%s -> %s" var (formatTerm term)

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [ Variable("X"); Atom("William") ]) ]
solve family [] [ Predicate("father", [ Variable("X"); Variable("Y") ]) ]


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


// Query: add(2, 3, X)
// Output should include: 'X = 5'
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [ num 2; num 3; Variable("X") ]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3'
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [ num 2; Variable("X"); num 5 ]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))'
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [ num 2; Variable("Y"); Variable("X") ]) ]

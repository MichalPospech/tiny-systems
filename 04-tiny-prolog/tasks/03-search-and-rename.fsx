open System
// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
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
    // TODO: Return all clauses from 'program' whose 'Head' can be
    // unified with the specified 'query' and return the resulting
    // substitutions. Before unifying, rename variables in the program
    // rule using 'withFreshVariables'. You can do this using 'List.choose'
    // or by using list comprehension.
    //
    // The return type of this is a list of tuples consisting of the matching
    // clause and a substitution (list<string * Term>). Calling 'unify'
    // gives you 'option<list<string * Term>>', so you need to pattern match
    // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
    program
    |> List.map withFreshVariables
    |> List.choose (fun c -> unify c.Head query |> Option.map (fun s -> (c, s)))


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule
    (Predicate("grandparent", [ Variable("X"); Variable("Y") ]))
    [ Predicate("parent", [ Variable("X"); Variable("Z") ])
      Predicate("parent", [ Variable("Z"); Variable("Y") ]) ]
|> withFreshVariables

// Some information about the British royal family
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

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [ Variable("X") ])) |> Console.WriteLine

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [ Variable("X"); Atom("William") ]))
|> Console.WriteLine

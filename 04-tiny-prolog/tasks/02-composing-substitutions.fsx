open Microsoft.FSharp.Core
open System
// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
    (Predicate("loves", [ Atom("narcissus"); Atom("narcissus") ]))
    (Predicate("loves", [ Variable("X"); Variable("X") ]))
|> Console.WriteLine

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
    (Predicate("loves", [ Atom("odysseus"); Atom("penelope") ]))
    (Predicate("loves", [ Variable("X"); Variable("X") ]))
|> Console.WriteLine

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
    (Predicate("add", [ Atom("zero"); Predicate("succ", [ Atom("zero") ]) ]))
    (Predicate("add", [ Variable("Y"); Predicate("succ", [ Variable("Y") ]) ]))
|> Console.WriteLine

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify (Predicate("loves", [ Variable("X"); Atom("narcissus") ])) (Predicate("loves", [ Variable("Y"); Variable("X") ]))
|> Console.WriteLine

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
    (Predicate("add", [ Predicate("succ", [ Variable("X") ]); Variable("X") ]))
    (Predicate("add", [ Variable("Y"); Predicate("succ", [ Variable("Z") ]) ]))
|> Console.WriteLine

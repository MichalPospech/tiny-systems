// ----------------------------------------------------------------------------
// 01 - Implementing basic unication of terms
// ----------------------------------------------------------------------------

// A term is a recursive type, which can be either an atom (a known fact
// like 'socrates'), a variable (to which we want to asign a term by
// unification), or a predicate (with predicate name and a list of arguments).
type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list
//| Call of Term * Term list

// A clause is for example 'mortal(X) :- human(X)' or 'human(socrates)'. It
// consists of a head and body (head :- body). Body is a sequence of terms.
type Clause = { Head: Term; Body: Term list }

// A program is just a list of clauses
type Program = Clause list

// Create a clause that states a fact
let fact p = { Head = p; Body = [] }

// Create a clause that defines a rule
let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Unification of terms and lists
// ----------------------------------------------------------------------------

let rec unifyLists l1 l2 : option<list<string * Term>> =

    match l1, l2 with
    | [], [] -> Some []
    | h1 :: t1, h2 :: t2 ->
        match unify h1 h2, unifyLists t1 t2 with
        | Some(x), Some(y) -> Some(x @ y)
        | _, _ -> None
    | _, _ -> None

and unify t1 t2 =

    match t1, t2 with
    | Atom(x), Atom(y) when x = y -> Some []
    | Predicate(x, terms1), Predicate(y, terms2) when x = y -> unifyLists terms1 terms2
    | Variable(x), y
    | y, Variable(x) -> Some [ (x, y) ]
    | _, _ -> None

// ----------------------------------------------------------------------------
// Basic unification tests
// ----------------------------------------------------------------------------

// Example: human(socrates) ~ human(X)
// Returns: [X -> socrates]
unify (Predicate("human", [ Atom("socrates") ])) (Predicate("human", [ Variable("X") ]))
|> System.Console.WriteLine

// Example: human(socrates) ~ mortal(X)
// Returns: None (fail)
unify (Predicate("human", [ Atom("socrates") ])) (Predicate("mortal", [ Variable("X") ]))
|> System.Console.WriteLine

// Example: parent(charles, harry) ~ parent(charles, X)
// Returns: [X -> harry]
unify
    (Predicate("parent", [ Atom("charles"); Atom("harry") ]))
    (Predicate("parent", [ Atom("charles"); Variable("X") ]))
|> System.Console.WriteLine

// Example: parent(X, harry) ~ parent(charles, Y)
// Returns: [X -> charles; Y -> harry]
unify (Predicate("parent", [ Variable("X"); Atom("harry") ])) (Predicate("parent", [ Atom("charles"); Variable("Y") ]))
|> System.Console.WriteLine

// Example: succ(succ(succ(zero))) ~ succ(X)
// Returns: [X -> succ(succ(zero))]
unify
    (Predicate("succ", [ Predicate("succ", [ Predicate("succ", [ Atom("zero") ]) ]) ]))
    (Predicate("succ", [ Variable("X") ]))
|> System.Console.WriteLine

// Example: succ(succ(zero)) ~ succ(zero)
// Returns: None (fail)
unify (Predicate("succ", [ Predicate("succ", [ Atom("zero") ]) ])) (Predicate("succ", [ Atom("zero") ]))
|> System.Console.WriteLine

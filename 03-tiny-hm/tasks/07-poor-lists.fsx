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
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    // NOTE: Added the unit value and recursive definition
    | Recursive of string * Expression * Expression
    | Unit

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    | TyFunction of Type * Type
    | TyTuple of Type * Type
    | TyUnion of Type * Type
    // NOTE: We need another primitive type for units
    | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    match ty with
    | TyBool
    | TyNumber
    | TyUnit -> false
    | TyList(ty) -> occursCheck vcheck ty
    | TyVariable(var) -> var.Equals vcheck
    | TyFunction(t1, t2)
    | TyTuple(t1, t2)
    | TyUnion(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2

let rec substType (subst: Map<_, _>) ty =
    match ty with
    | TyBool
    | TyNumber
    | TyUnit -> ty
    | TyVariable(v) -> if subst.ContainsKey v then subst.[v] else TyVariable(v)
    | TyList(tv) -> TyList(substType subst tv)
    | TyFunction(t1, t2) -> TyFunction(substType subst t1, substType subst t2)
    | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)
    | TyUnion(t1, t2) -> TyUnion(substType subst t1, substType subst t2)

let substConstrs subst cs =
    List.map (fun cons -> substType subst (fst cons), substType subst (snd cons)) cs

let rec solve cs =
    match cs with
    | [] -> Map.empty
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs
    | (TyUnit, TyUnit) :: cs -> solve cs
    | (t, TyVariable(tv)) :: cs
    | (TyVariable(tv), t) :: cs ->
        if occursCheck tv t then
            failwith "Cannot be solved (occurs check)"

        let cons = substConstrs (Map [ (tv, t) ]) cs
        let subs = solve cons
        let t = substType subs t
        subs.Add(tv, t)
    | (TyList(t1), TyList(t2)) :: cs -> solve ((t1, t2) :: cs)
    | (TyFunction(ta1, tr1), TyFunction(ta2, tr2)) :: cs -> solve ((ta1, ta2) :: (tr1, tr2) :: cs)
    | (TyTuple(t11, t12), TyTuple(t21, t22)) :: cs -> solve ((t11, t21) :: (t12, t22) :: cs)
    | (TyUnion(t11, t12), TyUnion(t21, t22)) :: cs -> solve ((t11, t21) :: (t12, t22) :: cs)
    | (_, _) :: cs -> failwith "Substitution not possible"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ ->
        // NOTE: If the expression is a constant number, we return
        // its type (number) and generate no further constraints.
        TyNumber, []

    | Binary("+", e1, e2)
    | Binary("*", e1, e2) ->
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


    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op

    | Variable v ->
        // TODO: Just get the type of the variable from 'ctx' here.
        ctx.[v], []
    | If(econd, etrue, efals) ->
        // TODO: Call generate recursively on all three sub-expressions,
        // collect all constraints and add a constraint that (i) the type
        // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
        let tcond, scond = generate ctx econd
        let ttrue, strue = generate ctx etrue
        let tfals, sfals = generate ctx efals
        ttrue, scond @ strue @ sfals @ [ tcond, TyBool; ttrue, tfals ]

    | Let(v, e1, e2) ->
        // TODO: Generate type & constraints for 'e1' first and then
        // add the generated type to the typing context for 't2'.
        let t1, s1 = generate ctx e1
        let t2, s2 = generate (ctx.Add(v, t1)) e2
        t2, s1 @ s2

    | Lambda(v, e) ->
        let targ = newTyVariable ()
        // TODO: We do not know what the type of the variable 'v' is, so we
        // generate a new type variable and add that to the 'ctx'. The
        // resulting type will be 'TyFunction' with 'targ' as argument type.
        let t, s = generate (ctx.Add(v, targ)) e
        TyFunction(targ, t), s

    | Application(e1, e2) ->
        // TODO: Tricky case! We cannot inspect the generated type of 'e1'
        // to see what the argument/return type of the function is. Instead,
        // we have to generate a new type variable and add a constraint.

        let t2, s2 = generate ctx e2
        let t1, s1 = generate ctx e1
        let tres = newTyVariable ()
        tres, s1 @ s2 @ [ TyFunction(t2, tres), t1 ]

    | Tuple(e1, e2) ->
        // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyTuple(t1, t2), s1 @ s2

    | TupleGet(b, e) ->
        let t, s = generate ctx e
        t, s @ [ TyTuple(newTyVariable (), newTyVariable ()), t ]

    | Match(e, v, e1, e2) ->
        // TODO: As with tuples, we know the type of 'e' is some union,
        // but we do not know what. We need new type variables. When
        // checking 'e1' and 'e2', add variable 'v' to the context!
        // Also note that the return types of 'e1' and 'e2' have to match.
        let tu1 = newTyVariable ()
        let tu2 = newTyVariable ()
        let t, s = generate ctx e
        let t1, s1 = generate (ctx.Add((v, tu1))) e1
        let t2, s2 = generate (ctx.Add((v, tu2))) e2
        t1, s @ s1 @ s2 @ [ TyUnion(tu1, tu2), t; t1, t2 ]

    | Case(b, e) ->
        // TODO: Here, we know the type of 'e' is the type of one of
        // the cases, but we still need a new type variable for the other.
        let t, s = generate ctx e
        let newTyVar = newTyVariable ()

        match b with
        | true -> TyUnion(t, newTyVar), s
        | false -> TyUnion(newTyVar, t), s

    | Unit ->
        // NOTE: This is so easy I wrote it for you :-)
        TyUnit, []

    | Recursive(v, e1, e2) ->
        // TODO: This is easier than evaluation. We need a new type variable
        // for the type of the thing we are defining (variable 'v') and add
        // it to the context when checking both 'e1' and 'e2'.
        let var = newTyVariable ()
        let t1, s1 = generate (ctx.Add(v, var)) e1
        let t2, s2 = generate (ctx.Add(v, var)) e2
        t2, s1 @ s2 @ []



// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e =
    let typ, constraints = generate Map.empty e
    let subst = solve constraints
    let typ = substType (subst) typ
    typ

// Helper to generate list 1 .. 5 from TinyML tasks
let rec makeListExpr l =
    match l with
    | x :: xs -> Case(true, Tuple(x, makeListExpr xs))
    | [] -> Case(false, Unit)

// We can type check this, but the type is horrible!
makeListExpr [ for i in 1..5 -> Constant i ] |> infer

// Code for the List.map function from TinyML task. This fails to check.
Recursive(
    "map",
    Lambda(
        "f",
        Lambda(
            "l",
            Match(
                Variable("l"),
                "x",
                Case(
                    true,
                    Tuple(
                        Application(Variable "f", TupleGet(true, Variable "x")),
                        Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x"))
                    )
                ),
                Case(false, Unit)
            )
        )
    ),
    Variable("map")
)
|> infer

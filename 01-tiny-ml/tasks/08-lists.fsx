// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    | ValCase of bool * Value
    // NOTE: A value that represents "empty value" and is
    // useful as the value for representing the empty list.
    | ValUnit

and Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Variable of string
    | Unary of string * Expression
    | If of Expression * Expression * Expression
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    | Recursive of string * Expression * Expression
    // NOTE: An expression that evaluates to a unit value.
    // This exists in F# too and it is written as '()'
    | Unit

and VariableContext = Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx: VariableContext) e =
    match e with
    | Constant n -> ValNum n
    | Binary(op, e1, e2) ->
        let v1 = evaluate ctx e1
        let v2 = evaluate ctx e2

        match v1, v2 with
        | ValNum n1, ValNum n2 ->
            match op with
            | "+" -> ValNum(n1 + n2)
            | "*" -> ValNum(n1 * n2)
            | _ -> failwith "unsupported binary operator"
        | _ -> failwith "invalid argument of binary operator"
    | Variable(v) ->
        match ctx.TryFind v with
        | Some res -> res.Value
        | _ -> failwith ("unbound variable: " + v)

    // NOTE: You have the following from before
    | Unary(op, e) ->
        let v = evaluate ctx e in

        match v with
        | ValNum intVal ->
            match op with
            | "-" -> ValNum(-intVal)
            | _ -> failwith "unsupported unary operator"
        | _ -> failwith "expression must evaluate to NumVal"
    | If(cond, t, f) ->
        match evaluate ctx cond with
        | ValNum(1) -> evaluate ctx t
        | ValNum(_) -> evaluate ctx f
        | _ -> failwith "Condition must evaluate to a numerical value"
    | Lambda(v, e: Expression) -> ValClosure(v, e, ctx)
    | Application(e1, e2) ->
        match evaluate ctx e1 with
        | ValClosure(var, e, capturedCtx) -> evaluate (capturedCtx.Add(var, Lazy(evaluate ctx e2))) e
        | _ -> failwith "e1 must be variable closure"
    | Let(v, e1, e2) -> evaluate ctx (Application(Lambda(v, e2), e1))
    | Tuple(e1, e2) -> ValTuple(evaluate ctx e1, evaluate ctx e2)
    | TupleGet(b, e) ->
        match evaluate ctx e with
        | ValTuple(v1, v2) ->
            match b with
            | true -> v1
            | false -> v2
        | _ -> failwith (e.ToString() + " must be a tuple")
    | Match(e, v, e1, e2) ->
        match evaluate ctx e with
        | ValCase(b, res) ->
            match b with
            | true -> evaluate (ctx.Add((v), lazy (res))) e1
            | false -> evaluate (ctx.Add((v), lazy (res))) e2
        | _ -> failwith "e must be a Case expression"
    | Case(b, e) -> ValCase(b, evaluate ctx e)

    | Recursive(v, e1, e2) ->
        // TODO: Implement recursion for 'let rec v = e1 in e2'.
        // (In reality, this will only work if 'e1' is a function
        // but the case can be implemented without assuming that).
        let rec factClosure = lazy evaluate (ctx.Add(v, factClosure)) e1
        evaluate (ctx.Add(v, factClosure)) e2

    // NOTE: This is so uninteresting I did this for you :-)
    | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2),
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we
// do not need to write them by hand!
let rec makeListExpr l =
    match l with
    | x :: xs -> Case(true, Tuple(x, makeListExpr xs))
    | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1..5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l ->
//     match l with
//     | Case1 t -> Case1(f x#1, (map f) x#2)
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em =
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
        Application(Application(Variable "map", Lambda("y", Binary("*", Variable "y", Constant 10))), el)
    )

evaluate Map.empty em

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l ->
//     match l with
//     | Case1 t ->
//          if f x#1 then Case1(x#1, (map f) x#2)
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//
let ef =
    Recursive(
        "filter",
        Lambda(
            "f",
            Lambda(
                "l",
                Match(
                    Variable("l"),
                    "x",
                    Case(
                        true,
                        If(
                            Application(Variable "f", TupleGet(true, Variable "x")),
                            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x")),
                            Tuple(
                                TupleGet(true, Variable "x"),
                                Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
                            )
                        )
                    ),
                    Case(false, Unit)
                )
            )
        ),
        Application(Application(Variable "filter", Lambda("y", Binary("+", Variable "y", Unary("-", Constant 2)))), el)
    )

evaluate Map.empty ef

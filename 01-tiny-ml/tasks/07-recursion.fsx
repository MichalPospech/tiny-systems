// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    | ValCase of bool * Value

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
    // NOTE: A recursive definition. You can think of
    // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'.
    | Recursive of string * Expression * Expression

and VariableContext =
    // NOTE: For recursive calls, we need to add the function
    // being defined to the variable context when defining it.
    // This can be done using 'let rec', but we need to store
    // the variables as lazy values.
    Map<string, Lazy<Value>>

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
        | _ -> failwith ("invalid argument of binary operator" + "\n"+v1.ToString()  + "\n" + v2.ToString())
    | Variable(v) ->
        match ctx.TryFind v with
        | Some res ->
            // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
            res.Value
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
        match e with
        | Tuple(v1, v2) ->
            match b with
            | true -> evaluate ctx v1
            | false -> evaluate ctx v2
        | _ -> failwith "e must be a tuple"
    | Match(e, v, e1, e2) ->
        match evaluate ctx e with
        | ValCase(b, res) ->
            match b with
            | true -> evaluate (ctx.Add((v), lazy(res))) e1
            | false -> evaluate (ctx.Add((v), lazy(res))) e2
        | _ -> failwith "e must be a Case expression"
    | Case(b, e) -> ValCase(b, evaluate ctx e)

    | Recursive(v, e1, e2) ->
        // TODO: Implement recursion for 'let rec v = e1 in e2'.
        // (In reality, this will only work if 'e1' is a function
        // but the case can be implemented without assuming that).
        let rec factClosure = lazy evaluate (ctx.Add(v,factClosure)) e1
        evaluate (ctx.Add(v, factClosure)) e2
// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x ->
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er =
    Recursive(
        "factorial",
        Lambda(
            "x",
            If(
                Variable("x"),
                Constant(1),
                Binary("*", Variable("x"), Application(Variable("factorial"), Binary("+", Constant(-1), Variable("x"))))
            )
        ),
        Application(Variable "factorial", Constant 5)
    )

evaluate Map.empty er

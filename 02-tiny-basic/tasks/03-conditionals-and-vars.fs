// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
    | StringValue of string
    // NOTE: Added numerical and Boolean values
    | NumberValue of int
    | BoolValue of bool

type Expression =
    | Const of Value
    // NOTE: Added functions and variables. Functions  are used for both
    // functions (later) and binary operators (in this step). We use only
    // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
    | Function of string * Expression list
    | Variable of string

type Command =
    | Print of Expression
    | Run
    | Goto of int
    // NOTE: Assign expression to a given variable and conditional that
    // runs a given Command only if the expression evaluates to 'BoolValue(true)'
    | Assign of string * Expression
    | If of Expression * Command

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue s -> printfn "%s" s
    | BoolValue b -> printfn (if b then "true" else "false")
    | NumberValue n -> printfn "%d" n

let getLine (state: State) (line: int) =
    List.exactlyOne (List.filter (fun (l, _) -> l = line) state.Program)

let addLine state (line, cmd) =
    { Program =
        state.Program
        |> List.filter (fun (l, _) -> l <> line)
        |> List.append [ (line, cmd) ]
        |> List.sortBy (fun (l, _) -> l)
      Variables = state.Variables }

let addVariable state name value =
    { Program = state.Program
      Variables = state.Variables.Add(name, value) }
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr =
    // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
    // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
    // (takes two values and returns Boolean). Note that you can test if two
    // F# values are the same using '='. It works on values of type 'Value' too.
    //
    // HINT: You will need to pass the program state to 'evalExpression'
    // in order to be able to handle variables!
    match expr with
    | Const(c) -> c
    | Variable(v) -> state.Variables.[v]
    | Function(f, exs) ->
        let evaluatedExs = List.map (evalExpression state) exs
        assert (evaluatedExs.Length = 2)

        match f with
        | "-" ->
            let numericExes =
                evaluatedExs
                |> List.map (fun e ->
                    match e with
                    | NumberValue(n) -> n
                    | _ -> failwith "e must be a numeric expression")

            assert (numericExes.Length = 2)
            NumberValue(numericExes.[0] - numericExes.[1])
        | "=" -> BoolValue(evaluatedExs.[0] = evaluatedExs.[1])
        | s -> failwithf "unknown function %s" s

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(expr) ->
        printValue (evalExpression state expr)
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        // TODO: Find the right line of the program using 'getLine' and call
        // 'runCommand' recursively on the found line to evaluate it.
        runCommand state (getLine state line)

    // TODO: Implement assignment and conditional. Assignment should run the
    // next line after setting the variable value. 'If' is a bit trickier:
    // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
    // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
    // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
    //
    // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
    // the command in the 'THEN' branch and the current line as the line number.
    | Assign(v, e) -> runNextLine (addVariable state v (evalExpression state e)) line
    | If(cond, comm) ->
        let condRes = evalExpression state cond

        match condRes with
        | BoolValue(b) ->
            if b then
                runCommand state (line, comm)
            else
                runNextLine state line
        | _ -> failwith "cond does not evaluate to boolean"

and runNextLine state line =
    let res =
        state.Program
        |> List.sortBy (fun (l, _) -> l)
        |> List.skipWhile (fun (l, _) -> l <= line)
        |> List.tryHead

    match res with
    | Some(r) -> runCommand state r
    | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    match line with
    | Some(l) -> addLine state (l, cmd)
    | None -> runCommand state (System.Int32.MaxValue, cmd)


let runInputs state cmds =
    // TODO: Apply all the specified commands to the program state using 'runInput'.
    // This is a one-liner if you use 'List.fold' which has the following type:
    //   ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
    List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; Variables = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce =
    [ Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let helloInf =
    [ Some 20, Goto 10
      Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let testVariables =
    [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n"))
      Some 20, Assign("I", Const(NumberValue 1))
      Some 30, Assign("B", Function("=", [ Variable("I"); Const(NumberValue 1) ]))
      Some 40, Print(Variable "S")
      Some 50, Print(Variable "I")
      Some 60, Print(Variable "B")
      None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function)
runInputs empty testVariables |> ignore

let helloTen =
    [ Some 10, Assign("I", Const(NumberValue 10))
      Some 20, If(Function("=", [ Variable("I"); Const(NumberValue 1) ]), Goto(60))
      Some 30, Print(Const(StringValue "HELLO WORLD\n"))
      Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
      Some 50, Goto 20
      Some 60, Print(Const(StringValue ""))
      None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore

// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value = StringValue of string

type Expression = Const of Value

type Command =
    | Print of Expression
    | Run
    | Goto of int

type State = { Program: list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue s -> printfn "%s" s


let getLine (state: State) (line: int) =
    List.exactlyOne (List.filter (fun (l, _) -> l = line) state.Program)

let addLine state (line, cmd) =
    { Program =
        state.Program
        |> List.filter (fun (l, _) -> l <> line)
        |> List.append [ (line, cmd) ]
        |> List.sortBy (fun (l, _) -> l) }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression (expr: Expression) =
    // TODO: Implement evaluation of expressions. The function should take
    // 'Expression' and return 'Value'. In this step, it is trivial :-)
    match expr with
    | Const(c) -> c

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(expr) ->
        // TODO: Evaluate the expression and print the resulting value here!
        printValue (evalExpression (expr))
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        // TODO: Find the right line of the program using 'getLine' and call
        // 'runCommand' recursively on the found line to evaluate it.
        runCommand state (getLine state line)

and runNextLine state line =
    let res =
        state.Program
        |> List.sortBy (fun (l, _) -> l)
        |> List.skipWhile (fun (l, _) -> l <= line)
        |> List.tryExactlyOne

    match res with
    | Some(r) -> runCommand state r
    | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    // TODO: Simulate what happens when the user enters a line of code in the
    // interactive terminal. If the 'line' number is 'Some ln', we want to
    // insert the line into the right location of the program (addLine); if it
    // is 'None', then we want to run it immediately. To make sure that
    // 'runCommand' does not try to run anything afterwards, you can pass
    // 'System.Int32.MaxValue' as the line number to it (or you could use -1
    // and handle that case specially in 'runNextLine')
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

let helloOnce =
    [ Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let helloInf =
    [ Some 20, Goto 10
      Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore

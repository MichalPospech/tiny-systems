// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal.
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value = StringValue of string

type Expression = Const of Value

type Command =
    | Print of Expression
    | Run
    // NOTE: GOTO specified line number. Note that this is an integer, rather
    // than an expression, so you cannot calculate line number dynamically.
    // (But there are tricks to do this by direct memory access on a real C64!)
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
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = { Program = [ 10, Print(Const(StringValue "HELLO WORLD\n")) ] }

let helloInf =
    { Program = [ 10, Print(Const(StringValue "HELLO WORLD\n")); 20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore

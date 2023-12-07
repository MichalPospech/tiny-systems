// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
    | StringValue of string
    | NumberValue of int
    | BoolValue of bool

type Expression =
    | Const of Value
    | Function of string * Expression list
    | Variable of string

type Command =
    | Print of Expression
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at
    // the console location (x, y). In C64, the actual POKE writes to a given
    // memory location, but we only use it for screen access here.
    | Clear
    | Poke of Expression * Expression * Expression

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Random: Random }

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
      Variables = state.Variables
      Random = state.Random }

let addVariable state name value =
    { Program = state.Program
      Variables = state.Variables.Add(name, value)
      Random = state.Random }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two numerical arguments"


let binaryNumOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> NumberValue(f a b)
    | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args =
    match args with
    | [ BoolValue a; BoolValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two bool arguments"

let rnd state args =
    match args with
    | [ NumberValue n ] -> NumberValue(state.Random.Next(n))
    | _ -> failwith "Expected one numeric argument"

let rec evalExpression state expr =
    // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
    // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
    // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
    // add helpers for numerical operators and binary Boolean operators to make
    // your code a bit nicer.
    match expr with
    | Const(c) -> c
    | Variable(v) -> state.Variables.[v]
    | Function(f, exs) ->
        let evaluatedExs = List.map (evalExpression state) exs

        match f with
        | "-" -> binaryNumOp (-) evaluatedExs
        | "=" -> binaryRelOp (=) evaluatedExs
        | "<" -> binaryRelOp (<) evaluatedExs
        | ">" -> binaryRelOp (>) evaluatedExs
        | "||" -> binaryBoolOp (||) evaluatedExs
        | "RND" -> rnd state evaluatedExs
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

    // TODO: Implement two commands for screen manipulation
    | Clear ->
        Console.Clear()
        runNextLine state line

    | Poke(x, y, e) ->
        match [ evalExpression state x, evalExpression state y, evalExpression state e ] with
        | [ NumberValue(x), NumberValue(y), StringValue(e) ] ->
            Console.SetCursorPosition(x, y)
            System.Console.Write(e)
            runNextLine state line
        | _ -> failwith "expected 2 numeric arguments"

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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.:
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [ a; b ])
let (.<) a b = Function("<", [ a; b ])
let (.>) a b = Function(">", [ a; b ])
let (.-) a b = Function("-", [ a; b ])
let (.=) a b = Function("=", [ a; b ])
let (@) s args = Function(s, args)

let empty =
    { Program = []
      Variables = Map.empty
      Random = new Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars =
    [ Some 10, Clear
      Some 20, Poke("RND" @ [ num System.Console.WindowWidth ], "RND" @ [ num System.Console.WindowHeight ], str "*")
      Some 30, Assign("I", num 100)
      Some 40, Poke("RND" @ [ num System.Console.WindowWidth ], "RND" @ [ num System.Console.WindowHeight ], str " ")
      Some 50, Assign("I", var "I" .- num 1)
      Some 60, If(var "I" .> num 1, Goto(40))
      Some 100, Goto(20)
      None, Run ]

// NOTE: Make the cursor invisible to get a nicer stars animation
Console.CursorVisible <- false
runInputs empty stars |> ignore

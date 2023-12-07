// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    | Clear
    | Poke of Expression * Expression * Expression
    // NOTE: Input("X") reads a number from console and assigns it to X;
    // Stop terminates the program; I also modified Print to take a list of
    // expressions instead of just one (which is what C64 supports too).
    | Print of Expression list
    | Input of string
    | Stop

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Random: System.Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------
let printValue value =
    match value with
    | StringValue s -> printfn "%s" s
    | BoolValue b -> printfn (if b then "true" else "false")
    | NumberValue n -> printfn "%d" n

let printValues values = List.map printValue values


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


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

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
        | "MIN" -> binaryNumOp (min) evaluatedExs
        | "<" -> binaryRelOp (<) evaluatedExs
        | ">" -> binaryRelOp (>) evaluatedExs
        | "||" -> binaryBoolOp (||) evaluatedExs
        | "RND" -> rnd state evaluatedExs
        | s -> failwithf "unknown function %s" s


let rec runCommand state (line, cmd) =
    match cmd with
    | Print(exprs) ->
        printValues (List.map (evalExpression state) exprs) |> ignore
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

    // TODO: Input("X") should read a number from the console using Console.RadLine
    // and parse it as a number using Int32.TryParse (retry if the input is wrong)
    // Stop terminates the execution (you can just return the 'state'.)
    | Input var ->
        let mutable succ = false
        let mutable num = ref 0

        while (not succ) do
            let s = Console.ReadLine()
            succ <- Int32.TryParse(s, num)

        runNextLine (addVariable state var (NumberValue(num.Value))) line
    | Stop -> state

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
      Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim =
    [ Some 10, Assign("M", num 20)
      Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 30,
      Print
          [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 50, Input("P")
      Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
      Some 70, Assign("M", var "M" .- var "P")
      Some 80, If(var "M" .= num 0, Goto 200)
      Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 100,
      Print
          [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 120, Input("P")
      Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
      Some 140, Assign("M", var "M" .- var "P")
      Some 150, If(var "M" .= num 0, Goto 220)
      Some 160, Goto 20
      Some 200, Print [ str "PLAYER 1 WINS!" ]
      Some 210, Stop
      Some 220, Print [ str "PLAYER 2 WINS!" ]
      Some 230, Stop
      None, Run ]

runInputs empty nim |> ignore

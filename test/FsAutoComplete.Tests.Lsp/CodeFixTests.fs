module FsAutoComplete.Tests.CodeFixTests

open Expecto
open Helpers
open System.IO
open Utils.Utils
open Utils.TextEdit
open Utils.ServerTests
open Utils.CursorbasedTests
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open Utils.Server
open Utils.CursorbasedTests.CodeFix

module private Diagnostics =
  let expectCode code (diags: Diagnostic[]) =
    Expecto.Flip.Expect.exists 
      $"There should be a Diagnostic with code %s{code}"
      (fun (d: Diagnostic) -> d.Code = Some code)
      diags
  let acceptAll = ignore

  open FsAutoComplete.Logging
  let private logger = FsAutoComplete.Logging.LogProvider.getLoggerByName "CodeFixes.Diagnostics"
  /// Usage: `(Diagnostics.log >> Diagnostics.expectCode "XXX")`
  /// Logs as `info`
  let log (diags: Diagnostic[]) =
    logger.info (
      Log.setMessage "diags({count})={diags}"
      >> Log.addContext "count" diags.Length
      >> Log.addContextDestructured "diags" diags
    )
    diags

module CodeFix =
  open FsAutoComplete.Logging
  let private logger = FsAutoComplete.Logging.LogProvider.getLoggerByName "CodeFixes.CodeFix"
  /// Usage: `(CodeFix.log >> CodeFix.withTitle "XXX")`
  /// Logs as `info`
  let log (codeActions: CodeAction[]) =
    logger.info (
      Log.setMessage "codeActions({count})={codeActions}"
      >> Log.addContext "count" codeActions.Length
      >> Log.addContextDestructured "codeActions" codeActions
    )
    codeActions

/// `ignore testCaseAsync`
/// 
/// Like `testCaseAsync`, but test gets completely ignored.
/// Unlike `ptestCaseAsync` (pending), this here doesn't even show up in Expecto summary.
/// 
/// -> Used to mark issues & shortcomings in CodeFixes, but without any (immediate) intention to fix
///    (vs. `pending` -> marked for fixing)  
/// -> ~ uncommenting tests without actual uncommenting
let itestCaseAsync name test = ()

let private addExplicitTypeToParameterTests state =
  serverTestList (nameof AddExplicitTypeToParameter) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddExplicitTypeToParameter.title
    testCaseAsync "can suggest explicit parameter for record-typed function parameters" <|
      CodeFix.check server
        """
        type Foo =
            { name: string }

        let name $0f =
            f.name
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type Foo =
            { name: string }

        let name (f: Foo) =
            f.name
        """
    testCaseAsync "can add type for int param" <|
      CodeFix.check server
        """
        let f ($0x) = x + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (x: int) = x + 1
        """
    testCaseAsync "can add type for generic param" <|
      CodeFix.check server
        """
        let f ($0x) = ()
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (x: 'a) = ()
        """
    testCaseAsync "doesn't trigger when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f ($0x: int) = ()
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to tuple item" <|
      CodeFix.check server
        """
        let f (a, $0b, c) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (a, b: int, c) = a + b + c + 1
        """
    testCaseAsync "doesn't trigger in tuple when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f (a, $0b: int, c) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to 2nd of 3 param" <|
      CodeFix.check server
        """
        let f a $0b c = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f a (b: int) c = a + b + c + 1
        """
    testCaseAsync "doesn't trigger on 2nd of 3 param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f a ($0b: int) c = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to 2nd of 3 param when other params have types" <|
      CodeFix.check server
        """
        let f (a: int) $0b (c: int) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (a: int) (b: int) (c: int) = a + b + c + 1
        """
    testCaseAsync "can add type to member param" <|
      CodeFix.check server
        """
        type A() =
          member _.F($0a) = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A() =
          member _.F(a: int) = a + 1
        """
    testCaseAsync "doesn't trigger for member param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        type A() =
          member _.F($0a: int) = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to ctor param" <|
      CodeFix.check server
        """
        type A($0a) =
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(a: int) =
          member _.F() = a + 1
        """
    testCaseAsync "doesn't trigger for ctor param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        type A($0a: int) =
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to correct ctor param" <|
      CodeFix.check server
        """
        type A(str, $0n, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = n + 1
          member _.FBool() = sprintf "b=%b" b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(str, n: int, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = n + 1
          member _.FBool() = sprintf "b=%b" b
        """
    testCaseAsync "doesn't trigger for ctor param when existing type and multiple params" <|
      CodeFix.checkNotApplicable server
        """
        type A(str, $0n: int, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = a + 1
          member _.FBool() = sprintf "b=%b" b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to secondary ctor param" <|
      CodeFix.check server
        """
        type A(a) =
          new($0a, b) = A(a+b)
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(a) =
          new(a: int, b) = A(a+b)
          member _.F() = a + 1
        """
    testList "parens" [
      testCaseAsync "single param without parens -> add parens" <|
        CodeFix.check server
          """
          let f $0x = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f (x: int) = x + 1
          """
      testCaseAsync "single param with parens -> keep parens" <|
        CodeFix.check server
          """
          let f ($0x) = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f (x: int) = x + 1
          """
      testCaseAsync "multi params without parens -> add parens" <|
        CodeFix.check server
          """
          let f a $0x y = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f a (x: int) y = x + 1
          """
      testCaseAsync "multi params with parens -> keep parens" <|
        CodeFix.check server
          """
          let f a ($0x) y = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f a (x: int) y = x + 1
          """
      testList "tuple params without parens -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f ($0x, y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x: int, y, z) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (x, $0y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y: int, z) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (x, y, $0z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y, z: int) = x + y + z + 1
            """
      ]
      testList "tuple params with parens -> keep parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (($0x), y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f ((x: int), y, z) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (x, ($0y), z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, (y: int), z) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (x, y, ($0z)) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y, (z: int)) = x + y + z + 1
            """
      ]
      testList "tuple params without parens but spaces -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (  $0x   ,   y   ,   z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x: int   ,   y   ,   z   ) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (  x   ,   $0y   ,   z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x   ,   y: int   ,   z   ) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (  x   ,   y   ,   $0z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x   ,   y   ,   z: int   ) = x + y + z + 1
            """
      ]
      testList "long tuple params without parens but spaces -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (  xV$0alue   ,   yAnotherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue: int   ,   yAnotherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (  xValue   ,   yAn$0otherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue   ,   yAnotherValue: int   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (  xValue   ,   yAnotherValue   ,   zFina$0lValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue   ,   yAnotherValue   ,   zFinalValue: int   ) = xValue + yAnotherValue + zFinalValue + 1
            """
      ]
      testCaseAsync "never add parens to primary ctor param" <|
        CodeFix.check server
          """
          type A (
            $0a
            ) =
            member _.F(b) = a + b
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          type A (
            a: int
            ) =
            member _.F(b) = a + b
          """
    ]
  ])

let private addMissingEqualsToTypeDefinitionTests state =
  serverTestList (nameof AddMissingEqualsToTypeDefinition) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddMissingEqualsToTypeDefinition.title
    testCaseAsync "can add = to record def" <|
      CodeFix.check server
        """
        type Person $0{ Name : string; Age : int; City : string }
        """
        (Diagnostics.expectCode "3360") 
        selectCodeFix
        """
        type Person = { Name : string; Age : int; City : string }
        """
    testCaseAsync "can add = to union def" <|
      CodeFix.check server
        """
        type Name $0Name of string
        """
        (Diagnostics.expectCode "3360") 
        selectCodeFix
        """
        type Name = Name of string
        """
  ])

let private addMissingFunKeywordTests state =
  serverTestList (nameof AddMissingFunKeyword) state defaultConfigDto None (fun server -> [
    testCaseAsync "can generate the fun keyword when error 10 is raised" <|
      CodeFix.check server
        """
        let doThing = x $0-> printfn "%s" x
        """
        (Diagnostics.expectCode "10")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingFunKeyword.title)
        """
        let doThing = fun x -> printfn "%s" x
        """
  ])

let private addMissingInstanceMemberTests state =
  serverTestList (nameof AddMissingInstanceMember) state defaultConfigDto None (fun server -> [
    testCaseAsync "can add this member prefix" <|
      CodeFix.check server
        """
        type C () =
          member $0Foo() = ()
        """
        (Diagnostics.expectCode "673")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingInstanceMember.title)
        """
        type C () =
          member x.Foo() = ()
        """
  ])

let private addMissingRecKeywordTests state =
  serverTestList (nameof AddMissingRecKeyword) state defaultConfigDto None (fun server -> [
    // `rec` in single function is handled in `MakeOuterBindingRecursive`
    testCaseAsync "can add rec to mutual recursive function" <|
      CodeFix.check server
        """
        $0let a x = x
        and b x = x
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "a"))
        """
        let rec a x = x
        and b x = x
        """
  ])

let private addNewKeywordToDisposableConstructorInvocationTests state =
  serverTestList (nameof AddNewKeywordToDisposableConstructorInvocation) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddNewKeywordToDisposableConstructorInvocation.title
    testCaseAsync "can add new to Disposable" <|
      CodeFix.check server
        """
        open System.Threading.Tasks
        let _ = $0Task<int>(fun _ -> 1)
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        open System.Threading.Tasks
        let _ = new Task<int>(fun _ -> 1)
        """
    testCaseAsync "can add new to Disposable with namespace" <|
      CodeFix.check server
        """
        let _ = System.Threading.Tasks.$0Task<int>(fun _ -> 1)
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let _ = new System.Threading.Tasks.Task<int>(fun _ -> 1)
        """
    testCaseAsync "doesn't trigger for not Disposable" <|
      CodeFix.checkNotApplicable server
        """
        let _ = System.$0String('.', 3)
        """
        Diagnostics.acceptAll
        selectCodeFix
  ])

let private addTypeToIndeterminateValueTests state =
  serverTestList (nameof AddTypeToIndeterminateValue) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddTypeToIndeterminateValue.title
    testCaseAsync "can add type annotation to error 72 ('Lookup on object of indeterminate type')" <|
      CodeFix.check server
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun d -> $0d.Value > 20) data
        """
        (Diagnostics.expectCode "72")
        selectCodeFix
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun (d: {| Name: string; Value: int |}) -> d.Value > 20) data
        """
    testCaseAsync "can add type annotation to error 3245 ('The input to a copy-and-update expression that creates an anonymous record must be either an anonymous record or a record')" <|
      CodeFix.check server
        """
        [1..5]
        |> List.fold
            (fun s i ->
              match i % 2 with
              | 0 -> {| $0s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """
        (Diagnostics.expectCode "3245")
        selectCodeFix
        """
        [1..5]
        |> List.fold
            (fun (s: {| Evens: int |}) i ->
              match i % 2 with
              | 0 -> {| s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """
  ])

let private changeDerefBangToValueTests state =
  serverTestList (nameof ChangeDerefBangToValue) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ChangeDerefBangToValue.title
    testCaseAsync "can replace ! with .Value" <|
      CodeFix.check server
        """
        let rv = ref 5
        let v = $0!rv
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = rv.Value
        """
    testCaseAsync "can replace ! with .Value when parens" <|
      CodeFix.check server
        """
        let rv = ref 5
        let v = $0!(rv)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = (rv).Value
        """
    testCaseAsync "can replace ! with .Value when function in parens" <|
      CodeFix.check server
        """
        let fr a = ref a
        let v = $0!(fr 5)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let fr a = ref a
        let v = (fr 5).Value
        """
    testCaseAsync "can replace ! with .Value when space between ! and variable" <|
      CodeFix.check server
        """
        let rv = ref 5
        let v = $0! rv
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = rv.Value
        """
    testCaseAsync "can replace ! with .Value when when parens and space between ! and variable" <|
      CodeFix.check server
        """
        let rv = ref 5
        let v = $0! (rv)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = (rv).Value
        """
  ])

let private changeDowncastToUpcastTests state = 
  serverTestList (nameof ChangeDowncastToUpcast) state defaultConfigDto None (fun server -> [
    let selectOperatorCodeFix = CodeFix.withTitle ChangeDowncastToUpcast.titleUpcastOperator
    let selectFunctionCodeFix = CodeFix.withTitle ChangeDowncastToUpcast.titleUpcastFunction
    testCaseAsync "can change :?> to :>" <|
      CodeFix.check server
        """
        type I = interface end
        type C() = interface I

        let v: I = C() $0:?> I
        """
        (Diagnostics.expectCode "3198") 
        selectOperatorCodeFix
        """
        type I = interface end
        type C() = interface I

        let v: I = C() :> I
        """
    testCaseAsync "can change downcast to upcast" <|
      CodeFix.check server
        """
        type I = interface end
        type C() = interface I

        let v: I = $0downcast C()
        """
        (Diagnostics.expectCode "3198") 
        selectFunctionCodeFix
        """
        type I = interface end
        type C() = interface I

        let v: I = upcast C()
        """
    ()
  ])

let private changeEqualsInFieldTypeToColonTests state = 
  serverTestList (nameof ChangeEqualsInFieldTypeToColon) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ChangeEqualsInFieldTypeToColon.title
    testCaseAsync "can change = to : in single line" <|
      CodeFix.check server
        """
        type A = { Name : string; Key $0= int }
        """
        (Diagnostics.expectCode "10") 
        selectCodeFix
        """
        type A = { Name : string; Key : int }
        """
    testCaseAsync "can change = to : in multi line" <|
      CodeFix.check server
        """
        type A = { 
          Name : string
          Key $0= int 
        }
        """
        (Diagnostics.expectCode "10") 
        selectCodeFix
        """
        type A = { 
          Name : string
          Key : int 
        }
        """
  ])

let private changePrefixNegationToInfixSubtractionTests state =
  serverTestList (nameof ChangePrefixNegationToInfixSubtraction) state defaultConfigDto None (fun server -> [
    testCaseAsync "converts negation to subtraction" <|
      CodeFix.check server
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. $0l -1 ]
        """
        (Diagnostics.expectCode "3")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle ChangePrefixNegationToInfixSubtraction.title)
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. l - 1 ]
        """
  ])

let private changeRefCellDerefToNotTests state = 
  serverTestList (nameof ChangeRefCellDerefToNot) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ChangeRefCellDerefToNot.title
    testCaseAsync "can change simple deref to not" <|
      CodeFix.check server
        """
        let x = 1
        !$0x
        """
        (Diagnostics.expectCode "1") 
        selectCodeFix
        """
        let x = 1
        not x
        """
    testCaseAsync "can change simple deref with parens to not" <|
      CodeFix.check server
        """
        let x = 1
        !($0x)
        """
        (Diagnostics.expectCode "1") 
        selectCodeFix
        """
        let x = 1
        not (x)
        """
    testCaseAsync "can change deref of binary expr to not" <|
      CodeFix.check server
        """
        let x = 1
        !($0x = false)
        """
        (Diagnostics.expectCode "1") 
        selectCodeFix
        """
        let x = 1
        not (x = false)
        """
  ])

let private changeTypeOfNameToNameOfTests state =
  serverTestList (nameof ChangeTypeOfNameToNameOf) state defaultConfigDto None (fun server -> [
    testCaseAsync "can suggest fix" <|
      CodeFix.check server
        """
        let x = $0typeof<Async<string>>.Name
        """
        (Diagnostics.acceptAll)
        (CodeFix.ofKind "refactor" >> CodeFix.withTitle ChangeTypeOfNameToNameOf.title)
        """
        let x = nameof(Async<string>)
        """
  ])

let private convertBangEqualsToInequalityTests state = 
  serverTestList (nameof ConvertBangEqualsToInequality) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertBangEqualsToInequality.title
    testCaseAsync "can change != to <>" <|
      CodeFix.check server
        """
        1 $0!= 2
        """
        (Diagnostics.expectCode "43") 
        selectCodeFix
        """
        1 <> 2
        """
  ])

let private convertCSharpLambdaToFSharpLambdaTests state =
  serverTestList (nameof ConvertCSharpLambdaToFSharpLambda) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertCSharpLambdaToFSharpLambda.title
    testCaseAsync "can convert csharp lambda in variable assignment with cursor on input" <|
      CodeFix.check server
        """
        let x = $0y => 1 + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
    testCaseAsync "can convert csharp lambda in variable assignment with cursor on usage" <|
      CodeFix.check server
        """
        let x = y => 1 + $0y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
    //ENHANCEMENT: trigger on `=>`
    itestCaseAsync "can convert csharp lambda in variable assignment with cursor on =>" <|
      CodeFix.check server
        """
        let x = y $0=> 1 + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
    testCaseAsync "can convert csharp lambda in lambda with parens with cursor on input" <|
      CodeFix.check server
        """
        [1..10] |> List.map ($0x => 1 + x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """
    testCaseAsync "can convert csharp lambda in lambda with parens with cursor on usage" <|
      CodeFix.check server
        """
        [1..10] |> List.map (x => 1 + $0x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """
    testCaseAsync "keep multi-line lambda intact - cursor on input" <|
      CodeFix.check server
        """
        let x =
          $0y =>
            let a = 1 + y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """
    testCaseAsync "keep multi-line lambda intact - cursor on usage" <|
      CodeFix.check server
        """
        let x =
          y =>
            let a = 1 + $0y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """
  ])

let private convertDoubleEqualsToSingleEqualsTests state = 
  serverTestList (nameof ConvertDoubleEqualsToSingleEquals) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertDoubleEqualsToSingleEquals.title
    testCaseAsync "can replace == with =" <|
      CodeFix.check server
        """
        1 $0== 1
        """
        (Diagnostics.expectCode "43") 
        selectCodeFix
        """
        1 = 1
        """
    testCaseAsync "doesn't replace existing operator == with =" <|
      CodeFix.checkNotApplicable server
        """
        let (==) a b = a = b
        1 $0== 1
        """
        Diagnostics.acceptAll
        selectCodeFix
  ])
 
let private convertInvalidRecordToAnonRecordTests state = 
  serverTestList (nameof ConvertInvalidRecordToAnonRecord) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertInvalidRecordToAnonRecord.title
    testCaseAsync "can convert single-line record with single field" <|
      CodeFix.check server
        """
        let v = { $0Name = "foo" }
        """
        (Diagnostics.expectCode "39") 
        selectCodeFix
        """
        let v = {| Name = "foo" |}
        """
    testCaseAsync "can convert single-line record with two fields" <|
      CodeFix.check server
        """
        let v = { $0Name = "foo"; Value = 42 }
        """
        (Diagnostics.expectCode "39") 
        selectCodeFix
        """
        let v = {| Name = "foo"; Value = 42 |}
        """
    testCaseAsync "can convert multi-line record with two fields" <|
      CodeFix.check server
        """
        let v = {
          $0Name = "foo"
          Value = 42
        }
        """
        (Diagnostics.expectCode "39") 
        selectCodeFix
        """
        let v = {|
          Name = "foo"
          Value = 42
        |}
        """
    testCaseAsync "doesn't trigger for existing record" <|
      CodeFix.checkNotApplicable server
        """
        type V = { Name: string; Value: int }
        let v = { $0Name = "foo"; Value = 42 }
        """
        (Diagnostics.acceptAll) 
        selectCodeFix
    testCaseAsync "doesn't trigger for anon record" <|
      CodeFix.checkNotApplicable server
        """
        let v = {| $0Name = "foo"; Value = 42 |}
        """
        (Diagnostics.acceptAll) 
        selectCodeFix
  ])

let private convertPositionalDUToNamedTests state =
  serverTestList (nameof ConvertPositionalDUToNamed) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertPositionalDUToNamed.title
    testCaseAsync "in parenthesized let binding" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        let (A(a$0, b)) = A(1, true)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        let (A(a = a; b = b;)) = A(1, true)
        """
    testCaseAsync "in simple match" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(a$0, b) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(a = a; b = b;) -> ()
        """
    testCaseAsync "in parenthesized match" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(a$0, b)) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(a = a; b = b;)) -> ()
        """
    testCaseAsync "when there is one new field on the DU" <|
      CodeFix.check server
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded($0a, b)) = ThirdFieldWasJustAdded(1, true, 'c')
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded(a = a; b = b; c = _;)) = ThirdFieldWasJustAdded(1, true, 'c')
        """
    testCaseAsync "when there are multiple new fields on the DU" <|
      CodeFix.check server
        """
        type U = U of aValue: int * boolean: int * char: char * dec: decimal * element: int
        let (U($0a, b)) = failwith "..."
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type U = U of aValue: int * boolean: int * char: char * dec: decimal * element: int
        let (U(aValue = a; boolean = b; char = _; dec = _; element = _;)) = failwith "..."
        """
  ])

let private generateAbstractClassStubTests state = 
  let config = { defaultConfigDto with AbstractClassStubGeneration = Some true }
  serverTestList (nameof GenerateAbstractClassStub) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle GenerateAbstractClassStub.title
    testCaseAsync "can generate a derivative of a long ident - System.IO.Stream" <|
      CodeFix.checkApplicable server
        """
        type My$0Stream() =
          inherit System.IO.Stream()
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
    testCaseAsync "can generate a derivative for a simple ident - Stream" <|
      CodeFix.checkApplicable server
        """
        open System.IO
        type My$0Stream2() =
          inherit Stream()
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
    ptestCaseAsync "can generate abstract class stub" <|
      // issue: Wants to insert text in line 13, column 12.
      //        But Line 13 (line with `"""`) is empty -> no column 12
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        """
    ptestCaseAsync "can generate abstract class stub without trailing nl" <|
      // issue: Wants to insert text in line 13, column 12.
      //        But there's no line 13 (last line is line 12)
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        """
    ptestCaseAsync "inserts override in correct place" <|
      // issue: inserts overrides after `let a = ...`, not before
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        let a = 0
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        let a = 0
        """
    ptestCaseAsync "can generate abstract class stub with existing override" <|
      // issue: Generates override for already existing member
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Name = "Circle"

          override this.Area: float = 
              failwith "Not Implemented"
        """
  ])

let private generateRecordStubTests state =
  let config = 
    { defaultConfigDto with 
        RecordStubGeneration = Some true 
        RecordStubGenerationBody = Some "failwith \"---\""
    }
  serverTestList (nameof GenerateRecordStub) state config None (fun server -> [
    CodeFix.testAllPositions "can generate record stubs for every pos in the record as soon as one field is known"
      server
      """
      type R = { a: string; b: int }

      let a = $0{  $0a = $0"";$0  }$0
      """
      (Diagnostics.expectCode "764")
      (CodeFix.withTitle GenerateRecordStub.title)
      """
      type R = { a: string; b: int }

      let a = {  a = "";
                 b = failwith "---"  }
      """
  ])

let private generateUnionCasesTests state =
  let config = 
    { defaultConfigDto with 
        UnionCaseStubGeneration = Some true 
        UnionCaseStubGenerationBody = Some "failwith \"---\""
    }
  serverTestList (nameof GenerateUnionCases) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle GenerateUnionCases.title
    testCaseAsync "can generate match cases for a simple DU" <|
      CodeFix.check server
        """
        type Letter = A | B | C

        let char = A

        match $0char with
        | A -> ()
        """
        (Diagnostics.expectCode "25")
        (CodeFix.withTitle GenerateUnionCases.title)
        """
        type Letter = A | B | C

        let char = A

        match char with
        | A -> ()
        | B -> failwith "---"
        | C -> failwith "---"
        """
  ])

let private makeDeclarationMutableTests state =
  serverTestList (nameof MakeDeclarationMutable) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle MakeDeclarationMutable.title
    testCaseAsync "can make decl mutable in top level assignment" <|
      CodeFix.check server
        """
        let x = 0
        x $0<- 1
        """
        (Diagnostics.expectCode "27") 
        selectCodeFix
        """
        let mutable x = 0
        x <- 1
        """
    testCaseAsync "can make decl mutable in nested assignment" <|
      CodeFix.check server
        """
        let x = 0
        let _ =
          x $0<- 1
          ()
        """
        (Diagnostics.expectCode "27") 
        selectCodeFix
        """
        let mutable x = 0
        let _ =
          x <- 1
          ()
        """
    testCaseAsync "can make decl mutable in function" <|
      CodeFix.check server
        """
        let count xs =
          let counter = 0
          for x in xs do
            counter $0<- counter + 1
          counter
        """
        (Diagnostics.expectCode "27") 
        selectCodeFix
        """
        let count xs =
          let mutable counter = 0
          for x in xs do
            counter <- counter + 1
          counter
        """
    testCaseAsync "doesn't trigger for already mutable variable" <|
      CodeFix.checkNotApplicable server
        """
        let mutable x = 0
        x $0<- 1
        """
        Diagnostics.acceptAll 
        selectCodeFix
    testCaseAsync "doesn't trigger for immutable parameter" <|
      CodeFix.checkNotApplicable server
        """
        let f (v: int) =
          v $0<- 1
          v
        """
        Diagnostics.acceptAll 
        selectCodeFix
    testCaseAsync "doesn't trigger for immutable member parameter" <|
      CodeFix.checkNotApplicable server
        """
        type C() =
          member _.M(v: int)
            v $0<- 1
        """
        Diagnostics.acceptAll 
        selectCodeFix
  ])

let private makeOuterBindingRecursiveTests state =
  serverTestList (nameof MakeOuterBindingRecursive) state defaultConfigDto None (fun server -> [
    testCaseAsync "can make the outer binding recursive when self-referential" <|
      CodeFix.check server
        """
        let mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                $0mySum tail (acc + 1)
        """
        (Diagnostics.expectCode "39")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle MakeOuterBindingRecursive.title)
        """
        let rec mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                mySum tail (acc + 1)
        """
  ])

let private removeRedundantQualifierTests state =
  let config = { defaultConfigDto with SimplifyNameAnalyzer = Some true }
  serverTestList (nameof RemoveRedundantQualifier) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle RemoveRedundantQualifier.title
    testCaseAsync "can remove redundant namespace" <|
      CodeFix.check server
        """
        open System
        let _ = $0System.String.IsNullOrWhiteSpace "foo"
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        open System
        let _ = String.IsNullOrWhiteSpace "foo"
        """
    testCaseAsync "doesn't remove necessary namespace" <|
      CodeFix.checkNotApplicable server
        """
        let _ = $0System.String.IsNullOrWhiteSpace "foo"
        """
        Diagnostics.acceptAll 
        selectCodeFix
  ])

let private removeUnnecessaryReturnOrYieldTests state =
  serverTestList (nameof RemoveUnnecessaryReturnOrYield) state defaultConfigDto None (fun server -> [
    testCaseAsync "can remove return" <|
      CodeFix.check server
        """
        let f x =
          $0return x
        """
        (Diagnostics.expectCode "748")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "return"))
        """
        let f x =
          x
        """
    testCaseAsync "can remove return!" <|
      CodeFix.check server
        """
        let f x =
          $0return! x
        """
        (Diagnostics.expectCode "748")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "return!"))
        """
        let f x =
          x
        """
    testCaseAsync "can remove yield" <|
      CodeFix.check server
        """
        let f x =
          $0yield x
        """
        (Diagnostics.expectCode "747")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield"))
        """
        let f x =
          x
        """
    testCaseAsync "can remove yield!" <|
      CodeFix.check server
        """
        let f x =
          $0yield! x
        """
        (Diagnostics.expectCode "747")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield!"))
        """
        let f x =
          x
        """
    testCaseAsync "doesn't trigger in seq" <|
      CodeFix.checkNotApplicable server
        """
        let f x = seq {
          $0yield x
        }
        """
        (Diagnostics.acceptAll)
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield"))
  ])

let private removeUnusedBindingTests state =
  let config = { defaultConfigDto with FSIExtraParameters = Some [| "--warnon:1182" |] }
  serverTestList (nameof RemoveUnusedBinding) state config None (fun server -> [
    let selectRemoveUnusedBinding = CodeFix.withTitle RemoveUnusedBinding.titleBinding
    let selectRemoveUnusedParameter = CodeFix.withTitle RemoveUnusedBinding.titleParameter
    let validateDiags = Diagnostics.expectCode "1182"

    testCaseAsync "can remove unused single character function parameter" <|
      CodeFix.check server
        """
        let incr $0i x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """
    testCaseAsync "can remove unused single character function parameter in parens" <|
      CodeFix.check server
        """
        let incr ($0i) x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """
    testCaseAsync "can remove unused binding inside top level" <|
      //ENHANCEMENT: remove empty line
      CodeFix.check server
        """
        let container () =
          let $0incr x = 2
          ()
        """
        validateDiags
        selectRemoveUnusedBinding
        """
        let container () =
          
          ()
        """
  ])

let private removeUnusedOpensTests state =
  let config = { defaultConfigDto with UnusedOpensAnalyzer = Some true }
  serverTestList (nameof RemoveUnusedOpens) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle RemoveUnusedOpens.title
    testCaseAsync "can remove single unused open" <|
      CodeFix.check server
        """
        open $0System
        """
        Diagnostics.acceptAll 
        selectCodeFix
        ""
    testCaseAsync "removes just current unused open" <|
      // unlike VS, `RemoveUnusedOpens` removes just current open (with cursor) and not all unused opens
      CodeFix.check server
        """
        open $0System
        open System.Text
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        open System.Text
        """
    testCaseAsync "removes just current unused open 2" <|
      CodeFix.check server
        """
        open System
        open $0System.Text
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        open System
        """
    testCaseAsync "doesn't remove used open" <|
      CodeFix.checkNotApplicable server
        """
        open $0System

        let _ = String.IsNullOrWhiteSpace ""
        """
        Diagnostics.acceptAll 
        selectCodeFix
    testCaseAsync "can remove open in nested module" <|
      CodeFix.check server
        """
        module A =
          module B =
            open $0System
            ()
          ()
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        module A =
          module B =
            ()
          ()
        """
    testCaseAsync "can remove used open in nested module when outer scope opens same open" <|
      CodeFix.check server
        """
        open System
        module A =
          module B =
            open $0System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        open System
        module A =
          module B =
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
    //ENHANCEMENT: detect open in outer scope as unused too
    itestCaseAsync "can remove used open in outer scope when usage in nested scope has own open" <|
      CodeFix.check server
        """
        open $0System
        module A =
          module B =
            open System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
        Diagnostics.acceptAll 
        selectCodeFix
        """
        module A =
          module B =
            open System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
    testCaseAsync "doesn't trigger for used open" <|
      CodeFix.checkNotApplicable server
        """
        open $0System
        let x = String.IsNullOrWhiteSpace ""
        """
        Diagnostics.acceptAll 
        selectCodeFix
  ])

let private renameParamToMatchSignatureTests state =
  let selectCodeFix expectedName = CodeFix.withTitle (RenameParamToMatchSignature.title expectedName)

  // requires `fsi` and corresponding `fs` file (and a project!)
  // -> cannot use untitled doc
  // -> use existing files, but load with text specified in tests
  let path = Path.Combine(__SOURCE_DIRECTORY__, @"./TestCases/CodeFixTests/RenameParamToMatchSignature/")
  let (fsiFile, fsFile) = ("Code.fsi", "Code.fs")
  let (fsiPath, fsPath) = (Path.Combine(path, fsiFile), Path.Combine(path, fsFile))

  serverTestList (nameof RenameParamToMatchSignature) state defaultConfigDto (Some path) (fun server -> [
    let checkWithFsi
      fsiSource
      fsSourceWithCursor
      selectCodeFix
      fsSourceExpected
      = async {
        let fsiSource = fsiSource |> Text.trimTripleQuotation
        let (cursor, fsSource) = 
          fsSourceWithCursor
          |> Text.trimTripleQuotation
          |> Cursor.assertExtractRange
        let! (fsiDoc, diags) = server |> Server.openDocumentWithText fsiFile fsiSource
        use fsiDoc = fsiDoc
        Expect.isEmpty diags "There should be no diagnostics in fsi doc"
        let! (fsDoc, diags) = server |> Server.openDocumentWithText fsFile fsSource
        use fsDoc = fsDoc

        do! 
          checkFixAt
            (fsDoc, diags)
            (fsSource, cursor)
            (Diagnostics.expectCode "3218")
            selectCodeFix
            (After (fsSourceExpected |> Text.trimTripleQuotation))
      }

    testCaseAsync "can rename parameter in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with backticks in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: ``my value``: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "``my value``")
        """
        module Code

        let f ``my value`` = ``my value`` + 1
        """
    testCaseAsync "can rename parameter with backticks in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f ``$0my value`` = ``my value`` + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename all usage in F# function" <|
      checkWithFsi
        """
        module Code

        val f: x: int -> value: int -> y: int -> int
        """
        """
        module Code

        let f x $0v y = 
          let a = v + 1
          let b = v * v
          let v = a + b
          v + x * y
        """
        (selectCodeFix "value")
        """
        module Code

        let f x value y = 
          let a = value + 1
          let b = value * value
          let v = a + b
          v + x * y
        """
    testCaseAsync "can rename parameter with type in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f ($0v: int) = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f (value: int) = value + 1
        """
    testCaseAsync "can rename parameter in constructor" <|
      checkWithFsi
        """
        module Code

        type T =
          new: value: int -> T
        """
        """
        module Code

        type T($0v: int) =
          let _ = v + 3
        """
        (selectCodeFix "value")
        """
        module Code

        type T(value: int) =
          let _ = value + 3
        """
    testCaseAsync "can rename parameter in member" <|
      checkWithFsi
        """
        module Code

        type T =
          new: unit -> T
          member F: value: int -> int
        """
        """
        module Code

        type T() =
          member _.F($0v) = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        type T() =
          member _.F(value) = value + 1
        """
    testCaseAsync "can rename parameter with ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value': int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value'")
        """
        module Code

        let f value' = value' + 1
        """
    testCaseAsync "can rename parameter with ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v' = v' + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: v'2: int -> int
        """
        """
        module Code

        let f $0value = value + 1
        """
        (selectCodeFix "v'2")
        """
        module Code

        let f v'2 = v'2 + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v'2 = v'2 + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with multiple ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value'v'2: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value'v'2")
        """
        module Code

        let f value'v'2 = value'v'2 + 1
        """
    testCaseAsync "can rename parameter with multiple ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0value'v'2 = value'v'2 + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    itestCaseAsync "can handle `' and implementation '` in impl name" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    //ENHANCEMENT: correctly detect below. Currently: detects sig name `sig`
    itestCaseAsync "can handle `' and implementation '` in sig name" <|
      checkWithFsi
        """
        module Code

        val f: ``sig' and implementation 'impl' do not match``: int -> int
        """
        """
        module Code

        let f $0value = value + 1
        """
        (selectCodeFix "``sig' and implementation 'impl' do not match``")
        """
        module Code

        let f ``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
  ])

let private renameUnusedValue state =
  let config = { defaultConfigDto with UnusedDeclarationsAnalyzer = Some true }
  serverTestList (nameof RenameUnusedValue) state config None (fun server -> [
    let selectReplace = CodeFix.ofKind "refactor" >> CodeFix.withTitle RenameUnusedValue.titleReplace
    let selectPrefix = CodeFix.ofKind "refactor" >> CodeFix.withTitle RenameUnusedValue.titlePrefix

    testCaseAsync "can replace unused self-reference" <|
      CodeFix.check server
        """
        type MyClass() =
          member $0this.DoAThing() = ()
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        type MyClass() =
          member _.DoAThing() = ()
        """
    testCaseAsync "can replace unused binding" <|
      CodeFix.check server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let _ = 6
        """
    testCaseAsync "can prefix unused binding" <|
      CodeFix.check server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectPrefix
        """
        let _six = 6
        """
    testCaseAsync "can replace unused parameter" <|
      CodeFix.check server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let add one two _ = one + two
        """
    testCaseAsync "can prefix unused parameter" <|
      CodeFix.check server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> selectPrefix)
        """
        let add one two _three = one + two
        """
  ])

let private replaceWithSuggestionTests state =
  serverTestList (nameof ReplaceWithSuggestion) state defaultConfigDto None (fun server -> [
    let selectCodeFix replacement = CodeFix.withTitle (ReplaceWithSuggestion.title replacement)
    testCaseAsync "can change Min to min" <|
      CodeFix.check server
        """
        let x = $0Min(2.0, 1.0)
        """
        Diagnostics.acceptAll 
        (selectCodeFix "min")
        """
        let x = min(2.0, 1.0)
        """
    testSequenced <| testList "can get multiple suggestions for flout" [
      testCaseAsync "can change flout to float" <|
        CodeFix.check server
          """
          let x = $0flout 2
          """
          Diagnostics.acceptAll 
          (selectCodeFix "float")
          """
          let x = float 2
          """
      testCaseAsync "can change flout to float32" <|
        CodeFix.check server
          """
          let x = $0flout 2
          """
          Diagnostics.acceptAll 
          (selectCodeFix "float32")
          """
          let x = float32 2
          """
    ]
    testCaseAsync "can change flout to float in var type" <|
      CodeFix.check server
        """
        let x: $0flout = 2.0
        """
        Diagnostics.acceptAll 
        (selectCodeFix "float")
        """
        let x: float = 2.0
        """
    testCaseAsync "can change namespace in open" <|
      CodeFix.check server
        """
        open System.Text.$0RegularEcpressions
        """
        Diagnostics.acceptAll 
        (selectCodeFix "RegularExpressions")
        """
        open System.Text.RegularExpressions
        """
    testCaseAsync "can change type in type constructor" <|
      CodeFix.check server
        """
        open System.Text.RegularExpressions
        let x = $0Regec()
        """
        Diagnostics.acceptAll 
        (selectCodeFix "Regex")
        """
        open System.Text.RegularExpressions
        let x = Regex()
        """
    testCaseAsync "can replace identifier in double-backticks" <|
      CodeFix.check server
        """
        let ``hello world`` = 2
        let x = ``$0hello word``
        """
        Diagnostics.acceptAll 
        (selectCodeFix "``hello world``")
        """
        let ``hello world`` = 2
        let x = ``hello world``
        """
    testCaseAsync "can add double-backticks" <|
      CodeFix.check server
        """
        let ``hello world`` = 2
        let x = $0helloword
        """
        Diagnostics.acceptAll 
        (selectCodeFix "``hello world``")
        """
        let ``hello world`` = 2
        let x = ``hello world``
        """
  ])

let private resolveNamespaceTests state =
  let config = { defaultConfigDto with ResolveNamespaces = Some true }
  serverTestList (nameof ResolveNamespace) state config None (fun server -> [
    testCaseAsync "doesn't fail when target not in last line" <| 
      CodeFix.checkApplicable server
        """
        let x = $0Min(2.0, 1.0)
        """   // Note: new line at end!
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> CodeFix.matching (fun ca -> ca.Title.StartsWith "open") >> Array.take 1)
    testCaseAsync "doesn't fail when target in last line" <| 
      CodeFix.checkApplicable server
        "let x = $0Min(2.0, 1.0)"   // Note: No new line at end!
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> CodeFix.matching (fun ca -> ca.Title.StartsWith "open") >> Array.take 1)

    //TODO: Implement & unify with `Completion.AutoOpen` (`CompletionTests.fs`)
    // Issues:
    // * Complex because of nesting modules (-> where to open)
    // * Different open locations of CodeFix and AutoOpen
  ])

let private useMutationWhenValueIsMutableTests state =
  serverTestList (nameof UseMutationWhenValueIsMutable) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle UseMutationWhenValueIsMutable.title
    testCaseAsync "can replace = with <- when cursor on =" <|
      CodeFix.check server
        """
        let _ =
          let mutable v = 42
          v $0= 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """
    testCaseAsync "can replace = with <- when cursor on variable" <|
      CodeFix.check server
        """
        let _ =
          let mutable v = 42
          $0v = 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """
    testCaseAsync "doesn't suggest fix when = is comparison" <|
      CodeFix.checkNotApplicable server
        """
        let _ =
          let mutable v = 42
          v $0= 5
        """
        Diagnostics.acceptAll
        selectCodeFix
    testCaseAsync "doesn't suggest fix when variable is not mutable" <|
      CodeFix.checkNotApplicable server
        """
        let _ =
          let v = 42
          v $0= 5
          v
        """
        Diagnostics.acceptAll
        selectCodeFix
  ])

let private useTripleQuotedInterpolationTests state =
  serverTestList (nameof UseTripleQuotedInterpolation) state defaultConfigDto None (fun server -> [
    testCaseAsync "converts erroring single-quoted interpolation to triple-quoted" <|
      CodeFix.check server
        """
        let a = $":^) {if true then $0"y" else "n"} d"
        """
        (Diagnostics.expectCode "3373")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle UseTripleQuotedInterpolation.title)
        // cannot use triple quotes string here: ends with `"""` -> cannot use in string
        @"
        let a = $"""""":^) {if true then ""y"" else ""n""} d""""""
        "
  ])

let private wrapExpressionInParenthesesTests state =
  serverTestList (nameof WrapExpressionInParentheses) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle WrapExpressionInParentheses.title
    testCaseAsync "can add parenthesize expression" <|
      CodeFix.check server
        """
        printfn "%b" System.String.$0IsNullOrWhiteSpace("foo")
        """
        (Diagnostics.expectCode "597") 
        selectCodeFix
        """
        printfn "%b" (System.String.IsNullOrWhiteSpace("foo"))
        """
    testCaseAsync "doesn't trigger for expression in parens" <|
      CodeFix.checkNotApplicable server
        """
        printfn "%b" (System.String.$0IsNullOrWhiteSpace("foo"))
        """
        Diagnostics.acceptAll 
        selectCodeFix
  ])

/// Helper functions for CodeFixes
module private CodeFixHelpers =
  // `src\FsAutoComplete\CodeFixes.fs` -> `FsAutoComplete.CodeFix`
  open Navigation
  open FSharp.Compiler.Text

  let private navigationTests =
    testList (nameof Navigation) [
      let extractTwoCursors text =
        let (text, poss) = Cursors.extract text
        let text = SourceText.ofString text
        (text, (poss[0], poss[1]))

      testList (nameof tryEndOfPrevLine) [
        testCase "can get end of prev line when not border line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5$0
let $0x = 5
let y = 7
let z = 4"""
          let (text, (expected, current)) = text |> extractTwoCursors
          let actual = tryEndOfPrevLine text current.Line
          Expect.equal actual (Some expected) "Incorrect pos"

        testCase "can get end of prev line when last line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7$0
let z$0 = 4"""
          let (text, (expected, current)) = text |> extractTwoCursors
          let actual = tryEndOfPrevLine text current.Line
          Expect.equal actual (Some expected) "Incorrect pos"

        testCase "cannot get end of prev line when first line" <| fun _ ->
          let text = """let $0foo$0 = 4
let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
          let (text, (_, current)) = text |> extractTwoCursors
          let actual = tryEndOfPrevLine text current.Line
          Expect.isNone actual "No prev line in first line"

        testCase "cannot get end of prev line when single line" <| fun _ ->
          let text = SourceText.ofString "let foo = 4"
          let line = 0
          let actual = tryEndOfPrevLine text line
          Expect.isNone actual "No prev line in first line"
      ]
      testList (nameof tryStartOfNextLine) [
        // this would be WAY easier by just using `{ Line = current.Line + 1; Character = 0 }`...
        testCase "can get start of next line when not border line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5
let $0x = 5
$0let y = 7
let z = 4"""
          let (text, (current, expected)) = text |> extractTwoCursors
          let actual = tryStartOfNextLine text current.Line
          Expect.equal actual (Some expected) "Incorrect pos"

        testCase "can get start of next line when first line" <| fun _ ->
          let text = """let $0foo = 4
$0let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
          let (text, (current, expected)) = text |> extractTwoCursors
          let actual = tryStartOfNextLine text current.Line
          Expect.equal actual (Some expected) "Incorrect pos"

        testCase "cannot get start of next line when last line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7
let $0z$0 = 4"""
          let (text, (current, _)) = text |> extractTwoCursors
          let actual = tryStartOfNextLine text current.Line
          Expect.isNone actual "No next line in last line"

        testCase "cannot get start of next line when single line" <| fun _ ->
          let text = SourceText.ofString "let foo = 4"
          let line = 0
          let actual = tryStartOfNextLine text line
          Expect.isNone actual "No next line in first line"
      ]
      testList (nameof rangeToDeleteFullLine) [
        testCase "can get all range for single line" <| fun _ ->
          let text = "$0let foo = 4$0"
          let (text, (start, fin)) = text |> extractTwoCursors
          let expected = { Start = start; End = fin }

          let line = fin.Line
          let actual = text |> rangeToDeleteFullLine line
          Expect.equal actual expected "Incorrect range"

        testCase "can get line range with leading linebreak in not border line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5$0
let x = 5$0
let y = 7
let z = 4"""
          let (text, (start, fin)) = text |> extractTwoCursors
          let expected = { Start = start; End = fin }

          let line = fin.Line
          let actual = text |> rangeToDeleteFullLine line
          Expect.equal actual expected "Incorrect range"

        testCase "can get line range with leading linebreak in last line" <| fun _ ->
          let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7$0
let z = 4$0"""
          let (text, (start, fin)) = text |> extractTwoCursors
          let expected = { Start = start; End = fin }

          let line = fin.Line
          let actual = text |> rangeToDeleteFullLine line
          Expect.equal actual expected "Incorrect range"

        testCase "can get line range with trailing linebreak in first line" <| fun _ ->
          let text = """$0let foo = 4
$0let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
          let (text, (start, fin)) = text |> extractTwoCursors
          let expected = { Start = start; End = fin }

          let line = start.Line
          let actual = text |> rangeToDeleteFullLine line
          Expect.equal actual expected "Incorrect range"
          
        testCase "can get all range for single empty line" <| fun _ ->
          let text = SourceText.ofString ""
          let pos = { Line = 0; Character = 0 }
          let expected = { Start = pos; End = pos }

          let line = pos.Line
          let actual = text |> rangeToDeleteFullLine line
          Expect.equal actual expected "Incorrect range"
      ]
    ]

  let tests = testList ($"{nameof FsAutoComplete}.{nameof FsAutoComplete.CodeFix}") [
    navigationTests
  ]

let tests state = testList "CodeFix tests" [
  CodeFixHelpers.tests

  addExplicitTypeToParameterTests state
  addMissingEqualsToTypeDefinitionTests state
  addMissingFunKeywordTests state
  addMissingInstanceMemberTests state
  addMissingRecKeywordTests state
  addNewKeywordToDisposableConstructorInvocationTests state
  addTypeToIndeterminateValueTests state
  changeDerefBangToValueTests state
  changeDowncastToUpcastTests state
  changeEqualsInFieldTypeToColonTests state
  changePrefixNegationToInfixSubtractionTests state
  changeRefCellDerefToNotTests state
  changeTypeOfNameToNameOfTests state
  convertBangEqualsToInequalityTests state
  convertCSharpLambdaToFSharpLambdaTests state
  convertDoubleEqualsToSingleEqualsTests state
  convertInvalidRecordToAnonRecordTests state
  convertPositionalDUToNamedTests state
  generateAbstractClassStubTests state
  generateRecordStubTests state
  generateUnionCasesTests state
  makeDeclarationMutableTests state
  makeOuterBindingRecursiveTests state
  removeRedundantQualifierTests state
  removeUnnecessaryReturnOrYieldTests state
  removeUnusedBindingTests state
  removeUnusedOpensTests state
  renameParamToMatchSignatureTests state
  renameUnusedValue state
  replaceWithSuggestionTests state
  resolveNamespaceTests state
  useMutationWhenValueIsMutableTests state
  useTripleQuotedInterpolationTests state
  wrapExpressionInParenthesesTests state
]

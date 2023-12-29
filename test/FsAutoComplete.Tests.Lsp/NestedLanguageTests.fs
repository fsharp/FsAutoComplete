module FsAutoComplete.Tests.NestedLanguageTests

open Expecto
open Utils.ServerTests
open Helpers
open Utils.Server
open System
open Ionide.LanguageServerProtocol.Types

type Document with

  member x.NestedLanguages =
    x.Server.Events
    |> Document.typedEvents<FsAutoComplete.Lsp.TextDocumentNestedLanguages> ("fsharp/textDocument/nestedLanguages")
    |> Observable.filter (fun n -> n.TextDocument = x.VersionedTextDocumentIdentifier)

let hasLanguages name source expectedLanguages server =
  testAsync name {
    let! (doc, diags) = server |> Server.createUntitledDocument source
    Expect.isEmpty diags "no diagnostics"
    let! nestedLanguages = doc.NestedLanguages |> Async.AwaitObservable

    let mappedExpectedLanguages: FsAutoComplete.Lsp.NestedLanguage array =
      expectedLanguages
      |> Array.map (fun (l, rs) ->
        { Language = l
          Ranges =
            rs
            |> Array.map (fun ((sl, sc), (el, ec)) ->
              { Start = { Line = sl; Character = sc }
                End = { Line = el; Character = ec } }) })

    Expect.equal nestedLanguages.NestedLanguages mappedExpectedLanguages "languages"
  }

let tests state =
  testList
    "nested languages"
    [ testList
        "BCL"
        // pending because class members don't return attributes in the FCS Parameter API
        [ pserverTestList "class member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            let b = System.UriBuilder("https://google.com")
            """
                [| ("uri", [| (1, 38), (1, 58) |]) |]
                server ]) ]
      testList
        "FSharp Code"
        // pending because class members don't return attributes in the FCS Parameter API
        [ pserverTestList "class member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            type Foo() =
              member x.Boo([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let f = new Foo()
            let u = f.Boo("https://google.com")
            """
                [| ("uri", [| (4, 26), (4, 46) |]) |]
                server ])

          serverTestList "let bound function member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            let boo ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let u = boo "https://google.com"
            """
                [| ("uri", [| (2, 24), (2, 44) |]) |]
                server

              hasLanguages
                    "with single string parameter and string literal"
                    """
            let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
            let u = uri "https://google.com"
            """
                    [| ("uri", [| (2, 24), (2, 44) |]) |]
                    server

              hasLanguages
                    "with single string parameter and interpolated string literal"
                    """
            let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
            let u = uri $"https://{true}.com"
            """
                    [| ("uri", [| (2, 24), (2, 35)
                                  (2,39), (2, 45) |]) |]
                    server

              hasLanguages
                    "multiple languages in the same document"
                    """
            let html ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("html")>]s: string) = ()
            let sql ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("sql")>]s: string) = ()
            let myWebPage = html "<body>wow</body>"
            let myQuery = sql "select * from accounts where net_worth > 1000000"
            """
                    [| ("html", [| (3, 33), (3, 51) |])
                       ("sql", [| (4, 30), (4, 80) |]) |]
                    server
          ]
        )
      ]
    ]

module FsAutoComplete.CodeFix.AddMissingFunKeyword

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
/// a codefix that adds a missing 'fun' keyword to a lambda
val fix: getFileLines: GetFileLines -> getLineText: GetLineText -> (CodeActionParams -> Async<Result<Fix list, string>>)

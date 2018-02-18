module LanguageServerProtocol

let private dbgf format = Printf.ksprintf (fun s -> System.Diagnostics.Trace.WriteLine(s)) format

module Protocol =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Converters

    type TextDocumentSyncKind =
        | None = 0
        | Full = 1
        | Incremental = 2

    [<AutoOpen>]
    module General =
        type TraceSetting =
            | Off = 0
            | Messages = 1
            | Verbose = 2

        /// Capabilities for methods that support dynamic registration.
        type DynamicCapabilities = {
            /// Method supports dynamic registration.
            DynamicRegistration: bool option
        }

        /// Capabilities specific to `WorkspaceEdit`s
        type WorkspaceEditCapabilities = {
            /// The client supports versioned document changes in `WorkspaceEdit`s
            DocumentChanges: bool option
        }

        type SymbolKind =
        | File = 1
        | Module = 2
        | Namespace = 3
        | Package = 4
        | Class = 5
        | Method = 6
        | Property = 7
        | Field = 8
        | Constructor = 9
        | Enum = 10
        | Interface = 11
        | Function = 12
        | Variable = 13
        | Constant = 14
        | String = 15
        | Number = 16
        | Boolean = 17
        | Array = 18
        | Object = 19
        | Key = 20
        | Null = 21
        | EnumMember = 22
        | Struct = 23
        | Event = 24
        | Operator = 25
        | TypeParameter = 26

        /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
        type SymbolKindCapabilities = {
            /// The symbol kind values the client supports. When this
            /// property exists the client also guarantees that it will
            /// handle values outside its set gracefully and falls back
            /// to a default value when unknown.
            ///
            /// If this property is not present the client only supports
            /// the symbol kinds from `File` to `Array` as defined in
            /// the initial version of the protocol.
            ValueSet: SymbolKind[] option
        }
        with
            static member DefaultValueSet =
                [
                    SymbolKind.File
                    SymbolKind.Module
                    SymbolKind.Namespace
                    SymbolKind.Package
                    SymbolKind.Class
                    SymbolKind.Method
                    SymbolKind.Property
                    SymbolKind.Field
                    SymbolKind.Constructor
                    SymbolKind.Enum
                    SymbolKind.Interface
                    SymbolKind.Function
                    SymbolKind.Variable
                    SymbolKind.Constant
                    SymbolKind.String
                    SymbolKind.Number
                    SymbolKind.Boolean
                    SymbolKind.Array
                ]

        /// Capabilities specific to the `workspace/symbol` request.
        type SymbolCapabilities = {
            /// Symbol request supports dynamic registration.
            DynamicRegistration: bool option

            /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
            SymbolKind: SymbolKindCapabilities option
        }

        /// Workspace specific client capabilities.
        type WorkspaceClientCapabilities = {
            /// The client supports applying batch edits to the workspace by supporting
            /// the request 'workspace/applyEdit'
            ApplyEdit: bool option

            /// Capabilities specific to `WorkspaceEdit`s
            WorkspaceEdit: WorkspaceEditCapabilities option

            /// Capabilities specific to the `workspace/didChangeConfiguration` notification.
            DidChangeConfiguration: DynamicCapabilities option

            /// Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
            DidChangeWatchedFiles: DynamicCapabilities option

            /// Capabilities specific to the `workspace/symbol` request.
            Symbol: SymbolCapabilities option
        }

        type SynchronizationCapabilities = {
            /// Whether text document synchronization supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports sending will save notifications.
            WillSave: bool option

            /// The client supports sending a will save request and
            /// waits for a response providing text edits which will
            /// be applied to the document before it is saved.
            WillSaveWaitUntil: bool option

            /// The client supports did save notifications.
            DidSave: bool option
        }

        module MarkupKind =
            let PlainText = "plaintext"
            let Markdown = "markdown"

        type HoverCapabilities = {
            /// Whether hover synchronization supports dynamic registration.
            DynamicRegistration: bool option

            /// Client supports the follow content formats for the content
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            ContentFormat: string[] option
        }

        type CompletionItemCapabilities = {
            /// Client supports snippets as insert text.
            ///
            /// A snippet can define tab stops and placeholders with `$1`, `$2`
            /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
            /// the end of the snippet. Placeholders with equal identifiers are linked,
            /// that is typing in one will update others too.
            SnippetSupport: bool option

            /// Client supports commit characters on a completion item.
            CommitCharactersSupport: bool option

            /// Client supports the follow content formats for the documentation
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            DocumentationFormat: string[] option
        }

        type CompletionItemKind =
        | Text = 1
        | Method = 2
        | Function = 3
        | Constructor = 4
        | Field = 5
        | Variable = 6
        | Class = 7
        | Interface = 8
        | Module = 9
        | Property = 10
        | Unit = 11
        | Value = 12
        | Enum = 13
        | Keyword = 14
        | Snippet = 15
        | Color = 16
        | File = 17
        | Reference = 18
        | Folder = 19
        | EnumMember = 20
        | Constant = 21
        | Struct = 22
        | Event = 23
        | Operator = 24
        | TypeParameter = 25

        type CompletionItemKindCapabilities = {
            /// The completion item kind values the client supports. When this
            /// property exists the client also guarantees that it will
            /// handle values outside its set gracefully and falls back
            /// to a default value when unknown.
            ///
            /// If this property is not present the client only supports
            /// the completion items kinds from `Text` to `Reference` as defined in
            /// the initial version of the protocol.
            ValueSet: CompletionItemKind[] option
        }
        with
            static member DefaultValueSet =
                [
                    CompletionItemKind.Text
                    CompletionItemKind.Method
                    CompletionItemKind.Function
                    CompletionItemKind.Constructor
                    CompletionItemKind.Field
                    CompletionItemKind.Variable
                    CompletionItemKind.Class
                    CompletionItemKind.Interface
                    CompletionItemKind.Module
                    CompletionItemKind.Property
                    CompletionItemKind.Unit
                    CompletionItemKind.Value
                    CompletionItemKind.Enum
                    CompletionItemKind.Keyword
                    CompletionItemKind.Snippet
                    CompletionItemKind.Color
                    CompletionItemKind.File
                    CompletionItemKind.Reference
                ]

        /// Capabilities specific to the `textDocument/completion`
        type CompletionCapabilities = {
            /// Whether completion supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports the following `CompletionItem` specific
            /// capabilities.
            CompletionItem: CompletionItemCapabilities option

            CompletionItemKind: CompletionItemKindCapabilities option

            /// The client supports to send additional context information for a
            /// `textDocument/completion` request.
            ContextSupport: bool option
        }

        type SignatureInformationCapabilities = {
            /// Client supports the follow content formats for the documentation
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            DocumentationFormat: string[] option
        }

        type SignatureHelpCapabilities = {
            /// Whether signature help supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports the following `SignatureInformation`
            /// specific properties.
            SignatureInformation: SignatureInformationCapabilities option
        }

        /// apabilities specific to the `textDocument/documentSymbol`
        type DocumentSymbolCapabilities = {
            /// Whether document symbol supports dynamic registration.
            DynamicRegistration: bool option

            /// Specific capabilities for the `SymbolKind`.
            SymbolKind: SymbolKindCapabilities option
        }

        /// Text document specific client capabilities.
        type TextDocumentClientCapabilities = {
            Synchronization: SynchronizationCapabilities option

            /// Capabilities specific to the `textDocument/completion`
            Completion: CompletionCapabilities option

            /// Capabilities specific to the `textDocument/hover`
            Hover: HoverCapabilities option

            /// Capabilities specific to the `textDocument/signatureHelp`
            SignatureHelp: SignatureHelpCapabilities option

            /// Capabilities specific to the `textDocument/references`
            References: DynamicCapabilities option

            /// Whether document highlight supports dynamic registration.
            DocumentHighlight: DynamicCapabilities option

            /// apabilities specific to the `textDocument/documentSymbol`
            DocumentSymbol: DocumentSymbolCapabilities option

            /// Capabilities specific to the `textDocument/formatting`
            Formatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/rangeFormatting`
            RangeFormatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/onTypeFormatting`
            OnTypeFormatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/definition`
            Definition: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/codeAction`
            CodeAction: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/codeLens`
            CodeLens: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/documentLink`
            DocumentLink: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/rename`
            Rename: DynamicCapabilities option
        }

        type ClientCapabilities = {
            /// Workspace specific client capabilities.
            Workspace: WorkspaceClientCapabilities option

            /// Text document specific client capabilities.
            TextDocument: TextDocumentClientCapabilities option

            /// Experimental client capabilities.
            Experimental: JToken option
        }

        type InitializeParams = {
            ProcessId: int option
            RootPath: string option
            RootUri: string option
            InitializationOptions: JToken option
            Capabilities: ClientCapabilities option
            [<JsonConverter(typeof<StringEnumConverter>, true)>]
            trace: string option
        }

        /// Completion options.
        type CompletionOptions = {
            /// The server provides support to resolve additional information for a completion item.
            ResolveProvider: bool option

            /// The characters that trigger completion automatically.
            TriggerCharacters: string[] option
        }

        /// Signature help options.
        type SignatureHelpOptions = {
            /// The characters that trigger signature help automatically.
            TriggerCharacters: string[] option
        }

        /// Code Lens options.
        type CodeLensOptions = {
            /// Code lens has a resolve provider as well.
            ResolveProvider: bool option;
        }

        /// Format document on type options
        type DocumentOnTypeFormattingOptions = {
            /// A character on which formatting should be triggered, like `}`.
            FirstTriggerCharacter: string

            /// More trigger characters.
            MoreTriggerCharacter: string[] option
        }

        /// Document link options
        type DocumentLinkOptions = {
            /// Document links have a resolve provider as well.
            ResolveProvider: bool option
        }

        /// Execute command options.
        type ExecuteCommandOptions = {
            /// The commands to be executed on the server
            commands: string[] option
        }

        /// Save options.
        type SaveOptions = {
            /// The client is supposed to include the content on save.
            IncludeText: bool option
        }

        type TextDocumentSyncOptions = {
            /// Open and close notifications are sent to the server.
            OpenClose: bool option

            /// Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
            /// and TextDocumentSyncKindIncremental.
            Change: TextDocumentSyncKind option

            /// Will save notifications are sent to the server.
            WillSave: bool option

            /// Will save wait until requests are sent to the server.
            WillSaveWaitUntil: bool option

            /// Save notifications are sent to the server.
            Save: SaveOptions option
        }
        with
            static member Default =
                {
                    OpenClose = None
                    Change = None
                    WillSave = None
                    WillSaveWaitUntil = None
                    Save = None
                }

        type ServerCapabilities = {
            /// Defines how text documents are synced. Is either a detailed structure defining each notification or
            /// for backwards compatibility the TextDocumentSyncKind number.
            TextDocumentSync: TextDocumentSyncOptions option

            /// The server provides hover support.
            HoverProvider: bool option

            /// The server provides completion support.
            CompletionProvider: CompletionOptions option

            /// The server provides signature help support.
            SignatureHelpProvider: SignatureHelpOptions option

            /// The server provides goto definition support.
            DefinitionProvider: bool option

            /// The server provides find references support.
            ReferencesProvider: bool option

            /// The server provides document highlight support.
            DocumentHighlightProvider: bool option

            /// The server provides document symbol support.
            DocumentSymbolProvider: bool option

            /// The server provides workspace symbol support.
            WorkspaceSymbolProvider: bool option

            /// The server provides code actions.
            CodeActionProvider: bool option

            /// The server provides code lens.
            CodeLensProvider: CodeLensOptions option

            /// The server provides document formatting.
            DocumentFormattingProvider: bool option

            /// The server provides document range formatting.
            DocumentRangeFormattingProvider: bool option

            /// The server provides document formatting on typing.
            DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option

            /// The server provides rename support.
            RenameProvider: bool option

            /// The server provides document link support.
            DocumentLinkProvider: DocumentLinkOptions option

            /// The server provides execute command support.
            ExecuteCommandProvider: ExecuteCommandOptions option

            /// Experimental server capabilities.
            Experimental: JToken option
        }
        with
            static member Default =
                {
                    HoverProvider = None
                    TextDocumentSync = None
                    CompletionProvider = None
                    SignatureHelpProvider = None
                    DefinitionProvider = None
                    ReferencesProvider = None
                    DocumentHighlightProvider = None
                    DocumentSymbolProvider = None
                    WorkspaceSymbolProvider = None
                    CodeActionProvider = None
                    CodeLensProvider = None
                    DocumentFormattingProvider = None
                    DocumentRangeFormattingProvider = None
                    DocumentOnTypeFormattingProvider = None
                    RenameProvider = None
                    DocumentLinkProvider = None
                    ExecuteCommandProvider = None
                    Experimental = None
                }

        type InitializeResult = {
            Capabilities: ServerCapabilities
        }
        with
            static member Default =
                {
                    Capabilities = ServerCapabilities.Default
                }

    [<AutoOpen>]
    module Window =
        type MessageType =
        | Error = 1
        | Warning = 2
        | Info = 3
        | Log = 4

        type LogMessageParams = {
            Type: MessageType
            Message: string
        }

        type ShowMessageParams = {
            Type: MessageType
            Message: string
        }

    /// Position in a text document expressed as zero-based line and zero-based character offset.
    /// A position is between two characters like an ‘insert’ cursor in a editor.
    type Position = {
        /// Line position in a document (zero-based).
        Line: int

        /// Character offset on a line in a document (zero-based). Assuming that the line is
        /// represented as a string, the `character` value represents the gap between the
        /// `character` and `character + 1`.
        ///
        /// If the character value is greater than the line length it defaults back to the
        /// line length.
        Character: int
    }

    /// A range in a text document expressed as (zero-based) start and end positions.
    /// A range is comparable to a selection in an editor. Therefore the end position is exclusive.
    /// If you want to specify a range that contains a line including the line ending character(s)
    /// then use an end position denoting the start of the next line. For example:
    type Range = {
        /// The range's start position.
        Start: Position

        /// The range's end position.
        End: Position
    }

    type DocumentUri = string

    type TextDocumentIdentifier = {
        /// The text document's URI.
        Uri: DocumentUri
    }

    type TextDocumentPositionParams = {
        /// The text document.
        TextDocument: TextDocumentIdentifier
        /// The position inside the text document.
        Position: Position
    }

    /// A `MarkupContent` literal represents a string value which content is interpreted base on its
    /// kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
    ///
    /// If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
    /// See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
    ///
    /// Here is an example how such a string can be constructed using JavaScript / TypeScript:
    /// ```ts
    /// let markdown: MarkdownContent = {
    ///     kind: MarkupKind.Markdown,
    ///     value: [
    ///         '# Header',
    ///         'Some text',
    ///         '```typescript',
    ///         'someCode();',
    ///         '```'
    ///     ].join('\n')
    /// };
    /// ```
    ///
    /// *Please Note* that clients might sanitize the return markdown. A client could decide to
    /// remove HTML from the markdown to avoid script execution.
    type MarkupContent = {
        /// The type of the Markup
        Kind: string

        // The content itself
        Value: string
    }

    let plaintext s = { Kind = MarkupKind.PlainText; Value = s }
    let markdown s = { Kind = MarkupKind.Markdown; Value = s }

    /// The result of a hover request.
    type Hover = {
        /// The hover's content
        Contents: MarkupContent

        /// An optional range is a range inside a text document
        /// that is used to visualize a hover, e.g. by changing the background color.
        Range: Range option
    }

    /// An item to transfer a text document from the client to the server.
    type TextDocumentItem = {
        /// The text document's URI.
        Uri: DocumentUri

        /// The text document's language identifier.
        LanguageId: string

        /// The version number of this document (it will increase after each
        /// change, including undo/redo).
        Version: int

        /// The content of the opened text document.
        Text: string
    }

    type DidOpenTextDocumentParams = {
        /// The document that was opened.
        TextDocument: TextDocumentItem
    }

    [<AutoOpen>]
    module Messages =
        type ClientRequest =
            | Initialize of InitializeParams
            | DidOpenTextDocument of DidOpenTextDocumentParams
            | Hover of TextDocumentPositionParams
            | Initialized
            | Shutdown
            | Exit
        with
            member this.IsNotification
                with get() =
                    match this with
                    | Initialize _
                    | Hover _ -> false
                    | DidOpenTextDocument _
                    | Initialized
                    | Shutdown
                    | Exit -> true

        type ServerRequest =
            | LogMessage of LogMessageParams
            | ShowMessage of ShowMessageParams
        with
            member this.AsJsonSerializable
                with get() =
                    match this with
                    | LogMessage x -> box x
                    | ShowMessage x -> box x
            member this.Method
                with get() =
                    match this with
                    | LogMessage _ -> "window/showMessage"
                    | ShowMessage _ -> "window/showMessage"

        type ServerResponse =
            | NoResponse
            | InvalidRequest of string
            | UnhandledRequest
            | InitializeResponse of InitializeResult
            | HoverResponse of Hover option
        with
            member this.AsJsonSerializable
                with get() =
                    match this with
                    | InitializeResponse x -> box x
                    | HoverResponse x -> box x
                    | NoResponse
                    | InvalidRequest _
                    | UnhandledRequest -> failwith "Technical responses can't be sent as JSON"


module LowLevel =
    open System
    open System.IO
    open System.Text

    let headerBufferSize = 300
    let minimumHeaderLength = 21
    let cr = byte '\r'
    let lf = byte '\f'
    let headerEncoding = Encoding.ASCII

    let private readLine (stream: Stream) =
        let buffer = Array.zeroCreate<byte> headerBufferSize
        let mutable count = stream.Read(buffer, 0, 2)
        if count < 2 then
            None
        else
            // TODO: Check that we don't over-fill headerBufferSize
            while count < headerBufferSize && (buffer.[count-2] <> cr && buffer.[count-1] <> lf) do
                 let additionalBytesRead  = stream.Read(buffer, count, 1)
                 // TODO: exit when additionalBytesRead = 0, end of stream
                 count <- count + additionalBytesRead

            if count >= headerBufferSize then
                None
            else
                Some (headerEncoding.GetString(buffer, 0, count - 2))

    let rec private readHeaders (stream: Stream) =
        let line = readLine stream
        match line with
        | Some "" ->  []
        | Some line ->
            let separatorPos = line.IndexOf(": ")
            if separatorPos = -1 then
                failwithf "Separator not found in header '%s'" line
            else
                let name = line.Substring(0, separatorPos)
                let value = line.Substring(separatorPos + 2)
                (name,value) :: (readHeaders stream)
        | None ->
            raise (EndOfStreamException())

    let read (stream: Stream) =
        dbgf "Starting READ"
        let headers = readHeaders stream

        dbgf "Headers = %A" headers
        let contentLength =
            headers
            |> List.tryFind(fun (name, _) -> name = "Content-Length")
            |> Option.map snd
            |> Option.bind (fun s -> match Int32.TryParse(s) with | true, x -> Some x | _ -> None)

        if contentLength = None then
            dbgf "Content-Length header not found"
            failwithf "Content-Length header not found"
        else
            dbgf "Reading %d bytes of data" contentLength.Value
            let result = Array.zeroCreate<byte> contentLength.Value
            let readCount = stream.Read(result, 0, contentLength.Value)
            dbgf "Read %d bytes" readCount
            let str = Encoding.UTF8.GetString(result, 0, readCount)
            dbgf "Read '%s'" str
            headers, str

    let write (stream: Stream) (data: string) =
        let asciiWriter = new StreamWriter(stream, Encoding.ASCII, 1024 * 1024, true)
        let bytes = Encoding.UTF8.GetBytes(data)
        asciiWriter.Write("Content-Length: ")
        asciiWriter.WriteLine(string bytes.Length)
        asciiWriter.WriteLine("Content-Type: utf-8")
        asciiWriter.WriteLine();
        asciiWriter.Dispose()

        stream.Write(bytes, 0, bytes.Length)

module JsonRpc =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type Request = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Method: string
        Params: JToken option
    }
    with
        static member Create(id: int, method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = Some id; Method = method'; Params = Some rpcParams }

        static member Create(method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = None; Method = method'; Params = Some rpcParams }

    type Error = {
        Code: int
        Message: string
        Data: JToken option
    }
    with
        static member Create(code: int, message: string) =
            { Code = code; Message = message; Data = None }

    type Response = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Error: Error option
        Result: JToken option
    }
    with
        static member Success(id: int, result: JToken) =
            { Version = "2.0"; Id = Some id; Result = Some result; Error = None }
        static member Failure(id: int option, error: Error) =
            { Version = "2.0"; Id = id; Result = None; Error = Some error }

module Server =
    open System.IO
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Serialization

    open Protocol

    let jsonSettings =
        let result = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        result.Converters.Add(FsAutoComplete.OptionConverter())
        result.ContractResolver <- CamelCasePropertyNamesContractResolver()
        result

    let jsonSerializer = JsonSerializer.Create(jsonSettings)

    let private parseRequest<'a> (f: 'a -> ClientRequest) (paramsToken: JToken option) =
        paramsToken
        |> Option.map (fun paramsToken ->
            paramsToken.ToObject<'a>(jsonSerializer) |> f)

    let private parseEmpty (req: ClientRequest) (_params: JToken option) =
        Some req

    let requestParser = function
    | "initialize" -> parseRequest<InitializeParams> Initialize
    | "initialized" -> parseEmpty Initialized
    | "textDocument/hover" -> parseRequest<TextDocumentPositionParams> Hover
    | "textDocument/didOpen" -> parseRequest<DidOpenTextDocumentParams> DidOpenTextDocument
    | "shutdown" -> parseEmpty Shutdown
    | "exit" -> parseEmpty Exit
    | _ -> fun _ -> None

    type RequestSender = ServerRequest -> unit

    let start (input: Stream) (output: Stream) (handler: RequestSender -> ClientRequest -> ServerResponse) =
        dbgf "Starting up !"

        let sender = MailboxProcessor<string>.Start(fun inbox ->
            let rec loop () = async {
                let! str = inbox.Receive()
                dbgf "Writing to client: %s" str
                LowLevel.write output str
                return! loop ()
            }
            loop ())

        let requestHandler = handler (fun r ->
            let serializedResponse = JToken.FromObject(r.AsJsonSerializable, jsonSerializer)
            let req = JsonRpc.Request.Create(r.Method, serializedResponse)
            let reqString = JsonConvert.SerializeObject(req, jsonSettings)
            sender.Post(reqString))

        while true do
            try
                let _, rpcRequestString = LowLevel.read input
                dbgf "Received: %s" rpcRequestString

                let rpcRequest = JsonConvert.DeserializeObject<JsonRpc.Request>(rpcRequestString, jsonSettings)
                let request = requestParser rpcRequest.Method rpcRequest.Params
                dbgf "Parsed as: %A" request

                match request with
                | Some request ->
                    let response = requestHandler request
                    dbgf "Will answer: %A" response
                    let rpcResponse =
                        match response with
                        | NoResponse -> None
                        | InvalidRequest message ->
                            JsonRpc.Response.Failure(rpcRequest.Id, JsonRpc.Error.Create(-32602, message)) |> Some
                        | UnhandledRequest ->
                            JsonRpc.Response.Failure(rpcRequest.Id, JsonRpc.Error.Create(-32601, "Method not found")) |> Some
                        | response ->
                            let serializedResponse = JToken.FromObject(response.AsJsonSerializable, jsonSerializer)
                            JsonRpc.Response.Success(rpcRequest.Id.Value, serializedResponse) |> Some
                    match rpcResponse with
                    | Some rpcResponse ->
                        let rpcResponseString = JsonConvert.SerializeObject(rpcResponse, jsonSettings)
                        sender.Post(rpcResponseString)
                    | _ -> ()
                | _ ->
                    dbgf "Unable to parse request: %s" rpcRequestString
            with
            | :? EndOfStreamException ->
                dbgf "Client closed the input stream"
                System.Environment.Exit(0)
            | ex -> dbgf "%O" ex
        ()

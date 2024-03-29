namespace FSharp.Compiler

module Syntax =
  open FSharp.Compiler.Syntax

  type SyntaxCollectorBase =
    new: unit -> SyntaxCollectorBase
    abstract WalkSynModuleOrNamespace: SynModuleOrNamespace -> unit
    abstract WalkAttribute: SynAttribute -> unit
    abstract WalkSynModuleDecl: SynModuleDecl -> unit
    abstract WalkExpr: SynExpr -> unit
    abstract WalkTypar: SynTypar -> unit
    abstract WalkTyparDecl: SynTyparDecl -> unit
    abstract WalkTypeConstraint: SynTypeConstraint -> unit
    abstract WalkType: SynType -> unit
    abstract WalkMemberSig: SynMemberSig -> unit
    abstract WalkPat: SynPat -> unit
    abstract WalkValTyparDecls: SynValTyparDecls -> unit
    abstract WalkBinding: SynBinding -> unit
    abstract WalkSimplePat: SynSimplePat -> unit
    abstract WalkInterfaceImpl: SynInterfaceImpl -> unit
    abstract WalkClause: SynMatchClause -> unit
    abstract WalkInterpolatedStringPart: SynInterpolatedStringPart -> unit
    abstract WalkMeasure: SynMeasure -> unit
    abstract WalkComponentInfo: SynComponentInfo -> unit
    abstract WalkTypeDefnSigRepr: SynTypeDefnSigRepr -> unit
    abstract WalkUnionCaseType: SynUnionCaseKind -> unit
    abstract WalkEnumCase: SynEnumCase -> unit
    abstract WalkField: SynField -> unit
    abstract WalkTypeDefnSimple: SynTypeDefnSimpleRepr -> unit
    abstract WalkValSig: SynValSig -> unit
    abstract WalkMember: SynMemberDefn -> unit
    abstract WalkUnionCase: SynUnionCase -> unit
    abstract WalkTypeDefnRepr: SynTypeDefnRepr -> unit
    abstract WalkTypeDefn: SynTypeDefn -> unit

  val walkAst: walker: SyntaxCollectorBase -> input: ParsedInput -> unit

  /// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
  val (|Sequentials|_|): (SynExpr -> SynExpr list option)
  val (|ConstructorPats|): (SynArgPats -> SynPat list)
  /// A pattern that collects all attributes from a `SynAttributes` into a single flat list
  val (|AllAttrs|): attrs: SynAttributes -> SynAttribute list
  /// A pattern that collects all patterns from a `SynSimplePats` into a single flat list
  val (|AllSimplePats|): pats: SynSimplePats -> SynSimplePat list

namespace FsAutoComplete

module UntypedAstUtils =

  open FSharp.Compiler.Syntax
  open FSharp.Compiler.Text

  type Range with

    member inline IsEmpty: bool

  type internal ShortIdent = string
  type internal Idents = ShortIdent[]
  val internal longIdentToArray: longIdent: LongIdent -> Idents

  /// matches if the range contains the position
  val (|ContainsPos|_|): pos: pos -> range: range -> unit option
  /// Active pattern that matches an ident on a given name by the ident's `idText`
  val (|Ident|_|): ofName: string -> (SynExpr -> unit option)
  /// matches if the range contains the position
  val (|IdentContainsPos|_|): pos: pos -> ident: Ident -> unit option

module FoldingRange =
  open FSharp.Compiler.Syntax
  open FSharp.Compiler.Text

  val getRangesAtPosition: input: ParsedInput -> r: Position -> Range list

module Completion =
  open FSharp.Compiler.Syntax
  open FSharp.Compiler.Text

  [<RequireQualifiedAccess>]
  type Context =
    | StringLiteral
    | Unknown
    | SynType

  val atPos: pos: Position * ast: ParsedInput -> Context

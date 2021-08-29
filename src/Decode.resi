type jsonTreeRef = {
  tree: Js.Json.t,
  path: string,
}

type jsonObjectRef = {
  object: Js.Dict.t<Js.Json.t>,
  path: string,
}

type jsonParseError =
  | SyntaxError(string)
  | WrongType(string, string)
  | NoValueError(string)
  | RecordParseError(string, string)
  | CustomError(string)

let jsonParseErrorToString: jsonParseError => string

type jsonParseResult<'a> = Belt.Result.t<'a, jsonParseError>
type jsonTreeResult = Belt.Result.t<jsonTreeRef, jsonParseError>
type parser<'a> = jsonTreeResult => jsonParseResult<'a>
type recordParser<'a> = (Js.Dict.key, jsonTreeResult) => jsonParseResult<'a>;

let parse: (string, parser<'a>) => jsonParseResult<'a>

let object_: jsonTreeResult => jsonParseResult<jsonObjectRef>

let record:  (jsonTreeResult, recordParser<'a>) => jsonParseResult<array<'a>>

let string: jsonTreeResult => jsonParseResult<string>

let number: jsonTreeResult => jsonParseResult<float>

let integer: jsonTreeResult => jsonParseResult<int>

let boolean: jsonTreeResult => jsonParseResult<bool>

let null: jsonTreeResult => jsonParseResult<Js.Null.t<'a>>

let array: (jsonTreeResult, parser<'a>) => jsonParseResult<array<'a>>

let field: (jsonParseResult<jsonObjectRef>, string, parser<'a>) => jsonParseResult<'a>

let optional: (jsonTreeResult, parser<'a>) => jsonParseResult<option<'a>>

let any: (jsonTreeResult) => jsonParseResult<Js.Json.t>


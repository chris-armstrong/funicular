@ocaml.doc(
  "A reference to part of the jSON parse tree. This is an intermediate type used in parsing to improve error messaging"
)
type jsonTreeRef = {
  @ocaml.doc("The JSON element")
  tree: Js.Json.t,
  @ocaml.doc("Dot-separated path to the JSON element (from the root element)")
  path: string,
}

@ocaml.doc("A JSON Object reference")
type jsonObjectRef = {
  @ocaml.doc("The JSON object (as a dictionary)")
  object: Js.Dict.t<Js.Json.t>,
  @ocml.doc("Dot-separated path to the JSON object (from the root element)")
  path: string,
}

/*
 * A JSON parse error
 */
@ocaml.doc("A JSON parse error")
type jsonParseError = [
  | @ocaml.doc("A syntax error (message given as parameter)") #SyntaxError(string)
  | @ocaml.doc("The path (first parameter) was expected to be of type (second parameter)")
  #WrongTypeError(string, string)
  | @ocaml.doc("No value was found at the specified path") #NoValueError(string)
]

@ocaml.doc("Convert the given jsonParseError to a string")
let jsonParseErrorToString: jsonParseError => string

// A JSON parse result
@ocaml.doc("The result type of a JSON parse operation")
type jsonParseResult<'a, 'b> = result<'a, [> jsonParseError] as 'b>
// A result with part of the JSON tree
@ocaml.doc("A JSON tree result (from looking up a field)")
type jsonTreeResult<'c> = result<jsonTreeRef, [> jsonParseError] as 'c>
// A parser function that converts a tree result to a concrete type
@ocaml.doc("A parser for a JSON type that produces a concrete type")
type parser<'a, 'b> = jsonTreeResult<'b> => result<'a, 'b>

@ocaml.doc("Create a human-readable string for the given jsonParseError")
let jsonParseErrorToString: jsonParseError => string

/* Parse the given JSON string with the specified parser function, giving a result */
@ocaml.doc("Parse the given JSON string with a JSON parser, returning the value as a result")
let parse: (string, parser<'a, [> jsonParseError] as 'b>) => result<'a, 'b>

@ocaml.doc("Parse the given JSON tree reference as a JSON object")
let object_: jsonTreeResult<'b> => jsonParseResult<jsonObjectRef, 'b>

@ocaml.doc("Parse the given JSON tree reference as a string")
let string: jsonTreeResult<'b> => jsonParseResult<string, 'b>

@ocaml.doc("Parse the given JSON tree reference as a float")
let number: jsonTreeResult<'b> => jsonParseResult<float, 'b>

@ocaml.doc("Parse the given JSON tree reference as an integer")
let integer: jsonTreeResult<'b> => jsonParseResult<int, 'b>

@ocaml.doc("Parse the given JSON tree reference as boolean")
let boolean: jsonTreeResult<'b> => jsonParseResult<bool, 'b>

@ocaml.doc("Parse the given JSON tree reference as JavaScript `null` value")
let null: jsonTreeResult<'b> => jsonParseResult<Js.Null.t<'a>, 'b>

@ocaml.doc("Parse the given JSON tree reference as an array")
let array: (
  @ocaml.doc("The tree reference that is expected to contain an array. It takes the tree reference and an item parser that should be used to parse each item, returning a result containing the parsed array.")
  jsonTreeResult<'b>,
  @ocaml.doc("The array item parser, a function which will parse each array element")
  parser<'a, [> jsonParseError] as 'b>,
) => jsonParseResult<array<'a>, 'b>

@ocaml.doc("Lookup a field on the given JSON Object of name, using parser to convert it to a concrete value")
let field: (
  result<jsonObjectRef, 'b>,
  string,
  parser<'a, [> jsonParseError] as 'b>,
) => jsonParseResult<'a, 'b>

@ocaml.doc("Convert the parser given to be optional (making its concrete type 'a into option<'a>. When the parser gets a value, the result will be Some('a), otherwise if the parser generates #NoValueError, the result will instead be None instead of an Error")
let optional: (
  parser<'a, [> jsonParseError] as 'b>,
  result<jsonTreeRef, 'b>,
) => jsonParseResult<option<'a>, 'b>

@ocaml.doc("Parse the given JSON tree result as an `any` type (effectively just gets the Js.Json.t at the given location)")
let any: jsonTreeResult<'b> => jsonParseResult<Js.Json.t, 'b>

@ocaml.doc("Wrap a mapping function to begin a decode-value train. Your mapping function should be of the form:\r\n

 ('a, 'b) => 'res\n\n

where it may be a function of any arity with parameters 'a, 'b, etc. giving a value of type 'res.You can then feed in concrete jsonParseResult values with the -> operator by wrapping them in v() e.g.

let name_ = object->field(\"name\", string);\r\n
let address_ = object->field(\"address\", string)\r\n
rmap((name, address) => { name, address })->v(name_)->v(address_)\r\n
")
let rmap: 'a => jsonParseResult<'a, 'b>

@ocaml.doc("Wrap a jsonParseResult and feed it into an rmap mapping function (see rmap() for an example)")
let v: (result<'a => 'b, 'c>, jsonParseResult<'a, 'c>) => jsonParseResult<'b, 'c>

let map1: ('r1 => 'mapped, jsonParseResult<'r1, 'err>) => result<'mapped, 'err>
let map2: (('r1, 'r2) => 'mapped, result<'r1, 'err>, result<'r2, 'err>) => result<'mapped, 'err>;
let map3: (('r1, 'r2, 'r3) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>) => result<'mapped, 'err>;
let map4: (('r1, 'r2, 'r3, 'r4) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>) => result<'mapped, 'err>;
let map5: (('r1, 'r2, 'r3, 'r4, 'r5) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>) => result<'mapped, 'err>;
let map6: (('r1, 'r2, 'r3, 'r4, 'r5, 'r6) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>, result<'r6, 'err>) => result<'mapped, 'err>;
let map7: (('r1, 'r2, 'r3, 'r4, 'r5, 'r6, 'r7) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>, result<'r6, 'err>, result<'r7, 'err>) => result<'mapped, 'err>;
let map8: (('r1, 'r2, 'r3, 'r4, 'r5, 'r6, 'r7, 'r8) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>, result<'r6, 'err>, result<'r7, 'err>, result<'r8, 'err>) => result<'mapped, 'err>;
let map9: (('r1, 'r2, 'r3, 'r4, 'r5, 'r6, 'r7, 'r8, 'r9) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>, result<'r6, 'err>, result<'r7, 'err>, result<'r8, 'err>, result<'r9, 'err>) => result<'mapped, 'err>;
let map10: (('r1, 'r2, 'r3, 'r4, 'r5, 'r6, 'r7, 'r8, 'r9, 'r10) => 'mapped, result<'r1, 'err>, result<'r2, 'err>, result<'r3, 'err>, result<'r4, 'err>, result<'r5, 'err>, result<'r6, 'err>, result<'r7, 'err>, result<'r8, 'err>, result<'r9, 'err>, result<'r10, 'err>) => result<'mapped, 'err>;

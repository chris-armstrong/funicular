@ocaml.doc("Encode string as a JSON string")
let string: string => Js.Json.t
@ocaml.doc("Encode integer as a JSON number")
let integer: int => Js.Json.t
@ocaml.doc("Encode float as a JSON number")
let number: float => Js.Json.t
@ocaml.doc("Encode bool as a JSON boolean")
let boolean: bool => Js.Json.t
@ocaml.doc("Encode null as a JSON null")
let null: Js.Null.t<'a> => Js.Json.t
@ocaml.doc("Generate a JSON undefined value (note that JSON itself cannot contain undefined, so this ensures the asociated property is removed)")
let undefined: Js.Json.t;
let optional: (option<'a>, 'a => Js.Json.t) => Js.Json.t
@ocaml.doc("Encode the given array of string-value tuples as a JSON object")
let object_: array<(Js.Dict.key, Js.Json.t)> => Js.Json.t
@ocaml.doc("Encode the given array of JSON values a JSON array")
let array: (array<'a>, 'a => Js.Json.t) => Js.Json.t
@ocaml.doc("Stringify the given JSON type")
let stringify: Js.Json.t => string

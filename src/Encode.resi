let string: string => Js.Json.t
let integer: int => Js.Json.t
let number: float => Js.Json.t
let boolean: bool => Js.Json.t
let null: Js.Null.t<'a> => Js.Json.t
let optional: ('a => Js.Json.t, option<'a>) => Js.Json.t
let object_: array<(Js.Dict.key, Js.Json.t)> => Js.Json.t;
let array: array<Js.Json.t> => Js.Json.t;


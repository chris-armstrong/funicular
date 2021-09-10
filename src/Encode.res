external string: string => Js.Json.t = "%identity"
external integer: int => Js.Json.t = "%identity"
external number: float => Js.Json.t = "%identity"
external boolean: bool => Js.Json.t = "%identity"
external null: Js.Null.t<'a> => Js.Json.t = "%identity"
let undefined: Js.Json.t = %raw(`undefined`)
let optional = (val, mapper) =>
  switch val {
  | Some(defined) => mapper(defined)
  | None => undefined
  }
let object_ = o => o->Js.Dict.fromArray->Js.Json.object_
let array = (arr, mapper) => Js.Json.array(Belt.Array.map(arr, mapper))
let stringify = Js.Json.stringify

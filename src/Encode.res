external string: string => Js.Json.t = "%identity"
external integer: int => Js.Json.t = "%identity"
external number: float => Js.Json.t = "%identity"
external boolean: bool => Js.Json.t = "%identity"
external null: Js.Null.t<'a> => Js.Json.t = "%identity"
let undefined: Js.Json.t = %raw(`undefined`)
let optional = (mapper, val) =>
  switch val {
  | Some(defined) => mapper(defined)
  | None => undefined
  }
let object_ = o => o->Js.Dict.fromArray->Js.Json.object_
let array = Js.Json.array

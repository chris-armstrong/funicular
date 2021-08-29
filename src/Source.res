open Js.Json


module Encode = {
  let string = str => Js.Json.string(str)
  let integer = int => Js.Json.number(Js.Int.toFloat(int))
  let number = fl => Js.Json.number(fl)
  let boolean = bl => Js.Json.boolean(bl)
  let object = o => o->Belt.Array.keepMap(((key, value)) => Belt.Option.map(value, x => (key, x)))->Js.Dict.fromArray->Js.Json.object_
  let array = a => a->Belt.Array.keepMap(item => item)->Js.Json.array
}

module ResultHelpers = {
  let fold = (res, apply) => Belt.Result.map(res, x => apply(x))
  let feed = (mapped, res) => Belt.Result.flatMap(mapped, fold(res))
  let applyTo = mapper => Ok(mapper)
}

@ocaml.doc("Additional JSON parser errors generated by the DecodeAux module")
type jsonParseAuxError<'a> = [>
  | Decode.jsonParseError
  | #DateParseError(string)
] as 'a

@ocaml.doc("Decode the given JSON string value as a JavaScript date (expecting it to be in ISO format). Gives #DateParseError if it cannot be parsed")
let date = (@ocaml.doc("The JSON encoded string") x): result<Js.Date.t, jsonParseAuxError<_>> => {
  open Decode
  switch x {
  | Ok({tree, path}) =>
    switch Js.Json.decodeString(tree) {
    | Some(str) => {
        let dateVal = Js.Date.fromString(str)
        if Js.Float.isNaN(Js.Date.valueOf(dateVal)) {
          Error(#DateParseError(path))
        } else {
          Ok(dateVal)
        }
      }
    | None => Error(#WrongTypeError(path, "string"))
    }
  | Error(error) => Error(error)
  }
}

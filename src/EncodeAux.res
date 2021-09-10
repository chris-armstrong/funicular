@ocaml.doc("Encode the given JavaScript date as an ISO string")
let date = d => d->Js.Date.toISOString->Encode.string

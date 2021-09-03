# railson - simple self-validating JSON parsing for ReScript

Easy-to-use JSON parsing for ReScript that is:
* typesafe 
* easily composable
* idiomatic and readable
* modern and result-based
* low-overhead
* self-validating with meaningful error-output

## Example

```ReScript

type customer = {
  id: int,
  firstName: string,
  lastName: string
}

type 

let decodeCustomer = value => {
  open Railson.Decode;
  let o = value=>object_
  let id = o->
}

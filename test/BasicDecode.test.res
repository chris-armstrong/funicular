open Decode
open Schema
open Zora


zoraBlock("decodeCustomer with valid JSON", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "email": "chris@example.com", "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Ok({
      firstName: "chris",
      lastName: "armstrong",
      email: "chris@example.com",
      points: 323,
      phone: None,
    }),
    "valid customer parse"
  )
})

zoraBlock("decodeCustomer with valid JSON and optional value", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "email": "chris@example.com", "phone": "12345", "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Ok({
      firstName: "chris",
      lastName: "armstrong",
      email: "chris@example.com",
      points: 323,
      phone: Some("12345"),
    }),
    "valid customer parse"
  )
})

zoraBlock("decodeCustomer with missing field value", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "phone": "12345", "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Error(NoValueError("$.email")),
    "invalid customer parse"
  )
})

zoraBlock("decodeCustomer with incorrect field type value", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "email": "chris@chris.com", "phone": 12345, "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Error(WrongType("$.phone", "string")),
    "invalid customer parse"
  )
})

zoraBlock("decodeCustomer with syntax error", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong, "email": "chris@chris.com", "phone": 12345, "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Error(SyntaxError("Unexpected token e in JSON at position 48")),
    "invalid syntax"
  )
  
})

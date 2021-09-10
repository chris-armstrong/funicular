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
    "valid customer parse",
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
    "valid customer parse",
  )
})

zoraBlock("decodeCustomer with missing field value", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "phone": "12345", "points": 323}`
  t->equal(parse(str, decodeCustomer), Error(#NoValueError("$.email")), "invalid customer parse")
})

zoraBlock("decodeCustomer with incorrect field type value", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong", "email": "chris@chris.com", "phone": 12345, "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Error(#WrongTypeError("$.phone", "string")),
    "invalid customer parse",
  )
})

zoraBlock("decodeCustomer with syntax error", t => {
  let str = `{"firstName": "chris", "lastName": "armstrong, "email": "chris@chris.com", "phone": 12345, "points": 323}`
  t->equal(
    parse(str, decodeCustomer),
    Error(#SyntaxError("Unexpected token e in JSON at position 48")),
    "invalid syntax",
  )
})

zoraBlock("decodeCustomer with array", t => {
  let str = `[
    { "firstName": "Chris", "lastName": "armstrong", "email": "chris@example.com", "points": 100},
    { "firstName":"Chris2", "lastName": "Armstrong2", "email": "chris2@example.com", "points": 200}
  ]`
  t->equal(
    parse(str, value => Decode.array(value, decodeCustomer)),
    Ok([
      {
        firstName: "Chris",
        lastName: "armstrong",
        email: "chris@example.com",
        points: 100,
        phone: None,
      },
      {
        firstName: "Chris2",
        lastName: "Armstrong2",
        email: "chris2@example.com",
        points: 200,
        phone: None,
      },
    ]),
    "decoded customer array",
  )
})

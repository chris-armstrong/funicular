open Schema
open Zora

zoraBlock("encode customer without optional", t => {
  let customer = {
    firstName: "Chris",
    lastName: "Armstrong",
    email: "chris@example.com",
    phone: None,
    points: 90,
  }
  t->equal(
    Js.Json.stringify(encodeCustomer(customer)),
    `{"firstName":"Chris","lastName":"Armstrong","email":"chris@example.com","points":90}`,
    "encoded without phone value",
  )
})
zoraBlock("encode customer with optional", t => {
  let customer = {
    firstName: "Chris",
    lastName: "Armstrong",
    email: "chris@example.com",
    phone: Some("12345"),
    points: 90,
  }
  t->equal(
    Js.Json.stringify(encodeCustomer(customer)),
    `{"firstName":"Chris","lastName":"Armstrong","email":"chris@example.com","phone":"12345","points":90}`,
    "encoded with phone value",
  )
})

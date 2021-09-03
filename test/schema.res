type customer = {
  firstName: string,
  lastName: string,
  email: string,
  points: int,
  phone: option<string>,
}

type createCustomerRequest = {
  customer: customer,
  matchOnEmail: bool,
}

let decodeCustomer = json => {
  open Decode
  let o = object_(json)
  let firstName = o->field("firstName", string)
  let lastName = o->field("lastName", string)
  let email = o->field("email", string)
  let phone = o->field("phone", optional(string))
  let points = o->field("points", integer)

  rmap((firstName, lastName, email, phone, points) => {
    firstName: firstName,
    lastName: lastName,
    email: email,
    phone: phone,
    points: points,
  })
  ->v(firstName)
  ->v(lastName)
  ->v(email)
  ->v(phone)
  ->v(points)
}

let encodeCustomer = value => {
  open Encode
  object_([
    ("firstName", string(value.firstName)),
    ("lastName", string(value.lastName)),
    ("email", string(value.email)),
  ])
}

let decodeCustomerRequest = value => {
  open Decode
  let o = value->object_
  rmap((customer, matchOnEmail) => {customer: customer, matchOnEmail: matchOnEmail})
  ->v(o->field("customer", decodeCustomer))
  ->v(o->field("matchOnEmail", boolean))
}

let encodeCustomerRequest = value => {
  open Encode
  object_([
    ("customer", encodeCustomer(value.customer)),
    ("matchOnEmail", boolean(value.matchOnEmail)),
  ])
}


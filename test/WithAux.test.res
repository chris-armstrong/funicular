open Zora

type order = {
  orderNo: int,
  placed: Js.Date.t,
}

type customer = {
  customerNo: int,
  name: string,
  orders: array<order>,
}

let gtZero = x =>
  switch x {
  | Ok(val) => val > 0 ? Ok(val) : Error(#InvalidIdError)
  | Error(val) => Error(val)
  }

let decodeOrder = value => {
  open Funicular.Decode
  open Funicular.DecodeAux
  let o = value->object_
  let orderNo = o->field("orderNo", integer)->gtZero
  let placed = o->field("placed", date)
  rmap((orderNo, placed) => {orderNo: orderNo, placed: placed})->v(orderNo)->v(placed)
}

let decodeCustomer = value => {
  open Funicular.Decode
  let o = value->object_
  let customerNo = o->field("customerNo", integer)->gtZero
  let name = o->field("name", string)
  let orders = o->field("orders", x => array(x, decodeOrder))
  rmap((customerNo, name, orders) => {customerNo: customerNo, name: name, orders: orders})
  ->v(customerNo)
  ->v(name)
  ->v(orders)
}

let encodeOrder = val => {
  open Funicular.Encode
  object_([("orderNo", integer(val.orderNo)), ("placed", Funicular.EncodeAux.date(val.placed))])
}

let encodeCustomer = val => {
  open Funicular.Encode
  object_([
    ("customerNo", integer(val.customerNo)),
    ("name", string(val.name)),
    ("orders", array(val.orders, encodeOrder)),
  ])
}

zoraBlock("should decode with the auxilliary decoders correctly", t => {
  let customerString = `
    {
      "customerNo": 90,
      "name": "Chris A",
      "orders": [
        { "orderNo": 10, "placed": "2020-01-03T00:33:00Z" },
        { "orderNo": 11, "placed": "2020-03-17T16:08:00Z" }
      ]
    }
  `
  let res = Funicular.Decode.parse(customerString, decodeCustomer)
  t->equal(
    res,
    Ok({
      customerNo: 90,
      name: "Chris A",
      orders: [
        {orderNo: 10, placed: Js.Date.fromString("2020-01-03T00:33:00Z")},
        {orderNo: 11, placed: Js.Date.fromString("2020-03-17T16:08:00Z")},
      ],
    }),
    "customer and orders decoded as expected",
  )
})

zoraBlock("should surface errors with auxilliary validation decoder correctly", t => {
  let orderString = `{ "orderNo": -90, "placed": "2020-01-01" }`
  let res = Funicular.Decode.parse(orderString, decodeOrder)
  t->equal(res, Error(#InvalidIdError), "integer should fail validation")
})

zoraBlock("should surface errors with auxilliary date decoder correctly", t => {
  let orderString = `{ "orderNo": 10, "placed": "not a date string" }`
  let res = Funicular.Decode.parse(orderString, decodeOrder)
  t->equal(res, Error(#DateParseError("$.placed")), "date string should not be parsed")
})

zoraBlock("should encode with customer encoders correctly", t => {
  let order = encodeOrder({orderNo: 2, placed: Js.Date.fromString("2020-01-01T00:00:00Z")})
  t->equal(
    Funicular.Encode.stringify(order),
    `{"orderNo":2,"placed":"2020-01-01T00:00:00.000Z"}`,
    "encodes as expected",
  )
})

zoraBlock("should encode customers with embedded orders correctly", t => {
  let customer = {
    customerNo: 9,
    name: "customer 1",
    orders: [{orderNo: 3, placed: Js.Date.fromString("2020-01-05")}],
  }
  t->equal(
    Funicular.Encode.stringify(encodeCustomer(customer)),
    `{"customerNo":9,"name":"customer 1","orders":[{"orderNo":3,"placed":"2020-01-05T00:00:00.000Z"}]}`,
    "correctly encoded customer and order",
  )
})

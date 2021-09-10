# funicular - composable JSON parsing for ReScript

* easily composable using builtin primitives, or extend with your own
* write typesafe and idiomatic parsers
* meaningful error output with result types

## Install

1. Install from npm

```bash
npm i rescript-funicular -S # yarn add rescript-funicular
```

2. Add to your `bsconfig.json`

```
  "bs-dependencies": [
    ...
    "rescript-funicular",
  ]
```

## Example

Define your type:

```rescript
type customer = {
  customerNo: int,
  name: string,
  orders: array<order>,
}
```

And write a decoder function:

```rescript
// Write decoder functions that translate the JSON structure into your fields
let decodeCustomer = value => {
  open Funicular.Decode

  // first decode the value as an object
  let o = value->object_

  // extract and parse each field of the object using the relevant decoder
  let customerNo = o->field("customerNo", integer)
  let name = o->field("name", string)
  // `decodeOrder` is a custom decoder for the `order` type
  let orders = o->field("orders", x => array(x, decodeOrder)) 

  // Use `rmap()` to wrap your builder function, and then feed in each parameter with `v()`
  rmap((customerNo, name, orders) => {customerNo: customerNo, name: name, orders: orders})
  ->v(customerNo)
  ->v(name)
  ->v(orders)
}
```

Use the `Funicular.Decode.parse` function to parse your string, passing in your decoder:

```rescript
// Parse with your custom decoder function for the root object
let customerString = `{"customerNo": 20, "name": "Chris", "orders": [] }`
let myCustomer = Funicular.Decode.parse(customerString, decodeCustomer);
```

Encoding is simpler - a straight type conversion without the error-handling overhead:

```rescript
let encodeCustomer = val => {
  open Funicular.Encode
  object_([
    ("customerNo", integer(val.customerNo)),
    ("name", string(val.name)),
    ("orders", array(val.orders, encodeOrder)),
  ])
}
```

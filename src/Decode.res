open Js.Json

let map_result = Belt.Result.map
let flatMap_result = Belt.Result.flatMap
let identity = x => x

type jsonTreeRef = {
  tree: Js.Json.t,
  path: string,
}

type jsonObjectRef = {
  object: Js.Dict.t<Js.Json.t>,
  path: string,
}

type jsonParseError = [
  | #SyntaxError(string)
  | #WrongTypeError(string, string)
  | #NoValueError(string)
]
type jsonParseResult<'a, 'b> = result<'a, [> jsonParseError] as 'b>
type jsonTreeResult<'c> = result<jsonTreeRef, [> jsonParseError] as 'c>
type parser<'a, 'b> = jsonTreeResult<'b> => result<'a, 'b>

/* * Helper to convert Option to Result */
let mapOptionWithError = (~mapper: 'a => 'b, opt, error: [> jsonParseError] as 'c): result<
  'b,
  'c,
> => Belt.Option.mapWithDefault(opt, Error(error), x => Ok(mapper(x)))

let jsonParseErrorToString = error =>
  switch error {
  | #SyntaxError(error) => `Syntax Error: ${error}`
  | #WrongTypeError(path, expected) => `Wrong Type Error: ${expected} was expected at path ${path}`
  | #NoValueError(path) => `No Value Error: expected a value at ${path}`
  | _ => `Unknown error`
  }

let parse = (jsonString: string, rootParser) => {
  let treeResult = try Ok(parseExn(jsonString)) catch {
  | Js.Exn.Error(payload) =>
    Error(#SyntaxError(Belt.Option.getWithDefault(Js.Exn.message(payload), "unknown")))
  }
  flatMap_result(treeResult, tree => rootParser(Ok({tree: tree, path: "$"})))
}

let object_ = (x: jsonTreeResult<'b>): jsonParseResult<jsonObjectRef, 'b> =>
  x
  ->map_result(({tree, path}) => (decodeObject(tree), path))
  ->flatMap_result(((dictOption, path)) =>
    mapOptionWithError(dictOption, #NoValueError(path), ~mapper=object => {
      object: object,
      path: path,
    })
  )

let string = x =>
  x
  ->map_result(({tree, path}) => (decodeString(tree), path))
  ->flatMap_result(((stringOption, path)) =>
    mapOptionWithError(stringOption, #WrongTypeError(path, "string"), ~mapper=identity)
  )
let number = x =>
  x
  ->map_result(({tree, path}) => (decodeNumber(tree), path))
  ->flatMap_result(((numberOption, path)) =>
    switch numberOption {
    | Some(num) => Ok(num)
    | None => Error(#WrongTypeError(path, "number"))
    }
  )
let integer = x =>
  x
  ->map_result(({tree, path}) => (decodeNumber(tree), path))
  ->flatMap_result(((numberOption, path)) =>
    mapOptionWithError(numberOption, #WrongTypeError(path, "integer"), ~mapper=num =>
      Belt.Float.toInt(num)
    )
  )
let boolean = x =>
  x
  ->map_result(({tree, path}) => (decodeBoolean(tree), path))
  ->flatMap_result(((boolOption, path)) =>
    mapOptionWithError(boolOption, #WrongTypeError(path, "boolean"), ~mapper=identity)
  )

let null = x =>
  x
  ->map_result(({tree, path}) => (decodeNull(tree), path))
  ->flatMap_result(((nullOption, path)) =>
    mapOptionWithError(nullOption, #WrongTypeError(path, "null"), ~mapper=identity)
  )

let array = (
  arrayRef: result<jsonTreeRef, 'b>,
  itemParser: parser<'a, [> jsonParseError] as 'b>,
): result<array<'a>, 'b> => {
  switch arrayRef {
  | Ok({tree, path}) =>
    switch decodeArray(tree) {
    | Some(arr) =>
      Belt.Array.reduceWithIndex(arr, Ok([]), (progress, next, i) =>
        flatMap_result(progress, items => {
          let record = itemParser(Ok({path: `${path}.${Belt.Int.toString(i)}`, tree: next}))
          switch record {
          | Ok(recordValue) => Ok(Belt.Array.concat(items, [recordValue]))
          | Error(error) => Error(error)
          }
        })
      )
    | None => Error(#WrongTypeError(path, "array"))
    }
  | Error(error) => Error(error)
  }
}

let field = (
  objectRef: result<jsonObjectRef, 'b>,
  fieldName: string,
  mapper: parser<'a, [> jsonParseError] as 'b>,
): result<'a, 'b> => {
  mapper(switch objectRef {
  | Ok({object, path}) => {
      let newPath = `${path}.${fieldName}`
      switch Js.Dict.get(object, fieldName) {
      | Some(fieldValue) => Ok({tree: fieldValue, path: newPath})
      | None => Error(#NoValueError(newPath))
      }
    }
  | Error(error) => Error(error)
  })
}

let optional = (
  mapper: parser<'a, [> jsonParseError] as 'b>,
  decodedResult: result<jsonTreeRef, 'b>,
): result<option<'a>, 'b> =>
  switch decodedResult {
  | Ok(value) =>
    switch mapper(Ok(value)) {
    | Ok(result) => Ok(Some(result))
    | Error(error) => Error(error)
    }
  | Error(error) =>
    switch error {
    | #NoValueError(_) => Ok(None)
    | _ => Error(error)
    }
  }

let any = x => x->map_result(({tree}) => tree)

let v = (mapped: result<'a => 'b, 'c>, res: result<'a, 'c>): result<'b, 'c> =>
  Belt.Result.flatMap(mapped, apply => Belt.Result.map(res, apply))

let rmap = mapper => Ok(mapper)

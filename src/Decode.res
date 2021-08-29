open Js.Json;

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

type jsonParseError =
  | SyntaxError(string)
  | WrongType(string, string)
  | NoValueError(string)
  | RecordParseError(string, string)
  | CustomError(string)

type jsonParseResult<'a> = Belt.Result.t<'a, jsonParseError>
type jsonTreeResult = Belt.Result.t<jsonTreeRef, jsonParseError>
type parser<'a> = jsonTreeResult => jsonParseResult<'a>
type recordParser<'a> = (string, jsonTreeResult) => jsonParseResult<'a>;

/** Helper to convert Option to Result */
let mapOptionWithError = (~mapper: 'a => 'b, opt, error): jsonParseResult<'b> =>
  Belt.Option.mapWithDefault(opt, Error(error), x => Ok(mapper(x)))

/** Helper to convert Option to Result */
let flatMapOptionWithError = (~mapper: 'a => jsonParseResult<'b>, opt, error): jsonParseResult<'b> =>
  switch opt {
    | Some(value) => mapper(value)
    | None => Error(error)
  }

let jsonParseErrorToString = error =>
  switch error {
  | SyntaxError(error) => `Syntax Error: ${error}`
  | WrongType(path, expected) => `Wrong Type Error: ${expected} was expected at path ${path}`
  | NoValueError(path) => `No Value Error: expected a value at ${path}`
  | RecordParseError(path, suberror) =>
    `Record parse error: at path ${path} received record parse error - ${suberror}`
  | CustomError(error) => `Other parse error: ${error}`
  }

let parse = (jsonString, rootParser) => {
  let treeResult = try Ok(parseExn(jsonString)) catch {
  | Js.Exn.Error(payload) =>
    Error(SyntaxError(Belt.Option.getWithDefault(Js.Exn.message(payload), "unknown")))
  }
  flatMap_result(treeResult, tree => rootParser(Ok({tree: tree, path: "$"})))
}


let object_ = (x: jsonTreeResult) =>
  x
  ->map_result(({tree, path}) => (decodeObject(tree), path))
  ->flatMap_result(((dictOption, path)) =>
    mapOptionWithError(dictOption, NoValueError(path), ~mapper=object => {
      object: object,
      path: path,
    })
  )

let record = (recordObject: jsonTreeResult, recordParser: recordParser<'a>): jsonParseResult<array<'a>> =>
  recordObject
  ->object_
  ->map_result(({object, path}) => (Js.Dict.entries(object), path))
  ->flatMap_result(((entries, path)) =>
    Belt.Array.reduce(entries, Ok([]), (records, (key, value)) =>
      flatMap_result(records, recordsValue => {
        let record = recordParser(key, Ok({path: `${path}.${key}`, tree: value}))
        switch record {
        | Ok(recordValue) => Ok(Belt.Array.concat(recordsValue, [recordValue]))
        | Error(error) =>
          switch error {
          | CustomError(y) => Error(RecordParseError(`${path}.${key}`, y))
          | _ => Error(error)
          }
        }
      })
    )
  )

let string = x =>
  x
  ->map_result(({tree, path}) => (decodeString(tree), path))
  ->flatMap_result(((stringOption, path)) =>
    mapOptionWithError(stringOption, WrongType(path, "string"), ~mapper=identity)
  )
let number = x =>
  x
  ->map_result(({tree, path}) => (decodeNumber(tree), path))
  ->flatMap_result(((numberOption, path)) =>
    switch numberOption {
    | Some(num) => Ok(num)
    | None => Error(WrongType(path, "number"))
    }
  )
let integer = x =>
  x
  ->map_result(({tree, path}) => (decodeNumber(tree), path))
  ->flatMap_result(((numberOption, path)) =>
    mapOptionWithError(numberOption, WrongType(path, "integer"), ~mapper=num =>
      Belt.Float.toInt(num)
    )
  )
let boolean = x =>
  x
  ->map_result(({tree, path}) => (decodeBoolean(tree), path))
  ->flatMap_result(((boolOption, path)) =>
    mapOptionWithError(boolOption, WrongType(path, "boolean"), ~mapper=identity)
  )

let null = x =>
  x
  ->map_result(({tree, path}) => (decodeNull(tree), path))
  ->flatMap_result(((nullOption, path)) =>
    mapOptionWithError(nullOption, WrongType(path, "null"), ~mapper=identity)
  )

let array = (arrayRef: jsonTreeResult, itemParser: parser<'a>): jsonParseResult<array<'a>> =>
  arrayRef
  ->map_result(({tree, path}) => (decodeArray(tree), path))
  ->flatMap_result(((arrayOption, path)) =>
    flatMapOptionWithError(arrayOption, WrongType(path, "array"), ~mapper=arr =>
      Belt.Array.reduceWithIndex(arr, Ok([]), (progress, next, i) =>
        flatMap_result(progress, items => {
          let record = itemParser(Ok({path: `${path}.${Belt.Int.toString(i)}`, tree: next}))
          switch record {
            | Ok(recordValue) => Ok(Belt.Array.concat(items, [recordValue]))
            | Error(error) => Error(error)
          }
        })
      )
    )
  )

let field = (objectRef: jsonParseResult<jsonObjectRef>, fieldName: string, parser: parser<'a>): jsonParseResult<'a> =>
  objectRef
  ->map_result(({object, path}) => (Js.Dict.get(object, fieldName), path))
  ->flatMap_result(((fieldValueOption, path)) =>
    mapOptionWithError(
      fieldValueOption,
      NoValueError(`${path}.${fieldName}`),
      ~mapper=fieldValue => {tree: fieldValue, path: `${path}.${fieldName}`},
    )
  )
  ->parser

let optional = (decodedResult: jsonTreeResult, mapper: parser<'a>): jsonParseResult<option<'a>> =>
  switch decodedResult {
  | Ok(value) =>
    switch mapper(Ok(value)) {
    | Ok(result) => Ok(Some(result))
    | Error(error) => Error(error)
    }
  | Error(error) =>
    switch error {
    | NoValueError(_) => Ok(None)
    | _ => Error(error)
    }
  }

let any = x => x->map_result(({tree}) => tree)

let fold = (res, apply) => Belt.Result.map(res, x => apply(x))
let v = (mapped, res) => Belt.Result.flatMap(mapped, fold(res))
let rmap = mapper => Ok(mapper)

[<AutoOpen>]
module internal Recall.Util

let inline constant x _ = x

let inline (|Just|Nothing|) (got, x) = if got then Just x else Nothing

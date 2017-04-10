let (%) f g x = f (g x)

let id a = a
let const a b = a
let flip f = fun a b -> f b a

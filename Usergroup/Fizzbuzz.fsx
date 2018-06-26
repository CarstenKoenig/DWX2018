

// Fizz Buzz

for n in [1..15] do
  if n % 15 = 0 then
    printfn "FizzBuzz"
  elif n % 3 = 0 then
    printfn "Fizz"
  elif n % 5 = 0 then
    printfn "Buzz"
  else
    printfn "%d" n













let fizzBuzz n =
  if n % 15 = 0 then
    "FizzBuzz"
  elif n % 3 = 0 then
    "Fizz"
  elif n % 5 = 0 then
    "Buzz"
  else
    string n

[1 .. 15]
|> List.map fizzBuzz
|> List.iteri (printfn "%d -> %s")











let fizzBuzz2 n =
  match (n%3, n%5) with
  | (0,0) -> "FizzBuzz"
  | (_,0) -> "Buzz"
  | (0,_) -> "Fizz"
  | _     -> string n

[1 .. 15]
|> List.map fizzBuzz2
|> List.iteri (printfn "%d -> %s")
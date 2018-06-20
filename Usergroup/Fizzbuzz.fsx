

// Fizz Buzz

// if is just an expression
let fizzBuzz n =
  if n % 15 = 0 then
    "FizzBuzz"
  elif n % 3 = 0 then
    "Fizz"
  elif n % 5 = 0 then
    "Buzz"
  else
    string n

// match it
let fizzBuzz2 n =
  match (n%3, n%5) with
  | (0,0) -> "FizzBuzz"
  | (_,0) -> "Buzz"
  | (0,_) -> "Fizz"
  | _     -> string n

// functional programmers loop
[1 .. 100]
|> List.map fizzBuzz2
|> List.iteri (printfn "%d -> %s")
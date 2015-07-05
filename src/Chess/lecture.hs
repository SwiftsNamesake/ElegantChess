--
-- Learning Haskell
--
-- January 7 2015
--

-- Modules (import, declare, qualified, as, hiding, Prelude)
import Data.Char (toUpper)

-- Defining a function
twice x = x * 2

-- Defining a value
π = 3.14159265
numbers = map twice [1..]
hello = "World"

-- List comprehensions, ranges
natural   = [1..]
third     = [0, 3..1000]
cubes     = [n^3 | n <- [1..20] ]
evenCubes = [n^3 | n <- [1..20], n^3 `mod` 2 == 0]

-- Type annotations
x = 5 :: Float

double :: Num a => a -> a
double x = x * 2

exclaim :: String -> Int -> String
exclaim message n = message ++ replicate n '!'

-- Strong, static typing, conversions


-- Scoped declarations (let, where)
hypotenuse a b = sqrt $ aa + bb
	where aa = a**2
	      bb = b**2

-- Guards, patterns, case of

-- Recursion
map' f [] = []
map' f (x:rest) = f x : map' f rest

-- Currying (partial application)


-- Higher order functions


-- Lambdas


-- newtype, data


-- classes (constraints)


-- monads


-- Entry point
main :: IO ()
main = do
	putStrLn "Hola, Mundo!"
	print $ take 15 numbers
	print $ map' toUpper "Jonatan"
	print $ hypotenuse 3 4
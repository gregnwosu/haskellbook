module CoreDump where


data Test =
    A Test2
  | B Test2
    deriving (Show)

data Test2 =
    C Int
    | D Int
      deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

discriminatory :: Bool -> Int
discriminatory b = case b of
                     False -> 0
                     True -> 1

-- exercises
--1. const 1 undefined  =
-- \_ -> 1 undefined =
-- case undefined { _ -> 1 } = 1
--2. const undefined 1 = \_ -> undefined 1
-- = case 1 {_ -> undefined} = undefined
-- 3 flip const undefined 1 = const 1 undefined =
-- \_ -> 1 undefined = case undefined {_ -> 1 } = 1
-- 4. flip const 1 undefined = const undefined 1 =
-- \_ -> undefined 1 = case 1 { _ -> undefined} = undefined
-- const undefined underfined = \_ -> undefined undefined = case undefined { _ -> undefined} = undefined
-- 6. foldr const 'z' ['a'..'e'] = const (const (const (const (const 'a' 'z') 'b') 'c') 'd') 'e'
-- = const (const (const (const  'a'  'b') 'c') 'd') 'e'
-- = const (const (const  'a'   'c') 'd') 'e'
-- = const (const  'a' 'd') 'e'
-- = const  'a' 'e'
-- = 'a'
-- 7. foldr (flip const) 'z' ['a'..'e'] = (const (const (const (const (const 'z' 'a') 'b') 'c') 'd') 'e')
-- = (const (const (const (const  'z'  'b') 'c') 'd') 'e')
-- = (const (const (const   'z' 'c') 'd') 'e')
-- =  (const (const  'z' 'd') 'e')
-- = (const   'z'  'e')
-- = 'z'

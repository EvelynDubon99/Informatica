module Main exposing (..)
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith l bs cs  = 
    case (bs, cs) of
    ([], _) -> []
    (_, []) -> []
    (b::bss, c::css) -> l b c ::zipWith l bss css

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy n list = (funciona n list, nofunciona n list)

funciona s list = case list of 
    [] -> []
    (b::bss) -> if s b then funciona s bss else b:: funciona s bss

nofunciona s list = case list of 
    [] -> []
    (l::lss) -> if s l then l:: nofunciona s lss else nofunciona s lss

bind : Maybe a1 -> (a1 -> Maybe a) -> Maybe a
bind ls f = case ls of 
    Nothing -> Nothing 
    Just a -> f a 
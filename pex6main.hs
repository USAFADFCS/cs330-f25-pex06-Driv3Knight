-- pex6.hs
-- unKnot Haskell

-- name: C2C Enoch Jung

{- DOCUMENTATION: Used https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html#:~:text=Colon%20operator%3A%20This%20is%20very,and%20returns%20a%20new%20list).&text=List%20comprehension%3A%20If%20you%20are,list%20comprehensions%20to%20construct%20lists.
- also this: https://stackoverflow.com/questions/1696751/what-does-the-infix-operator-do-in-haskell
- Helped a lot with understanding the ':' operator which was really fundamental in my implementation
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = " unknot"
   | hasType1 tripCode = unKnot (fixType1 tripCode)
   | hasType2 tripCode = unKnot (fixType2 tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode

main :: IO ()
main = do
   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " ) -- "Unkot"
   print(t02)
   print("   result:" ++ unKnot t02) -- "Unkot"
 
   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03) -- "Unkot"

hasType1 :: [(Char, Char)] -> Bool
-- if nothing, false
hasType1 [] = False
-- if only one thing, false
hasType1 [_] = False
-- splits first and second elements from the list
hasType1 ((x1, y1):(x2, y2):end)
   -- drops the first two elements because its a tangle
   | x1 == x2 = True
   -- if not a tangle goes to the next
   | otherwise = hasType1 ((x2, y2):end)

fixType1 :: [(Char, Char)] -> [(Char, Char)]
fixType1 [] = []
fixType1 [a] = [a]
-- splits first and second elements from the list
fixType1 ((x1, y1):(x2, y2):end)
   -- basically only returning the tail of the list if x1 = x2
   | x1 == x2 = end
   -- if not move on to the next pair and compare
   | otherwise = (x1, y1) : fixType1 ((x2, y2):end)

hasType2 :: [(Char, Char)] -> Bool
-- empty list, nothing to fix
hasType2 [] = False
-- only one element, nothing to fix
hasType2 [_] = False
-- if true means that fixType2 found a type2 case,  if false means that nothing was found
hasType2 x = x /= fixType2 x

hasType2Pair1 :: (Char, Char) -> (Char, Char) -> Bool
-- true if the crossings are different but type of crossing is the same
hasType2Pair1 (x1, y1) (x2, y2) = (y1 == y2) && (x1 /= x2)

hasType2Pair2 :: (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> Bool
-- takes two sets of pairs -> one from the pair1 function and a new pair -> conditionals check if this is actually a type 2 scenario
hasType2Pair2 (x1, y1) (x2, y2) (a1, b1) (a2, b2) = (b1 == b2 && b1 /= y1) && ((a1 == x1 && a2 == x2) || (a1 == x2 && a2 == x1))

fixType2 :: [(Char, Char)] -> [(Char, Char)]
-- empty, nothing to fix
fixType2 [] = []
-- one element nothing to fix
fixType2 [a] = [a]
-- splits into first and second elements (the two pairs we are observing) and the rest of the list
fixType2 ((x1, y1):(x2, y2):end)
   -- if its a valid type 2 pair goes through the list to find another pair
   | hasType2Pair1 (x1, y1) (x2, y2) = fixPair (x1, y1) (x2, y2) [] end
   -- if not recurse through the list for possible type 2
   | otherwise = (x1,y1) : fixType2 ((x2,y2):end)

fixPair :: (Char, Char) -> (Char, Char) -> [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
-- end of the list -> havent found anything substantial so the pair is not actually a type 2 -> basically get rid of the first pair, the second pair becomes the new first pair and recurse
fixPair (x1, y1) (x2, y2) between [] = (x1, y1) : fixType2 ((x2, y2) : between)
-- one element in the end of the list left -> first pair is probably not a type 2 -> stop using first pair and recurse with the second pair with everything from the between list and the last element
fixPair (x1, y1) (x2, y2) between [p] = (x1, y1) : fixType2 ((x2, y2) : (between ++ [p]))
-- there are at least two or more elements left
fixPair (x1, y1) (x2, y2) between ((a1, b1):(a2, b2):end)
   -- something was found -> remove all four elements by returning only between and the end lists mashed up (with ++)
   | hasType2Pair2 (x1, y1) (x2, y2) (a1, b1) (a2, b2) = between ++ end
   -- no match, continue to recurse
   | otherwise = fixPair (x1, y1) (x2, y2) (between ++ [(a1, b1)]) ((a2, b2):end)
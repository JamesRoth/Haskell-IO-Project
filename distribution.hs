{-
    Write and submit a Haskell program (distribution.hs) that computes and displays 
    the distribution of characters in a given sample of text.
    
    Output of your program should look like this:
    
    Please enter a string of text (the bigger the better): 
    The rain in Spain stays mainly in the plain.
    The distribution of characters in "The rain in Spain stays mainly in the plain." is:
    iiiiii
    nnnnnn
    aaaaa
    sss
    ttt
    ee
    hh
    ll
    pp
    yy
    m
    r
    
    Notice about this example:
    * The text: 'The rain ... plain' is provided by the user as input to your program.
    * Uppercase characters are converted to lowercase
    * Spaces and punctuation marks are ignored completely.
    * Characters that are more common appear first in the list.
    * Where the same number of characters occur, the lines are ordered alphabetically. 
      For example, in the printout above, the letters e, h, l, p and y both occur twice 
      in the text and they are listed in the output in alphabetical order.
    * Letters that do not occur in the text are not listed in the output at all.
-}

import Data.Char (toLower)

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

lengthsort [] = []  
lengthsort (x:xs) =   
    let smallerSorted = lengthsort [a | a <- xs, compareString a x]  
        biggerSorted = lengthsort [a | a <- xs, not (compareString a x)]  
    in  smallerSorted ++ [x] ++ biggerSorted  
    
compareString a b = 
    if length(a) == length(b)
        then a>b
    else length(a) <= length(b)
    

concatLike :: [String] -> [String] -> [String]
concatLike [] list = list
concatLike (x:xs) list = 
    if last(x) == last(last list)
      then (concatLike xs (init(list) ++ [last(list) ++ x]))
    else concatLike xs (list ++ [x])

filterList' w = [ x | x <- list, elem (head(x)) (['a'..'z']++['A'..'Z'])]
  where list = map (:[]) w

printList list = mapM_ putStrLn list

main = do
    putStrLn "Please enter a string!"
    str <- getLine
    let modstr =  quicksort (filterList' (map toLower str))
    --putStrLn modstr
    --putStrLn (tail modstr)
   -- putStrLn (words (head modstr))
    let modstr' = concatLike (tail modstr) (words(head modstr))
    
    printList (reverse(lengthsort(modstr')))


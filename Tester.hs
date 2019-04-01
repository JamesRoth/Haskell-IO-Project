concatLike :: [String] -> [String] -> [String]
concatLike [] list = list
concatLike (x:xs) list = 
    if last(x) == last(last list)
      then (concatLike xs (init(list) ++ [last(list) ++ x]))
    else concatLike xs (list ++ [x])

printList list = mapM_ putStrLn list
import Data.Char

isPalindrome :: String -> Bool
isPalindrome s = let voca = map toLower (filter isAlphaNum s) in 
                                            voca == reverse(voca)
main :: IO ()
main = do
    putStrLn "Please enter a phrase:  "
    s <- getLine
    if isPalindrome s
        then putStrLn "yes"
        else putStrLn "no"
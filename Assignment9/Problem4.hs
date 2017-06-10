--Problem 4
--4-a
forever :: IO () -> IO ()
forever a = do a >> forever a

--4-b
repeatN :: Int -> IO() -> IO()
repeatN 0 a = return ()
repeatN n a = do a >> repeatN (n-1) a

--4-c
--FIXME
each :: [IO a] -> IO [a]
each [] = return []
each (a:as) = do y <- a
                 ys <- each as
                 return (y:ys)

--4-d
--Answer in palindrome.hs

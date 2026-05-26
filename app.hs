module Main where

strConvert :: String -> String
strConvert str = "<p>" ++ str ++ "</p>"


processStr :: String -> String
processStr bruteStr = unlines (map strConvert (lines bruteStr))


main :: IO ()
main = do
    content <- readFile "content/post.txt"
    
    let htmlFinal = processStr content

    writeFile "dist/post.html" htmlFinal

    putStrLn "Site estático gerado com sucesso em dist/post.html" 


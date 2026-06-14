module Main where
import System.Directory (listDirectory)
import Data.List(isPrefixOf, isSuffixOf)


-- Conversor de String para parágrafo <p>
strConvert :: String -> String
strConvert str 
    -- Títulos
    | "### " `isPrefixOf` str = "<h3>" ++ drop 4 str ++ "</h3>"
    | "## " `isPrefixOf` str = "<h2>" ++ drop 3 str ++ "</h2>"
    | "# "  `isPrefixOf` str = "<h1>" ++ drop 2 str ++ "</h1>"
    -- Imagens
    | "#img " `isPrefixOf` str = "<div class=\"post-image\"><img src=\"" ++ drop 5 str ++ "\"></div>"
    | str == "#- " || "#-" `isPrefixOf` str = "<hr>"
    --Parágrafo
    | otherwise = "<p>" ++ str ++ "</p>"

-- Mapear e converter a String bruta
applyContent :: String -> String
applyContent bruteStr = unlines (map strConvert (lines bruteStr))

searchTitle :: [String] -> String
searchTitle metadataLines =

    -- Extrai um título inserido pelo usuário no frontmatter
    let titleLine = filter (\l -> take 7 l == "titulo:") metadataLines
    in if null titleLine
        then "Post Sem Título" -- Se não existe título inserido pelo usuário
        else extractMetadata (head titleLine)

searchDate :: [String] -> String
searchDate metadataLines =

    let dateLine = filter (\l -> take 5 l == "data:") metadataLines
    in if  null dateLine
        then "" -- Caso não encontre data, não colocar nada
        else extractMetadata(head dateLine)

extractMetadata :: String -> String
extractMetadata line = drop 2 (snd (break (== ':') line))


-- Compilar todos os arquivos em um .html só
applyTemplate :: String -> String -> String -> String -> String -> String
applyTemplate header title content footer date
    | not (null date) = header ++ "<title>" ++ title ++ "</title></head><body>" ++ content ++ footer ++ "<div><b>Gerado em: " ++ date ++ "</b></div></p></footer></body>"
    
    | otherwise = header ++ "<title>" ++ title ++ "</title></head><body>" ++ content ++ footer ++ "</p></body></html>"
    



-- Processar arquivos para leitura
processFile :: String -> String -> FilePath -> IO ()
processFile header footer fileName = do
    rawContent <- readFile ("content/" ++ fileName)


    --- Extrai Metadados da String (Frontmatter)
    let noMarker = tail (lines rawContent)
    let purePostText = tail (dropWhile (\linha -> linha /= "---") noMarker) 
    let postText = unlines purePostText 

    let metadataLines = takeWhile (\line -> line /= "---") noMarker
    let setTitle = searchTitle metadataLines
    let setDate = searchDate metadataLines
    --- Aplica o conteúdo da String e o template no arquivo .html
    let htmlContent = applyContent postText
    let htmlFinal = applyTemplate header setTitle htmlContent footer setDate

    let htmlFileName = takeWhile(/= '.') fileName

    writeFile ("dist/" ++ htmlFileName ++ ".html") htmlFinal


main :: IO ()
main = do
    
    
    templateHeader <- readFile "templates/header.txt"
    templateFooter <- readFile "templates/footer.txt"   
    
    htmlFile <- listDirectory "content"
    
    let validFiles = filter (\file -> ".txt" `isSuffixOf` file) htmlFile

    mapM_ (processFile templateHeader templateFooter) validFiles

    putStrLn "Todos os sites estáticos gerado com sucesso em dist/" 




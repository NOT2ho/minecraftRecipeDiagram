import Data.Set ( Set, fromList )
import System.FilePath
import System.Directory (listDirectory, getDirectoryContents, getCurrentDirectory)

type Graph a = [Node a] 
data Node a = NODE a [a]
    deriving (Show, Functor, Foldable, Traversable)

pmap :: (a -> b) -> [a] -> [b]
pmap = Prelude.map

main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    files <- listDirectory (currentDir </> "recipes")
    let dirs = map ( (currentDir </>).("recipes" </>) ) files
    recipes <- mapM io dirs
    let recp = map posetizer $ lengthFilter 2 recipes
    let roots = findRoots recp (ings recp)
    print recp
    

parser :: String -> [String]
parser s = recipe $ cutter s

cutter :: String -> [String]
cutter (c:cs) = case c of
    '"' -> takeWhile (/= '"') cs : cutter (dropWhile (=='"') (dropWhile (/= '"') cs))
    _ -> cutter cs
cutter [] = []

recipe :: [String] -> [String]
recipe (s:ss)
    | s == "key" = takeWhile (/="pattern") ss ++ dropWhile (/="pattern") ss
    | s == "id" = head ss : recipe ss
    | otherwise = recipe ss
recipe [] = []


lengthFilter :: Foldable t => Int -> [t a] -> [t a]
lengthFilter i = filter (\x -> length x >= i)

io :: String -> IO [String]
io dir= do
    json <- readFile dir
    return $ parser json


posetizer :: [String] -> Node String
posetizer l = let (s:ss) = (map name. reverse) $ lengthFilter 10 l
            in NODE s ss

ings :: Graph String -> Set String
ings s = fromList $ concatMap (\(NODE a b) -> b) s

findRoots :: Graph String -> Set String -> [Node String]
findRoots s ings = filter (\(NODE a b) -> a `notElem` ings) s

name :: String -> String
name = drop 1 . dropWhile (/= ':')

svg :: IO ()
svg = do 
    currentDir <- getCurrentDirectory
    let dir = currentDir </> "file.svg"
    writeFile dir svgxml

--todo
svgxml :: String
svgxml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <!DOCTYPE svg  PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'> \n <svg version=\"1.1\" viewBox=\"0 0 2000 2000\" xml:space=\"preserve\" xmlns=\"http://www.w3.org/2000/svg\"> \n \t <path d=\"\"/> \n </svg>"
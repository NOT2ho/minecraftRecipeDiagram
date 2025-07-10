{-# LANGUAGE ParallelListComp #-}

import Data.Set ( Set, fromList, unions, union, toList )
import System.FilePath
import System.Directory (listDirectory, getDirectoryContents, getCurrentDirectory)
import Data.List (sort, groupBy)
import GHC.List (foldl1')
import Data.Bits
import Data.Map (Map, fromList, findWithDefault)
import Numeric (showHex)

type Graph a = [Node a]
data Node a = NODE a [a]
    deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

pmap :: (a -> b) -> [a] -> [b]
pmap = Prelude.map

sFromList :: Ord a => [a] -> Set a
sFromList = Data.Set.fromList

main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    files <- listDirectory (currentDir </> "recipes")
    let dirs = map ( (currentDir </>).("recipes" </>) ) files
    recipes <- mapM io dirs
    let recp = link recipes
    let loop = loops recp
    let roots = findRoots recp
    svg $ graphtoSVG 50 4 9 recp


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
    | s == "ingredients" = takeWhile (/="results") ss ++ dropWhile (/="results") ss
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

link :: [[String]] -> Graph String
link l = merge $ map posetizer $ lengthFilter 2 l

ings :: Ord a => Graph a -> Set a
ings s = sFromList $ concatMap (\(NODE a b) -> b) s

findRoots :: Graph String -> ([Node String], Graph String)
findRoots s
    | null s = ([],[])
    | otherwise = let ing = ings s
                    in (filter (\(NODE a b) -> a `notElem` ing) s, filter (\(NODE a b) -> a `elem` ing) s)


next :: Eq a => ([Node a], Graph a) -> ([Node a], Graph a)
next (ns,g) = let l = concatMap (\(NODE a b)-> b) ns
            in (filter (\(NODE a b) -> a `elem` l) g, filter (\(NODE a b) -> a `notElem` l) g)

name :: String -> String
name = drop 1 . dropWhile (/= ':')

svg :: [String] -> IO ()
svg l  = do
    currentDir <- getCurrentDirectory
    let dir = currentDir </> "file.svg"
    writeFile dir $ svgxml l

svgxml :: [String] -> String
svgxml l = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <!DOCTYPE svg  PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'> \n <svg version=\"1.1\" viewBox=\"0 0 20000 20000\" xml:space=\"preserve\" xmlns=\"http://www.w3.org/2000/svg\"> \n \t" ++ concat l ++ "\n </svg>"

nexts :: Eq a => ([Node a], Graph a) -> [[Node a]]
nexts (ns, g) = if null ns then [] else ns : nexts (next (ns, g))

(*=) :: (Eq a) => Node a -> Node a -> Bool
(*=) (NODE a l) (NODE b l') = a == b

(*+) :: Node a -> Node a -> Node a
(*+) (NODE a l) (NODE b l') = NODE a (l++l')

(*+=) :: (Eq a) => Node a -> Node a -> Either (Node a) (Node a, Node a)
(*+=) a b = if a *= b then Left (a *+ b) else Right (a , b)

(-*-) :: (Eq a) => Node a -> Node a -> [Node a]
(-*-) a b = either (:[]) (\(x, y) -> [x,y]) (a *+= b)

merge :: Graph String -> Graph String
merge g = let gs =groupBy (*=) {- sort if it needed -} g
            in map (foldl1 (*+)) gs

hd :: Node a -> a
hd (NODE x l) = x

tl :: Node a -> [a]
tl (NODE x l) = l

nodeName :: Node String -> String
nodeName (NODE a l) = a

text :: String -> Int ->  String -> String -> Int -> Int  -> String
text font size color s x y = "<text x=\"" ++
        show x ++ "\" y=\"" ++ show y ++ "\"\n"
        ++ "\t\tfont-family=\"" ++ font ++ "\" font-size=\"" ++ show size ++ " \" fill=\"" ++ color ++ "\" >\n"
        ++ "\t" ++ s
        ++ "\n</text>\n"

isLoop :: Ord a => Graph a -> Node a -> Bool
isLoop g n = hd n `elem` concat (foldl1 (++) ((map . map) tl $ nexts ([n], g)))

loops :: Ord a => Graph a -> [Node a]
loops g = filter (isLoop g) g

xorshift32 :: (Num a, Bits a) => a -> a
xorshift32 seed =
    let l13seed = seed .^. (seed .<<. 13) in
    let r17seed = l13seed .^. (l13seed .>>. 17) in
    let l5seed = r17seed .^. (r17seed .<<. 5) in
        l5seed .&. 0xFFFFFFFF

xorshift32inf :: Int -> [Float]
xorshift32inf seed= map ((/ 4294967295) . fromIntegral) $ iterate xorshift32 seed

randomIntTuples :: Int -> Int -> Float -> [(Int, Int)]
randomIntTuples r1 r2 s = zip (map (round . (*s)) $ xorshift32inf r1) (map (round . (*s)) $ xorshift32inf r2)

-- nodetoSVG :: Show a => Int -> Node a -> (Int, Int) -> String
-- nodetoSVG i (NODE a l) (r1, r2)  = text "Super Sans" i "blue" r1 r2 (show a)

path :: (Int, Int) -> Int -> String -> (Int, Int) -> String
path (x,y) stroke color (x', y')  = "<path d=" ++ "\"M " ++ show x ++ " " ++ show y ++ " " ++ "L " ++ show x' ++ " " ++ show y' ++ " "++ "z\"\n"
       ++  "\tstroke=\"" ++ color ++ "\" stroke-width=\"" ++ show stroke ++ "\"/>\n"

coord :: Ord a => Int -> Int -> Graph a -> Map a (Int, Int)
coord i j g = let all = toList $ sFromList (concatMap tl g) `union` sFromList (map hd g)
            in Data.Map.fromList [(a , b) | a <- all | b <- randomIntTuples i j 10000]


graphtoSVG ::  (Ord a, Show a) => Int ->  Int -> Int -> Graph a -> [String]
graphtoSVG s i j g = let c = coord i j g
                    in concatMap (\x -> map
                       (path (findWithDefault (0, 0) (hd x) c) 2 (colorCode $ fst (findWithDefault (0, 0) (hd x) c))
                          . flip (findWithDefault (0, 0)) c)
                       (tl x)) g
                       ++ map (\x -> text "Super Sans" s "blue" (show $ hd x) `uncurry` findWithDefault (0,0) (hd x) c) g

colorCode :: Int -> String
colorCode i = "#" ++ concat ((take 6 . drop 2) $ random16 i)

random16 :: Int -> [String]
random16 i = map (hex . round .  (*15)) (xorshift32inf i)

hex :: Int -> String
hex i = if i > 9 
    then (if i == 10 then "A" else if i == 11 then "B" else if i == 12 then "C" else if i == 13 then "D" else if i == 14 then "E" else "F") 
    else show i
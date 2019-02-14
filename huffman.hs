import Data.List
import Data.Bits 
import Data.Word

import qualified Data.ByteString  as B
import qualified Data.ByteString.Char8 as C

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read)

encode :: (Eq a,Show a) => [a] -> (Tree (Int, a),String)
encode input = (tree,outputWithoutSpaces)
    where
        outputWithSpaces = encodeWithTree tree input
        outputWithoutSpaces = filter (/=' ') outputWithSpaces
        byteStr = toBinArray outputWithoutSpaces
        tree = generateTree input

encodeBin :: (Eq a,Show a) => [a] -> (Tree (Int, a),Int,B.ByteString)
encodeBin input = (tree,len,byteStr)
    where
        outputWithSpaces = encodeWithTree tree input
        outputWithoutSpaces = filter (/=' ') outputWithSpaces
        byteStr = toBinArray outputWithoutSpaces
        tree = generateTree input
        len = length outputWithoutSpaces

tree :: Tree (Int,Char)
tree = Node (468,'h') (Node (201,'h') (Node (91,'h') (Node (43,'h') (Node (20,'h') (Node (10,'h') EmptyTree EmptyTree) (Node (10,'p') EmptyTree EmptyTree)) (Node (23,'o') EmptyTree EmptyTree)) (Node (48,'m') (Node (24,'m') (Node (11,'m') (Node (5,'m') EmptyTree EmptyTree) (Node (6,'y') (Node (3,'y') EmptyTree EmptyTree) (Node (3,'.') EmptyTree EmptyTree))) (Node (13,'F') (Node (6,'F') (Node (3,'F') (Node (1,'F') EmptyTree EmptyTree) (Node (2,'C') (Node (1,'C') EmptyTree EmptyTree) (Node (1,'S') EmptyTree EmptyTree))) (Node (3,'k') EmptyTree EmptyTree)) (Node (7,'v') EmptyTree EmptyTree))) (Node (24,'i') EmptyTree EmptyTree))) (Node (110,'e') (Node (52,'e') EmptyTree EmptyTree) (Node (58,'r') (Node (29,'r') EmptyTree EmptyTree) (Node (29,'s') EmptyTree EmptyTree)))) (Node (267,'g') (Node (129,'g') (Node (62,'g') (Node (30,'g') (Node (15,'g') (Node (7,'g') EmptyTree EmptyTree) (Node (8,'w') (Node (4,'w') (Node (2,'w') EmptyTree EmptyTree) (Node (2,'-') EmptyTree EmptyTree)) (Node (4,'j') (Node (2,'j') (Node (1,'j') EmptyTree EmptyTree) (Node (1,'O') EmptyTree EmptyTree)) (Node (2,'H') EmptyTree EmptyTree)))) (Node (15,'l') EmptyTree EmptyTree)) (Node (32,'d') (Node (16,'d') EmptyTree EmptyTree) (Node (16,'u') EmptyTree EmptyTree))) (Node (67,'t') (Node (33,'t') EmptyTree EmptyTree) (Node (34,'n') EmptyTree EmptyTree))) (Node (138,' ') (Node (68,' ') EmptyTree EmptyTree) (Node (70,'a') (Node (34,'a') EmptyTree EmptyTree) (Node (36,'b') (Node (18,'b') (Node (9,'b') (Node (4,'b') EmptyTree EmptyTree) (Node (5,',') EmptyTree EmptyTree)) (Node (9,'f') EmptyTree EmptyTree)) (Node (18,'c') EmptyTree EmptyTree)))))

mdata :: String
mdata = "001110111100000110001111001111010001111111110011110011011010000001001100010010110000001110011111000111011111111000110011100100101001011001100101111100011011011110100110100111111110100011001011010111100111010100110111010011001000111011110011101110101110010110111111110101111111010011011001111001101101000000100110001001000100111101000011101011000000111001111100011101100101101010111010010100101101001000111111011111010100110010101110101100000101100001111110101001010011011001001111101111010001011011001011101111100000111100111000111011111111000110011100100011101110000011010100110111010110111000011000111101011101000111011100000110011101010110111111010100101101010001101110111100110101100011101110100001110000101101110100011101111111100010110011101000110100111110100100101111010101111110100111110111011110110100100110110101000000010110011001000100000010010111110100011100001111101110101000011011100000100110101101110101110010110100001100111010000100111100000011011010110101000110111011110011010001001111010000111010110111010001011100011101001001000101101010001000100001010100101101010000000101101000011010111101001000010010100101001110101110101101000011001101111100010100110010101001110111100010100010001111000001110000010110000111111010100101001101100101111001110111010111101110100111010000110000101101001101110101101110000110001111010111010110000010110000111111010100101001101100101101000010000000010011001011010100000001011010100011111100000111110110111010111001011001110010101001110111101111010110000100100011010100000001011011110001110111110010101110000111110111010100000001011011110100010001101011000110111110101001101110101101110000110001111010111010010100101101111000001001011001100101000010111111000110111011010111111010001110111000001101110100011000111010100000001011001110010001110100011000111011100110101001001100011010011111110011100010110100011101101111101110101110010110101101001100010110100111110100110111001110111000001101110110001000001111111011000010111111110001000010100010011"


splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n str = [(take n str)] ++ (splitInto n (drop n str))

strToWord :: String -> Word8
strToWord [] = 0
strToWord ('0':xs) = 2 * (strToWord xs)
strToWord ('1':xs) = 1 + 2 * (strToWord xs)

wordToStr :: Word8 -> [Char]
wordToStr 0 = []
wordToStr n = (wordToStr $ shiftR n 1) ++ [(head $ show $ n .&. 1)]

toBinArray :: String -> B.ByteString
toBinArray str = B.pack words
    where
        splitted = splitInto 8 str
        words = map (strToWord . fillToEight .reverse) splitted

decodeBin :: (Show a,Read a, Eq a) => Tree (Int,a) ->  Int  -> B.ByteString -> [a]
decodeBin tree len = (decode tree) . (take len) . convertByteStr


convertByteStr :: B.ByteString -> String
convertByteStr inp = concatMap (fillToEight . wordToStr) arr
    where
        arr :: [Word8]
        arr = B.unpack (B.append inp $ B.singleton 0)

fillToEight :: [Char] -> [Char]
fillToEight str = if length str >= 8 then str else fillToEight $ ('0':str)


intToBin :: Int -> B.ByteString
intToBin n = B.drop 1 $ helper n 8
    where
        helper :: Int -> Int -> B.ByteString
        helper _ 0 = B.singleton 0
        helper n c =  B.append recursion x
            where
                recursion = helper (shiftR n 8) (c - 1)
                x         = B.singleton $ fromIntegral $ (n .&. 255) 

binToInt :: [Word8] -> Int
binToInt [] = 0
binToInt (x:xs) = (shiftL (fromIntegral x) (size * 8) ) + (binToInt xs)
    where
        size = length $ xs
 
treeToBin :: Tree (Int,Char) -> B.ByteString
treeToBin t = B.append len strByte
    where
        str = show t
        strByte = C.pack str
        len = intToBin $ length str

binToTree :: B.ByteString -> (Tree (Int,Char),B.ByteString)
binToTree bstr = (read $ C.unpack $ raw,rest)
    where
        length = binToInt $ B.unpack $ B.take 8 bstr
        raw = C.take length $ C.drop 8 $ bstr
        rest = B.drop (8 + length) bstr


-- encode
toBinFile :: FilePath -> [Char] -> IO ()
toBinFile file str =  do
    let (t,l,b) = encodeBin str
    let contents = B.append (treeToBin t) (B.append (intToBin l) b)
    B.writeFile file contents
    
--decode
fromBinFile :: FilePath -> IO [Char]
fromBinFile file = do
    bin <- B.readFile file
    let (tree,restbin) = binToTree bin
    let len = binToInt $ B.unpack $ B.take 8 $ restbin
    let code = B.drop 8 restbin
    return $ decodeBin tree len code







-- mapM_ print $ prettyPrint tree
prettyPrint :: Tree (Int,Char) -> [[Char]]
prettyPrint EmptyTree = ["_"]
prettyPrint (Node (i,c) EmptyTree EmptyTree) = [[c]]
prettyPrint (Node (i,c) left right) = firstlevel : allnextlevels
    where
        leftprint = prettyPrint left
        rightprint = prettyPrint right
        
        leftfinal = if (length leftprint) < (length rightprint) then 
            leftprint ++ (take ((length rightprint) - (length leftprint)) $ repeat $ take leftlength dashes)
            else leftprint

        rightfinal = if (length rightprint) < (length leftprint) then 
            rightprint ++ (take ((length leftprint) - (length rightprint)) $ repeat $ take rightlength dashes)
            else rightprint
        
        allnextlevels = zipWith ((++) . (++"_")) leftfinal rightfinal
        leftlength = length $ head leftprint
        rightlength = length $ head rightprint
        dashes = repeat '_'
        firstlevel = (take leftlength dashes) ++ ['%'] ++ (take rightlength dashes)



decode :: (Show a,Read a, Eq a) => Tree (Int,a) -> String-> [a]
decode _ [] = []
decode tree inp = case result of    (Just ch) -> ch:(decode tree rest)
                                    Nothing   -> []
    where
        (result,rest) = decodeOne tree inp  

decodeOne :: (Show a,Read a, Eq a) => Tree (Int,a) -> String-> (Maybe a,String)
decodeOne EmptyTree _ = (Nothing,[])
decodeOne (Node (_,char) EmptyTree EmptyTree) str = (Just char,str)
decodeOne _ [] = (Nothing,[])
decodeOne (Node _ left right) ('0':xs) = decodeOne left xs
decodeOne (Node _ left right) ('1':xs) = decodeOne right xs



generateTree :: (Eq a,Show a) => [a] -> Tree (Int,a)
generateTree input = partialTree freqTrees
    where
        freq = analyzefrequency input
        freqTrees = map getUnitTree freq


encodeWithTree :: (Eq a,Show a) => Tree (Int,a) -> [a] -> String
encodeWithTree tree [] = []
encodeWithTree tree (x:xs) = (encodeOneWithTree tree x) ++ (encodeWithTree tree xs)


partialTree :: (Eq a,Show a) => [Tree (Int,a)] -> Tree (Int,a)
partialTree [] = EmptyTree
partialTree (t:[]) = t
partialTree currentTrees = partialTree newcurrentTrees
    where
        (t1:t2:ts) = sortBy (\tc1 tc2 -> compare (scoreTree tc1) (scoreTree tc2)) currentTrees
        sumOfTrees = (scoreTree t1) + (scoreTree t2)
        newcurrentTrees = (Node (sumOfTrees,topT t1) t1 t2 ) : ts


encodeOneWithTree :: (Eq a,Show a) => Tree (Int,a) -> a -> String
encodeOneWithTree EmptyTree _ = ""
encodeOneWithTree t@(Node (_,ch) EmptyTree EmptyTree) input = if ch == input then " " else ""
encodeOneWithTree t@(Node (_,ch) left right) input = recursion
    where
        leftrec = encodeOneWithTree left input
        rightrec = encodeOneWithTree right input
        recursion = 
            if leftrec /= "" then 
                '0':leftrec 
            else if rightrec /= "" then 
                '1':rightrec
            else
                ""

analyzefrequency :: (Eq a,Show a) => [a] -> [(Int,a)]
analyzefrequency input = map (\ch->(countRepetitions ch input,ch)) $ getUnique input

scoreTree :: Tree (Int,a) -> Int
scoreTree EmptyTree = 0
scoreTree (Node (i,_) _ _) = i

topT :: Tree (Int,a) -> a
topT EmptyTree = undefined
topT (Node (_,c) _ _) = c

topF :: (Int,a) -> a
topF (_,c) = c

getUnitTree :: (Int,a) -> Tree (Int,a)
getUnitTree y = Node y EmptyTree EmptyTree

getUnique :: (Eq a,Show a) => [a] -> [a]
getUnique = foldr (\x acc -> if x `elem` acc then acc else x:acc) []


countRepetitions :: (Eq a,Show a) => a -> [a] -> Int
countRepetitions ch = foldr (\x acc -> if (x==ch) then succ acc else acc) 0
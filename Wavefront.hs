--
-- Wavefront.hs
-- Parser and loader for WaveFront obj models
--
-- Jonatan H Sundqvist
-- February 8 2015
--

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--        - Separate MTL and OBJ parsers (?)
--        - Separate parsing, IO and testing
--        - Additional attributes (lighting, splines, etc.)
--        - FFI
--        - Debugging information (line number, missing file, missing values, etc.)
--        - Proper Haddock coverate, including headers
--        - Model type
--        - Caching (?)
--        - Performance, profiling, optimisations
--        - PrintfArg instances for the types defined in this module


-- SPEC | -
--        -



module Wavefront (OBJToken, OBJ, parseOBJ) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf, groupBy)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Maybe (catMaybes)

import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Polymorphic numerical types (?)
-- TODO: Add context, metadata (eg. line numbers, filename) (?)
data OBJToken = Vertex  Float Float Float |
                Normal  Float Float Float |
                Texture Float Float       |
                Face [(Int, Int, Int)]    | -- TODO: Associate material with each face, handle absent indices

                UseMTL String | --  
                LibMTL String | -- TODO: Use actual MTL type

                Group  [String] | -- TODO: Do grouped faces have to be consecutive?
                Object [String]   -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Use error type instead of String (?)
-- This would allow us to distinguish invalid data from eg. comments and blank lines
type OBJRow = Either String OBJToken


-- |
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
type OBJ = [(Int, OBJRow)]


-- | Abstract representation of an OBJ model with associated MTL definitions.
-- 
--
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processeed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
--
data Model = Model { vertices  :: [OBJToken],
                     normals   :: [OBJToken],
                     textures  :: [OBJToken],
                     faces     :: [OBJToken],
                     selects   :: [OBJToken], -- TODO: Rename (UseMTL) (?) 
                     materials :: [OBJToken],
                     groups    :: [OBJToken],
                     objects   :: [OBJToken] } deriving (Show)


-- | 
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- 
data MTLToken = Ambient  Float Float Float | -- Ka
                Diffuse  Float Float Float | -- Kd
                Specular Float Float Float | -- Ks

				MapDiffuse String | -- map_Kd
		        Material   String   -- newmtl
		        deriving (Eq, Show)
		-- ('Ns', 'Ni', 'd', 'Tr', 'illum')


-- |
type MTL = [Maybe MTLToken]



---------------------------------------------------------------------------------------------------
-- Functions (pure)
---------------------------------------------------------------------------------------------------
-- Parsers ----------------------------------------------------------------------------------------
-- | This function generates a list of OBJTokens (wrapped in Maybe to account for invalid data).
-- 
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Is it even necessary to strip whitespace?
-- TODO: Function for composing predicates (?)
-- TODO: Should this function separate the various fields (eg. [(Vertices, Faces, Materials, Groups)] instead of [Maybe OBJToken])
--
parseOBJ :: String -> OBJ
parseOBJ = zip [1..] . map parseOBJRow . lines -- . rows

-- I never new pattern matching in list comprehensions could be used to filter by constructor
-- let rows = parseOBJ data in ([ v | @v(Vertex {}) <- rows], [ v | @v(Vertex {}) <- rows])


-- |
--
-- TODO: Correctness (complete function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, currently unsupported attributes (ignore?) (pattern match against the entire line, eg. ["vn", x, y, z])
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indices
--
parseOBJRow :: String -> OBJRow -- Maybe OBJToken
parseOBJRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = let (which:values) = words ln in case which of
    "v"  -> vector Vertex values -- Vertex
    "vn" -> vector Normal values -- Normal
    -- TODO: Clean this up
    -- TODO: More generic way of unpacking the right number of values and applying read (?)
    "vt" -> let (x:y:[]) = values in Right $ Texture (read x) (read y) -- Texture
    -- TODO: Clean this up
    -- TODO: Handle invalid data
    -- TODO: Capture invalid vertex definitions (currently ignored by catMaybes)
    -- ("Invalid vertex: "++) .
    "f"  -> either (Left . const ln) (Right . Face) . sequence . map (vector triplet . splitOn '/') $ values -- Face
    "g"  -> Right . Group  $ values -- Group
    "o"  -> Right . Object $ values -- Object
    "s"  -> Left ln -- Smooth shading
    "mtllib" -> Right . LibMTL $ head values --
    "usemtl" -> Right . UseMTL $ head values --
    _ -> Left ln
    where triplet a b c = (a, b, c) -- TODO: Use tuple sections (?)


-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = map parseMTLRow . rows


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> Maybe MTLToken
parseMTLRow ln = let (which:values) = words ln in case which of
  "Ka" -> withChannels Ambient  values -- Ka
  "Kd" -> withChannels Diffuse  values -- Kd
  "Ks" -> withChannels Specular values -- Ks
  "map_Kd" -> let (name:[]) = values in Just $ MapDiffuse name -- map_Kd
  "newmtl" -> let (name:[]) = values in Just $ Material   name -- newmtl
  _        -> Nothing
  where withChannels token (r:g:b:[]) = Just $ token (read r) (read g) (read b) -- TODO: No alpha channel (optional?) (?) (read a) 
        withChannels _      _         = Nothing -- TODO: No alpha channel (optional?) (?) (read a)


-- Parsing utilities ------------------------------------------------------------------------------
-- |
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = filter (/=[c]) . groupBy ((==) `on` (==c)) $ s


-- |
-- TODO: Drop comments at the end of a line (?)
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- |
rows :: String -> [String]
rows = filter (\ ln -> not $ any ($ ln) [null, isComment]) . lines


-- |
-- TODO: Use readMaybe (?)
vector :: Read r => (r -> r -> r -> b) -> [String] -> Either String b
vector token (x:y:z:[]) = Right $ token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
vector _      _         = Left  "Pattern match failed"



---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Use bytestrings (?)
loadOBJ :: String -> IO OBJ
loadOBJ fn = do
	rawOBJ <- readFile fn    --
	return $ parseOBJ rawOBJ --


-- |
-- TODO: Use bytestrings (?)
loadMTL :: String -> IO MTL
loadMTL fn = do
	rawMTL <- readFile fn    --
	return $ parseMTL rawMTL --


-- |
-- loadModel


-- | 
promptContinue :: String -> IO ()
promptContinue prompt = do
  putStr prompt
  hFlush stdout
  _ <- getChar
  return ()



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "This is where the checks should be."

  let path = "C:/Users/Jonatan/Desktop/Python/experiments/WaveFront/"
  
  flip mapM_ ["queen", "cube"] $ \ fn -> do
    printf "\nParsing OBJ file: %s.obj\n" fn
    model <- loadOBJ $ printf (path ++ "data/%s.obj") fn
    printf "Found %d invalid rows in OBJ file.\n" $ length [ 1 | Left _ <- map snd model ] --filter (==Nothing) $ model

    promptContinue "Press any key to continue..."

    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token) <- model ]
    -- mapM_ print ["[" ++ show n ++ "] " ++ line | (n, Left line) <- model ]
    -- TODO: Print culprit lines (✓)

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf (path ++ "data/%s.mtl") fn
    printf "Found %d invalid rows in MTL file.\n" . length . filter (==Nothing) $ materials
    mapM_ print . catMaybes $ materials

    promptContinue "Press any key to continue..."
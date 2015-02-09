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
--        - Additional attributes (lighting, splines, etc.)
--        - FFI
--        - Debugging information (line number, missing file, missing values, etc.)

-- SPEC | -
--        -



module Wavefront (OBJToken, OBJ, parseOBJ) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf, groupBy)
import Data.Char (isSpace)
import Data.Function (on)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Polymorphic numerical types (?)
data OBJToken = Vertex  Float Float Float |
                Normal  Float Float Float |
                Texture Float Float Float |
                Face [(Int, Int, Int)]    | -- TODO: Associate material with each face

                UseMTL String | --  
                LibMTL String | -- TODO: Use actual MTL type

                Group  String | -- TODO: Do grouped faces have to be consecutive?
                Object String   -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Rename (?)
type OBJ = [Maybe OBJToken]

-- | 
data MTLToken = Ambient  Float Float Float Float | -- Ka
                Diffuse  Float Float Float Float | -- Kd
                Specular Float Float Float Float | -- Ks

				MapDiffuse String | -- map_Kd
		        Material   String   -- newmtl
		-- ('Ns', 'Ni', 'd', 'Tr', 'illum')

-- |
type MTL = [Maybe MTLToken]



---------------------------------------------------------------------------------------------------
-- Functions (pure)
---------------------------------------------------------------------------------------------------
-- Parsers ----------------------------------------------------------------------------------------
-- |
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Is it even necessary to strip whitespace?
-- TODO: Function for composing predicates (?)
-- TODO: Should this function separate the various fields (eg. [(Vertices, Faces, Materials, Groups)] instead of [Maybe OBJToken])
parseOBJ :: String -> OBJ
parseOBJ = map parseOBJRow . rows

-- I never new pattern matching in list comprehensions could be used to filter by constructor
-- let rows = parseOBJ data in ([ v | @v(Vertex {}) <- rows], [ v | @v(Vertex {}) <- rows])


-- |
-- TODO: Correctness (complete function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, currently unsupported attributes (ignore?)
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indeces
parseOBJRow :: String -> Maybe OBJToken
parseOBJRow ln = let (which:values) = words ln in case which of
  "v"  -> Just $ vector Vertex  values -- Vertex
  "vn" -> Just $ vector Normal  values -- Normal
  "vt" -> Just $ vector Texture values -- Texture
  -- TODO: Clean this up
  -- TODO: Handle invalid data
  "f"  -> Just . Face   $ map (vector triplet . splitOn '/') values -- Face
  "g"  -> Just . Group  $ head values -- Group
  "o"  -> Just . Object $ head values -- Object
  "s"  -> Nothing -- Smooth shading
  "mtllib" -> Just . LibMTL $ head values
  "usemtl" -> Just . UseMTL $ head values
  _ -> Nothing
  where triplet a b c = (a, b, c) -- TODO: Use tuple sections


-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = map parseMTLRow . rows


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> Maybe MTLToken
parseMTLRow ln = let (which:values) = words ln in case which of
  "Ka" -> Just $ withChannels Ambient  values -- Ka
  "Kd" -> Just $ withChannels Diffuse  values -- Kd
  "Ks" -> Just $ withChannels Specular values -- Ks
  "map_Kd" -> let (name:[]) = values in Just $ MapDiffuse name -- map_Kd
  "newmtl" -> let (name:[]) = values in Just $ Material   name -- newmtl
  _        -> Nothing
  where withChannels token rgba = let (r:g:b:a:[]) = rgba in token (read r) (read g) (read b) (read a) 


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
vector :: Read r => (r -> r -> r -> b) -> [String] -> b
vector token (x:y:z:[]) = token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
-- vector _      _         = Nothing



---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "This is where the checks should be."
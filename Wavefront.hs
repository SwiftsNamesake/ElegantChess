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

-- SPEC | -
--        -



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
-- data MTLToken = 

-- |
-- type MTL = [Maybe MTLToken]



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Is it even necessary to strip whitespace?
-- TODO: Function for composing predicates (?)
parseOBJ :: String -> OBJ
parseOBJ = map parseOBJRow . filter (\ ln -> not $ isPrefixOf "#" (dropWhile isSpace ln) || null ln) . lines


-- |
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
  "f"  -> Just . Face   $ map (vector triplet . filter (/="/") . groupBy ((==) `on` (=='/'))) values -- Face
  "g"  -> Just . Group  $ head values -- Group
  "o"  -> Just . Object $ head values -- Object
  "s"  -> Nothing -- Smooth shading
  "mtllib" -> Just . LibMTL $ head values
  "usemtl" -> Just . UseMTL $ head values
  _ -> Nothing
  where vector token (x:y:z:[]) = token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
        -- vector _      _         = Nothing
        triplet a b c = (a, b, c) -- TODO: Use tuple sections


-- |
-- process the OBJ tokens


-- |
-- process the MTL tokens



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "This is where the checks should be."
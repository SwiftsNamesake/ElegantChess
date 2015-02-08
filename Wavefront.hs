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

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
data OBJToken = Vertex  Float Float Float |
                Normal  Float Float Float |
                Texture Float Float Float |
                Face [(Int, Int, Int)]    |

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

{-
"v", Vertex
"vn", Normal
"vt", Texture
"f", Face
"g", Group
"o", Object
"s", Smooth shading
"mtllib"
"usemtl"
-}
-- elif values[0] in ('l'):



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Function for composing predicates (?)
parseOBJ :: String -> OBJ
parseOBJ = map parseOBJRow . filter (("#" `isPrefixOf`) . lstrip) . lines


-- |
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, 
parseOBJRow :: String -> Maybe OBJToken
parseOBJRow ln = let (which:values) = words ln in case which of
  "v"  -> vector Vertex  values -- Vertex
  "vn" -> vector Normal  values -- Normal
  "vt" -> vector Texture values -- Texture
  "f"  -> Just [Face v ] -- Face
  "g"  -> let (name:_) in Just $ Group  name -- Group
  "o"  -> let (name:_) in Just $ Object name -- Object
  "s"  -> Nothing -- Smooth shading
  "mtllib" -> LibMTL "Hello"
  "usemtl" -> UseMTL "Hello"
  _ -> Nothing 
  where vector token (x:y:z:[]) = Just values in Vertex (read x) (read y) (read z)
        vector _                = Nothing
data Game = Game
  { _ship  :: Ship        -- ^ snake as a sequence of points in N2
  , _rocks   :: [Rock]        -- ^ location of the food
  , _rockGenerator  :: Stream Rock -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _score  :: Int          -- ^ score
  } deriving (Show)

type Coord = (Int, Int)

type Rock = (Coord, Int)

type Ship = [Coord]

data Stream a = a :| Stream a
  deriving (Show)
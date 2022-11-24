# SpaceForce

1. Game object -
    
    ```
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
    ```
    
2. moveShip () - moves the ship by 1 unit every key press (held down) (A, V)
3. moveSpace - moves every asteroid in it’s direction by 1 unit (V)
4. hasCollided () - checks if spaceship has collided with any asteroid (S, V)
5. addRocks () every t timeframes (N)
6. removeRock () everytime a rock reaches the edge (N)
7. initGame ()
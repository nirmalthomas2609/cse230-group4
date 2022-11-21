import Control.Lens.Operators

data Kk = Kk {_p :: Int, _q :: String }
	deriving (Show)

a :: Kk
a = Kk {_p = 3, _q = "Lol"}
-- a & (p .~ 6)


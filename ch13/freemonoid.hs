-- Using this constructor directly is not safe
newtype NonNegative a = NonNegative a  deriving (Show) 

toNonNegative :: (Num a, Ord a) => a -> NonNegative a
toNonNegative x
  | x < 0 = error "Only non-negative values are allowed."
  | otherwise = NonNegative x

fromNonNegative :: NonNegative a -> a
fromNonNegative (NonNegative x) = x

-- since the tail is strict, we will always find bottom 
-- if we ever try to construct an infinite list
data FiniteList a = Cons a !(FiniteList a) | Empty deriving (Show)

-- The types in question
type MyFreeMonoid = FiniteList ()
type IsoType = NonNegative Int

-- The isomorphism we wanted to show
myIso :: MyFreeMonoid -> IsoType
myIso l = toNonNegative $ myLength l

myLength Empty = 0
myLength (Cons a fl) = 1+(myLength fl)

myIsoInv :: IsoType -> MyFreeMonoid
myIsoInv nn
  | 0 == fromNonNegative nn = Empty
  | otherwise = Cons () (myIsoInv nMinusOne)
    where nMinusOne = toNonNegative ((fromNonNegative nn) -1 )

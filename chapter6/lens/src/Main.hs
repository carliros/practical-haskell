{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import Lens.Micro.Platform
import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn "hello world"

class Ord v => Vector v  where
  distance :: v -> v -> Double
  centroid :: [v] -> v
class Vector v => Vectorizable e v where
  toVector :: e -> v

-- LENSES
-- Hand-written
data Client0 i = GovOrg0     i String
               | Company0    i String Person0 String
               | Individual0 i Person0
data Person0   = Person0 String String

firstName0 :: Lens' Person0 String
firstName0 = lens (\(Person0 f _) -> f)
                  (\(Person0 _ l) newF -> Person0 newF l)

lastName0 :: Lens' Person0 String
lastName0 = lens (\(Person0 _ l) -> l)
                 (\(Person0 f _) newL -> Person0 f newL)

identifier0 :: Lens (Client0 i) (Client0 j) i j
identifier0 = lens (\case (GovOrg0 i _)      -> i
                          (Company0 i _ _ _) -> i
                          (Individual0 i _)  -> i)
                   (\client newId -> case client of
                       GovOrg0 _ n      -> GovOrg0 newId n
                       Company0 _ n p r -> Company0 newId n p r
                       Individual0 _ p  -> Individual0 newId p)

fullName0 :: Lens' Person0 String
fullName0 = lens (\(Person0 f l) -> f ++ " " ++ l)
                 (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person0 f l
                                      _     -> error "Incorrect name")


-- Auto-generated
data Client i = GovOrg     { _identifier :: i, _name :: String }
              | Company    { _identifier :: i, _name :: String
                           , _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show
data Person   = Person { _firstName :: String, _lastName :: String }
              deriving Show
makeLenses ''Client
makeLenses ''Person

fullName :: Lens' Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")

-- K-means with lenses

data KMeansState e v = KMeansState { _centroids :: [v], _points :: [e]
                                   , _err :: Double, _threshold :: Double
                                   , _steps :: Int }

makeLenses ''KMeansState

initializeStateL :: (Int -> [e] -> [v])
                 -> Int -> [e] -> Double -> KMeansState e v
initializeStateL i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

kMeansL :: (Vector v, Vectorizable e v)
        => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansL i n pts t = view centroids $ kMeansL' (initializeStateL i n pts t)

kMeansL' :: (Vector v, Vectorizable e v)
         => KMeansState e v -> KMeansState e v
kMeansL' state = 
  let assignments = clusterAssignmentPhaseL state
      state1 = state  & centroids.traversed
                      %~ (\c -> centroid
                                    $ fmap toVector
                                    $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state^.centroids) 
                                                     (state1^.centroids))
      state3 = state2 & steps +~ 1
   in if state3^.err < state3^.threshold then state3 else kMeansL' state3

clusterAssignmentPhaseL :: (Vector v, Vectorizable e v)
                        => KMeansState e v -> M.Map v [e]
clusterAssignmentPhaseL = undefined

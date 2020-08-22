-- Author : Furkan BEKAR
-- Date : May 2020
--İNFO : Bu algoritma ağaçlarda en küçük maliyeti hesaplamamıza yarayan algoritmadır.
--       Genellikle Google Maps gibi uygulamalarda gideceğimiz yere ulaşmak için en kısa yolu
--       tarif etmek gibi amaçlar için kullanılır.
-- ################ Dijkstra Short Path Algorithm ################

module Graphs where
--Burada gerekli olan kütüphanelerimizi import ediyoruz ve gerekli olan değişkenlerimizi tanımlıyoruz.
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Mesafeler a = Map.Map (Kose a) Int
type Gidilen a = Map.Map (Kose a) (Kose a)

newtype Kose a = Keyed a
  deriving (Ord, Eq, Show)

-- Burada ise ağacımızı tanımlıyoruz
class Graph g where
  koseler :: Ord a => g a -> Set.Set (Kose a)

  komsular :: Ord a => g a -> Kose a -> Set.Set (Int, Kose a)

newtype MapGraph a = Weighted { toMap :: Map.Map (Kose a) (Set.Set (Int, Kose a)) }

instance Graph MapGraph where
  koseler (Weighted m) =
    Set.union (Map.keysSet m) $ Set.unions $ map (Set.map snd) $ map snd $ Map.toList m

  komsular (Weighted m) v =
    Map.findWithDefault Set.empty v m

-- Bu kısımda artık asıl algoritmayı kurduğumuz kısım ve en kısa yolu bulmada kullandığımız yardımcı fonksiyonu tanımladığımız kısım.
dijkstra :: (Graph g, Ord a) => g a -> Kose a -> (Mesafeler a, Gidilen a)
dijkstra graph source =
  let
    q = koseler graph
    dist = Map.singleton source 0
  in
    aux q dist Map.empty

  where
    aux q dist prev =
      if Set.null q
      then (dist, prev)
      else
        let
          u' = foldr (minIn dist) Nothing q
        in
          case u' of
            Nothing -> (dist, prev)
            Just u ->
              let
                q' = Set.delete u q
                ns = komsular graph u
                (dist', prev') = foldr (insertBetter u) (dist, prev) ns
              in
                aux q' dist' prev'

minIn _ v Nothing = Just v
minIn dist v1 (Just v2) =
  case (Map.lookup v1 dist, Map.lookup v2 dist) of
    (Just d1, Just d2) -> Just (if d1 < d2 then v1 else v2)
    (Nothing, Nothing) -> Nothing
    (Nothing, _) -> Just v2
    (_, Nothing) -> Just v1

insertBetter u (length, v) (dist, prev) =
  case (Map.lookup u dist, Map.lookup v dist) of
    (Just uD, Just vD) ->
      let
        alt = uD + length
      in
        if alt < vD
        then (Map.insert v alt dist, Map.insert v u prev)
        else (dist, prev)
    (Just uD, _) ->
      let
        alt = uD + length
      in
        (Map.insert v alt dist, Map.insert v u prev) 
    _ ->
      (dist, prev)

-- Burda ise algoritmamız son buluyor ve artık konsol ekranına bulduğumuz en kısa yolu yazdırıyoruz.
graph = Weighted $
  Map.fromList
      [
        (Keyed 'H', Set.fromList [(10, Keyed 'W'), (9, Keyed 'C')]),
        (Keyed 'C', Set.fromList [(9, Keyed 'H'), (9, Keyed 'W')]),
        (Keyed '@', Set.fromList [(10, Keyed 'H'), (2, Keyed 'C')])
      ]

main = print $ snd $ dijkstra graph (Keyed 'H')

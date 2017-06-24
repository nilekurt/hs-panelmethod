{-  This file is part of hs-panelmethod.

    hs-panelmethod is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hs-panelmethod is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hs-panelmethod.  If not, see <http://www.gnu.org/licenses/>. -}

module VectorUtils
 ( Line(..)
 , FV2
 , getNormal
 , getMidpoint
 , discretize
 , furthestLines )
where

import Linear

type FV2 = V2 Float
data Line = Line FV2 FV2

getNormal :: Line -> FV2
getNormal (Line v1 v2) = Linear.normalize $ V2 (-dy) dx
    where
    V2 dx dy = v2 - v1

getMidpoint :: Line -> FV2
getMidpoint (Line v1 v2)  = 0.5 * (v1 + v2)

discretize :: Line -> Float -> ([FV2],Float)
discretize (Line v1 v2) count = (map (\n -> v1 + dds * fromInteger n) [0..round count], nds)
    where
    ds = v2 - v1
    dds = ds ^/ count
    nds = norm dds

furthestLines :: FV2 -> [(Int,Line)] -> [Int]
furthestLines dir ls = hasPoint (furthestPoint . pointDistances dir $ ls) ls
    where
    hasPoint p = foldr (\(i,Line v1 v2) acc-> if v1 == p || v2 == p then i : acc else acc) []
    furthestPoint = fst . foldr (\p@(_,d1) acc@(_,d2)-> if d1 > d2 then p else acc) (V2 0 0, 0)
    pointDistances x = map (\(_,Line v1 v2)->
        let d1 = dot v1 x
            d2 = dot v2 x in
        if d1 > d2 then (v1,d1) else (v2,d2) 
        )

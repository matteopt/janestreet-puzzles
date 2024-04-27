-- Copyright (c) 2010-2016 The Gloss Development Team
--
--  Permission is hereby granted, free of charge, to any person
--  obtaining a copy of this software and associated documentation
--  files (the "Software"), to deal in the Software without
--  restriction, including without limitation the rights to use,
--  copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following
--  condition:
--
--  The above copyright notice and this permission notice shall be
--  included in all copies or substantial portions of the Software.

module FractionalOps where

import Linear (V2 (..), dot)

closestPointOnLineParam
        :: (Fractional a)
        => V2 a        -- ^ `P1`
        -> V2 a        -- ^ `P2`
        -> V2 a        -- ^ `P3`
        -> a

closestPointOnLineParam p1 p2 p3
        = (p3 - p1) `dot` (p2 - p1)
        / (p2 - p1) `dot` (p2 - p1)

intersectLineLine
        :: (Fractional a, Eq a)
        => V2 a        -- ^ `P1`
        -> V2 a        -- ^ `P2`
        -> V2 a        -- ^ `P3`
        -> V2 a        -- ^ `P4`
        -> Maybe (V2 a)

intersectLineLine (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x4 y4)
 = let  dx12    = x1 - x2
        dx34    = x3 - x4

        dy12    = y1 - y2
        dy34    = y3 - y4

        den     = dx12 * dy34  - dy12 * dx34

   in if den == 0
        then Nothing
        else let
                det12   = x1*y2 - y1*x2
                det34   = x3*y4 - y3*x4

                numx    = det12 * dx34 - dx12 * det34
                numy    = det12 * dy34 - dy12 * det34
             in Just $ V2 (numx / den) (numy / den)

intersectSegSeg
        :: (Fractional a, Eq a, Ord a)
        => V2 a        -- ^ `P1`
        -> V2 a        -- ^ `P2`
        -> V2 a        -- ^ `P3`
        -> V2 a        -- ^ `P4`
        -> Maybe (V2 a)

intersectSegSeg p1 p2 p3 p4
        -- TODO: merge closest point checks with intersection, reuse subterms.
        | Just p0       <- intersectLineLine p1 p2 p3 p4
        , t12           <- closestPointOnLineParam p1 p2 p0
        , t23           <- closestPointOnLineParam p3 p4 p0
        , t12 >= 0 && t12 <= 1
        , t23 >= 0 && t23 <= 1
        = Just p0

        | otherwise
        = Nothing

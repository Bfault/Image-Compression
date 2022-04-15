--
-- EPITECH PROJECT, 2021
-- compressor
-- File description:
-- Compute
--

module Compute
(
    findCluster,
    nearestCentroid,
) where

import Lib

getCentroid :: [In] -> [Int] -> [Color]
getCentroid _ [] = []
getCentroid contents (rd:rest) = _color (contents!!(rd `mod` length contents))
    : getCentroid contents rest

dumpCentroids :: [Cluster] -> [Cluster] -> [In] -> ([Cluster], [In])
dumpCentroids [] newClusters contents = (newClusters, contents)
dumpCentroids (cluster@Cluster{_colors=colors, _points=points}:clusters)
    newClusters contents
    = dumpCentroids clusters (Cluster{_colors=colors, _points=[]}:newClusters)
    (points ++ contents)

nearestCentroid :: [Cluster] -> In -> Int -> (Cluster, Float, Int)
    -> (Cluster, Int)
nearestCentroid [] _ _(centroid, _, idx) = (centroid, idx)
nearestCentroid (cluster : clusters) content@In{_color=color} idx
    (centroid, distance, id)
    | newDistance < distance = nearestCentroid clusters content
        (idx + 1) (newCluster, newDistance, idx)
    | otherwise = nearestCentroid clusters content (idx + 1)
        (centroid, distance, id)
    where
        newDistance = getDistance color (_colors cluster)
        newCluster = cluster{_points=content:_points cluster}

addColor :: Color -> Color -> Color
addColor Color{_r=r1, _g=g1, _b=b1} Color{_r=r2, _g=g2, _b=b2}
    = Color{_r=r1 + r2, _g=g1 + g2, _b=b1 + b2}

divideColor :: Color -> Int -> Color
divideColor color len
    = Color{_r=r, _g=g, _b=b}
    where
        r = round $ fromIntegral (_r color) / fromIntegral len
        g = round $ fromIntegral (_g color) / fromIntegral len
        b = round $ fromIntegral (_b color) / fromIntegral len

getMeanColor :: [In] -> Int -> Color -> Color
getMeanColor [] 0 total = total
getMeanColor [] len total = divideColor total len
getMeanColor (el : els) len total = getMeanColor els (len + 1)
    (addColor total (_color el))

getConvergence :: [Cluster] -> Float -> [Cluster] -> ([Cluster], Float)
getConvergence [] max newClusters = (newClusters, max)
getConvergence (cluster : clusters) max newClusters =
    if max < distance
        then
            getConvergence clusters distance (newCluster : newClusters)
        else
            getConvergence clusters max (newCluster : newClusters)
    where
        meanColor = getMeanColor (_points cluster) 0 noneColor
        distance = getDistance (_colors cluster) meanColor
        newCluster = Cluster{_colors=meanColor, _points=_points cluster}

runAtConvergence :: Params -> [Cluster] -> [In] -> [Cluster]
runAtConvergence param@Params{_limit=limit} centroids [] =
    if convergence > limit
        then
            runAtConvergence param dumpedCentroids contents
        else
            centroids
    where
        (newCentroids, convergence) = getConvergence centroids 0 []
        (dumpedCentroids, contents) = dumpCentroids newCentroids [] []
runAtConvergence param centroids (c : rest)
    = runAtConvergence param newCentroids rest
        where
            (newCentroid, id) = nearestCentroid centroids c 0
                (noneCluster, 999999.9, 0)
            newCentroids = replaceNth id newCentroid centroids

getInitCluster :: [In] -> [Int] -> [Cluster]
getInitCluster contents rdList =
    map (\x -> Cluster{_points=[], _colors=x}) centroids
        where
            centroids = getCentroid contents rdList


findCluster :: Params -> [In] -> [Int] -> [Cluster]
findCluster param contents rdList = runAtConvergence param initCluster contents
    where
        initCluster = getInitCluster contents rdList
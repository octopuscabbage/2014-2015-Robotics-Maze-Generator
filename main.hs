{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Maybe
import Data.Functor
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Data.List

{--data BTree a = Empty | Node a (BTree a) (BTree a)
	deriving (Eq, Show) --}

data RTree a = Empty | Node a [RTree a] deriving (Eq, Show)


main = do
	{--
	size <- getLine
	branchiness <- getLine
	--}
	tree <- generateStraightMap [2..49] (Node 1 []) 7
	print tree
	print $ maxLength tree
	print $ flatten tree
	print $ length $ flatten tree

generateStraightMap :: (Integral a, Show a) => [a] -> RTree a -> a -> IO (RTree a)
generateStraightMap remaining current size= getNextNode >>= \node -> if(done) then return current else determineNextStep node
	where	done = containsNode current (size*size) 
		getNextNode = getAdjacentNode remaining (leftMostRose current) size
		determineNextStep (Just next) = generateStraightMap (delete next remaining) (addNodeToBottom  next current) size
		determineNextStep Nothing = restart
			where	restart = generateStraightMap [2..size*size] (Node 1 []) size


	

addBranchAtIndex = undefined

maxLength = length . flatten

containsNode ::  Eq a => RTree a -> a -> Bool
containsNode Empty _ = False
containsNode (Node n xs) target = (n == target) || (elem True $ map (\t -> containsNode t target) xs)


leftMostRose ::  RTree t -> t
leftMostRose (Node n []) = n
leftMostRose (Node n xs) = leftMostRose $ head xs

addNodeToBottom ::  a -> RTree a -> RTree a
addNodeToBottom node (Node n []) = (Node n [(Node node [])])
addNodeToBottom node (Node n xs) = (Node n (addNodeToBottom node (head xs):(tail xs)))


getAdjacentNode ::  Integral a => [a] -> a -> a -> IO (Maybe a)
getAdjacentNode remaining n size= 
			if(length adjacentValues == 0) then (return Nothing )
							else do
								testVal <- pick adjacentValues
								return (Just testVal)
			
			where 	isAdjacent n c = (n-size == c) || (n+size ==c) || ((n-1 == c) && not (c /? size)) || ((n+1 == c) && not (n /? size))
				adjacentValues = filter (isAdjacent n) remaining

(/?) ::  Integral a => a -> a -> Bool
a /? b = a `mod` b == 0

pick:: [a] -> IO a
pick xs = runRVar (choice xs) DevRandom 

flatten (Node n []) = [n]
flatten (Node n xs) = [n] ++ concat(map flatten xs)

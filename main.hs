{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Maybe
import Data.Functor
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Data.List

data RTree a = Node a [RTree a] deriving (Eq, Show)
instance Functor RTree where 
	fmap f (Node n xs) = Node (f n) (map (fmap f) xs)

main = do
	{--
	size <- getLine
	branchiness <- getLine
	--}
	criticalPath <- generateStraightMap [2..49] (Node 1 []) 7
	fullMaze <- attachRemaining ([2..49] \\ flatten criticalPath) criticalPath 7
	
	let strTrees = (fmap show) fullMaze
	putStr $ drawTree strTrees

generateStraightMap :: (Integral a, Show a) => [a] -> RTree a -> a -> IO (RTree a)
generateStraightMap remaining current size= getNextNode >>= \node -> if done then return current else determineNextStep node
	where	done = containsNode current (size*size) 
		getNextNode = getAdjacentNode remaining (leftMostRose current) size
		determineNextStep (Just next) = generateStraightMap (delete next remaining) (addNodeToBottom  next current) size
		determineNextStep Nothing = restart
			where	restart = generateStraightMap [2..size*size] (Node 1 []) size
	
attachRemaining [] curTree _ = return curTree
attachRemaining remaining curTree size= do
	value <- pick remaining
	let adjacentNodes = adjacentNodesOnTree value curTree size
	if not $ null adjacentNodes then do
		adjacentTreeNode <- pick adjacentNodes
		attachRemaining (delete value remaining) (attach curTree value adjacentTreeNode) size
	else
		attachRemaining remaining curTree size

adjacentNodesOnTree ::  Integral a => a -> RTree a -> a -> [a]
adjacentNodesOnTree n tree size = filter (\c -> isAdjacent n c size) $ flatten tree

getAdjacentToRemaining remaining curTree size = do
	value <- pick remaining	
	let adjacentTreeNodes = adjacentNodesOnTree value curTree size
	if null adjacentTreeNodes then getAdjacentToRemaining remaining curTree size else value

attach ::  Eq a => RTree a -> a -> a -> RTree a
attach (Node n []) value node = Node n (if n == node then  [Node value []] else [])
attach (Node n xs) value node = Node n (if n == node then (xs++[Node value []]) else (map (\t -> attach t value node) xs) )
	

maxLength = length . flatten

containsNode ::  Eq a => RTree a -> a -> Bool
containsNode (Node n []) _ = False
containsNode (Node n xs) target = (n == target) || (elem True $ map (\t -> containsNode t target) xs)


leftMostRose ::  RTree t -> t
leftMostRose (Node n []) = n
leftMostRose (Node n xs) = leftMostRose $ head xs

addNodeToBottom ::  a -> RTree a -> RTree a
addNodeToBottom node (Node n []) = Node n [Node node []]
addNodeToBottom node (Node n xs) = Node n (addNodeToBottom node (head xs): tail xs)


getAdjacentNode ::  Integral a => [a] -> a -> a -> IO (Maybe a)
getAdjacentNode remaining n size=  if null adjacentValues then return Nothing 
					else pick adjacentValues >>= return . Just 
				where adjacentValues = filter (\c -> isAdjacent n c size) remaining

isAdjacent n c size = (n-size == c) || (n+size ==c) || ((n-1 == c) && not (c /? size)) || ((n+1 == c) && not (n /? size))
(/?) ::  Integral a => a -> a -> Bool
a /? b = a `mod` b == 0

pick:: [a] -> IO a
pick xs = runRVar (choice xs) DevRandom 

flatten (Node n []) = [n]
flatten (Node n xs) = n : concatMap flatten xs

drawTree = (concatMap (++ " \n ") ). draw

draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)

data Set a = Null | Node a (Set a) (Set a) deriving (Ord,Show,Read)


height :: Set a -> Int
height Null = -1
height (Node val left right) = 1 + max (height left) (height right) 

balanced :: Set a -> Bool
balanced Null = True
balanced (Node val left right)  | not (balanced left) = False
                                | not (balanced right) = False
                                | (abs (height left - height right) > 1) = False
                                | otherwise = True

left :: Set a -> Set a
left Null = Null
left (Node val left right) = left

right :: Set a -> Set a
right Null = Null
right (Node val left right) = right

treeRoot :: Set a -> a
treeRoot Null = error "Null node has no value"
treeRoot (Node val left right) = val

rotate :: Set a -> Set a
rotate Null = Null 
rotate (Node val l r)   | not (balanced l) = Node val (rotate l) r
                        | not (balanced r) = Node val l (rotate r)
                        | (height l) - (height r) > 1 && (height (left l)) > (height (right l))  -- Left Left
                           =  Node (treeRoot l) (left l) (Node val (right l) r)
                        | (height l) - (height r) > 1 && (height (left l)) < (height (right l))  -- Left Right
                           =  Node (treeRoot (right l)) (Node (treeRoot l) (left l) (left (right l))) (Node val (right (right l)) r)
                        | (height r) - (height l) > 1 && (height (right r)) > (height (left r))  -- Right Right
                           =  Node (treeRoot r) (Node val l (left r)) (right r) 
                        | (height r) - (height l) > 1 && (height (right r)) < (height (left r))  -- Right Left
                           = Node (treeRoot (left(r))) (Node val l (left (left r))) (Node (treeRoot r) (right (left r)) (right r))
                        | otherwise = Node val l r



-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList Null = []
toList (Node val l r) = toList (l) ++ [val] ++ toList (r) 

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList [] = Null
fromList xs = foldr insert Null (reverse xs)



-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
   Null == Null = True
   _ == Null = False
   Null == _ = False
   (Node val1 left1 right1) == (Node val2 left2 right2) | cardinality (difference (Node val1 left1 right1) (Node val2 left2 right2)) == 0 
                                                          && cardinality (Node val1 left1 right1) == cardinality (Node val2 left2 right2) = True
                                                        | otherwise = False 
    

-- the empty set
empty :: Set a
empty = Null


-- Set with one element 
singleton :: a -> Set a
singleton value = Node value Null Null

-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert value Null = (Node value Null Null)
insert value (Node val left right) | value > val = rotate(Node val left (insert value right))
                                   | value < val = rotate(Node val (insert value left) right)
                                   | otherwise = (Node val left right)


-- Set union
union :: (Ord a) => Set a -> Set a -> Set a
union Null Null = Null
union Null (Node val l f) = Node val l f
union (Node val l f) Null = Node val l f
union (Node val1 left1 right1) (Node val2 left2 right2) = setfoldr insert (Node val1 left1 right1) (Node val2 left2 right2)


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Null Null = Null
intersection _ Null = Null
intersection Null _ = Null
intersection (Node val1 left1 right1) (Node val2 left2 right2)    | val1 `member` (Node val2 left2 right2) = insert val1 (union (intersection left1 (Node val2 left2 right2)) (intersection right1 (Node val2 left2 right2)))
                                                                  | otherwise = union (intersection left1 (Node val2 left2 right2)) (intersection right1 (Node val2 left2 right2))


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a

difference Null Null = Null
difference (Node val l r) Null = (Node val l r)
difference Null _ = Null
difference (Node val1 left1 right1) (Node val2 left2 right2) | not (val1 `member` (Node val2 left2 right2)) = insert val1 (union (difference left1 (Node val2 left2 right2)) (difference right1 (Node val2 left2 right2)))
                                                             | otherwise = union (difference left1 (Node val2 left2 right2)) (difference right1 (Node val2 left2 right2))


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member value Null = False
member value (Node val l r)    | value == val = True
                               | value > val = member value r
                               | otherwise = member value l


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality Null = 0
cardinality (Node val l r) = 1 + (cardinality l) + (cardinality r)


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Null = Null
setmap f (Node val l r) = (Node (f val) (setmap f l) (setmap f r))


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f Null x = x
setfoldr f (Node val l r) x = setfoldr f l (f val (setfoldr f r x))


-- Builds a BALANCED Tree From a Sorted List without using the 'Ord a' type class
buildFromSortedList :: [a] -> Set a
buildFromSortedList [] = Null
buildFromSortedList (x:xs) = Node ((x:xs) !! ((length (x:xs)) `div` 2)) (buildFromSortedList (take ((length (x:xs)) `div` 2) (x:xs))) (buildFromSortedList (drop (((length (x:xs)) `div` 2) + 1) (x:xs)))

-- powerset of a list 
listPowerSet :: [a] -> [[a]]
listPowerSet xs = []: foldr tog [] xs
    where tog x acc = [x]: map (x:) acc ++ acc


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet Null = Null
powerSet (Node value l r) = buildFromSortedList ([ buildFromSortedList xs | xs <- (listPowerSet (toList (Node value l r)))])


-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian Null _ = Null
cartesian _  Null = Null
cartesian (Node val1 left1 right1) (Node val2 left2 right2) = buildFromSortedList  [ (x, y) | x <- (toList (Node val1 left1 right1)), y <- (toList (Node val2 left2 right2))]


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f Null = (Null, Null)
partition f (Node val l r) = (buildFromSortedList (filter f (toList (Node val l r))), buildFromSortedList (filter (not.f) (toList (Node val l r))))

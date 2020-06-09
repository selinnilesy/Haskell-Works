myButLast :: [a] -> a                         -- getting the second element from the end of an array

myButLast [] = error "Empty list"
myButLast [x] = error "Too few elements"
myButLast (x:xs) = 
		if length xs == 1 then x
		else myButLast xs

main = print  $ myButLast [1,2,3,4]     
------------------------------------------------------------------------------------------------------------------------------

nth 1 (x:_) = x             		   -- getting the given element of an array recursively using placeholder
nth n (_:y) = nth (n-1) y    		  --placeholder here corresponds to the first element or the rest of the list

main = print $ nth 2 [9,8,7,6,5,4,3,2,1]
------------------------------------------------------------------------------------------------------------------------------

elementAt :: ([a],Int) -> a                 -- getting the given element of an array recursively

elementAt ([],a1) = error "no list exists"
elementAt (xs2,0) = error "invalid index"
elementAt ((x:xs),a2)
        | a2==1 = x 
        | a2 > length xs+1 = error "index out of array range"
		| otherwise =  elementAt $ (xs,a2-1)

main = print  $ elementAt ([1,2,3],2)

------------------------------------------------------------------------------------------------------------------------------

myLength :: [a] -> Int                  -- getting the length of an array recursively

myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 +  myLength xs

main = print  $ myLength [1,2,3,4,5]

------------------------------------------------------------------------------------------------------------------------------

myReverse :: [a] -> [a]                 -- reversing an array recursively

myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x] 

main = print  $ myReverse [1,2,3,4,5]

------------------------------------------------------------------------------------------------------------------------------

isPalindrome :: (Eq a) => [a] -> Bool                  -- detecting palindrome array 
isPalindrome xs = xs == (reverse xs)

isPalindrome []  = True
isPalindrome [x] = False
isPalindrome list  = (head list) == (last list) && (isPalindrome $ init $ tail list)
main = print $ isPalindrome [1,2,3,2,1]

------------------------------------------------------------------------------------------------------------------------------

data NestedList a = Elem a | List [NestedList a] -- since nested lists are type error in Haskell, we need to define it

flatten :: NestedList a -> [a]                 -- flattens a nested list

flatten (List [])     = []
flatten (Elem temp)   = [temp]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

main = print $ flatten $ List[Elem 1, List [Elem 2, Elem 3], Elem 4]

------------------------------------------------------------------------------------------------------------------------------

removeDuplicates :: Eq a => [a] -> [a]           -- removes duplicate values of a list
removeDuplicates = helper []
    where helper seen [] = seen
          helper seen (x:xs)
              | x `elem` seen = helper seen xs
              | otherwise = helper (seen ++ [x]) xs

main = print $ removeDuplicates [1,2,2,3,5,5]

------------------------------------------------------------------------------------------------------------------------------

repli :: [a] -> Int -> [a]               -- replicates elements of a list given times

repli xs n = concatMap (replicate n) xs
main = print $ repli [1,2,3] 2

------------------------------------------------------------------------------------------------------------------------------

dropEvery :: [a] -> Int -> [a]            -- drops each Nth element of a list recursively

dropEvery xs 0 = []
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)
	  
main = print $ dropEvery [1,2,3,4,5,6,7,8] 3

------------------------------------------------------------------------------------------------------------------------------
removeAt :: Int -> [a] -> (a, [a])           -- removes Nth element of a list and returns it together with the remaining list

removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
	where (l, r) = removeAt (n - 1) xs
	  
main = print $ removeAt 2 [1,2,3]

------------------------------------------------------------------------------------------------------------------------------

data MyList = E | L (Int, MyList) deriving Show  	-- recursive list definition and its use
ex1= L (1, L(2, L(3, E)))
first_el (x:y) = x
first_el2 (L (x,y)) = x

ex2= [1,2,3,4,5,6,7]
last_el (x:[]) = x
last_el (x:xs) = last_el xs

last_el2 (L (x,E)) = x
last_el2 (L (x,xs)) = last_el2 xs
main = do 
    print $ last_el ex2
    print $ last_el2 ex1

------------------------------------------------------------------------------------------------------------------------------

data BT a = Empty | Node (a, BT a, BT a) deriving Show     -- recursive tree implementation and sum of the nodes
bt= Node(1, Node(2,Empty,Empty),Node(3,Node(4,Empty,Empty),Empty))
sum_bt Empty = 0
sum_bt (Node (x,y,z)) = x + sum_bt y + sum_bt z

main = print $ sum_bt bt

------------------------------------------------------------------------------------------------------------------------------
data EGG = Egg CHICKEN | Eg Int      		-- recursive type declarations and function implementations
data CHICKEN = Chicken EGG | Chick Bool
egg = Egg (Chicken (Egg (Chicken (Egg (Chick True)))))
chicken= Chicken (Egg (Chicken (Eg 2)))
f(Eg x) = x
f (Egg y)= 2*(g y)

g(Chick True) =1
g(Chick False) =0
g(Chicken x) =1+ (f x)
main = print $ g chicken

------------------------------------------------------------------------------------------------------------------------------

f x = x+1 					-- returning another function cal
g = f
h=3
w r s = r s
z (f1,f2,c) = if (c>0) then f1 else f2
 
main = print $ z (f,g,10) 5

------------------------------------------------------------------------------------------------------------------------------

fact' 0 n = n    				--  factorial function in tail recursion format
fact' k n  = (fact' (k-1) (n*k))
fact n = fact' n 1

main = print $ fact 4

------------------------------------------------------------------------------------------------------------------------------

append1 [] b = b     				--  concatenates two list together, if b is a single integer then it should be returned as [b]
append1 (x:xs) b = x:(append1 xs b)

main = print $ append1 [1,2,3,4] [2,3]
------------------------------------------------------------------------------------------------------------------------------

makeList 0 =[]					-creates a list with a given integer			
makeList n= n:makeList(n-1)

main = print $ makeList 5

------------------------------------------------------------------------------------------------------------------------------
take1 0 _ = []					-- taking the first n elements of a list
take1 _ [] = []
take1 n (x:y) = x:(take1 (n-1) y)

main = print $ take1 3 [1,2,3,4,5]

------------------------------------------------------------------------------------------------------------------------------

drop1 0 y = y 					-- dropping the first n elements of a list
drop1 _ [] = []
drop1 n (x:y) = (drop1 (n-1) y)

main = print $ drop1 2 [1,2,3,4,5]
------------------------------------------------------------------------------------------------------------------------------

data Tree = E | N (Int, Tree, Tree) deriving Show    -- preorder traversal
tr= N (3, N(5,E,E), N(6,N(4, N(1,E,E),E),N(3,E,E)))

preOrderTraversal E = []
preOrderTraversal (N (x,y,z)) = x: (preOrderTraversal y ++ preOrderTraversal z)

main = print $ preOrderTraversal tr
------------------------------------------------------------------------------------------------------------------------------
data Tree = E | N (Int, Tree, Tree) deriving Show 	--using take1 and drop1 auxiliary functions, returns the preorder traversal of a given list
tr= N (3, N(5,E,E), N(6,N(4, N(1,E,E),E),N(3,E,E)))

list_to_Tree [] = E
list_to_Tree (x:y) = N(x, list_to_Tree (take1 (div (length y) 2) y), list_to_Tree(drop1 (div (length y) 2) y))

main = print $ list_to_Tree [1,2,3,4,5,6,7,8]
------------------------------------------------------------------------------------------------------------------------------
fib' f s 1 = s				-- optimized fibonacci function with complexity O(N)
fib' f s n = fib' s (f+s) (n-1)
fib n = fib' 1 1 n

main = print $ fib 60
------------------------------------------------------------------------------------------------------------------------------
rev [] = []			--reverse function and its optimization from O(N^2) to O(N)
rev (x:y) = rev y ++ [x]

rev2' l = rev2 l [] where	-- function localization
    rev2 [] r = r
    rev2 (x:y) r = rev2 y (x:r)

main = print $ rev2' [1,2,3,4,5]

------------------------------------------------------------------------------------------------------------------------------
ones = 1:ones

mytake 0 _ = []                   -- hand-made factorial function :)
mytake n (a:b) = a:(mytake (n-1) b)


fact = 1: 1: zipWith (*) [2..] (tail fact)
main = print $ take 5 fact
------------------------------------------------------------------------------------------------------------------------------
fibs = 0 : 1 : sumlists fibs (tail fibs)
         where sumlists (x:xs) (y:ys) = (x+y) : sumlists xs ys      -- hand-made fibonacci functions :)
         
fib2 = 0 : 1 : zipWith (+) fib2 (tail fib2)  

fib3 = 0 : 1 : my_map (+) fib3 (tail fib3) 
                 where my_map f xs ys = [f x y | (x,y)<- zip xs ys]

main = print $ take 8 fib3
------------------------------------------------------------------------------------------------------------------------------
primes = sieve [2..]
        where sieve (p:ns) = p:sieve [n|n<- ns, n `mod` p /=0]

main = print $ take 18 primes

--
add x y = x+y   --reduce implementation for nested list
mult x y = x*y 
sub x y = x-y

reduce2 :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> [[a]] ->a
reduce2 f1 f2 n1 n2 [] = n1
reduce2 f1 f2 n1 n2 (x:xs) = f1 (reduce f2 n2 x) (reduce2 f1 f2 n1 n2 xs)

reduce func temp [] = temp
reduce func temp (x:xs) = func x (reduce func temp (xs))

sumprod= reduce2 add mult 0 1
sumsub = reduce2 add sub 0 0
subsum = reduce2 sub add 0 0
subsub = reduce2 sub sub 0 0

--
data NoneorOne = None | One MaleGuest deriving Show		 --abstract data types
data FemaleGuest = FG (String,NoneorOne,[FemaleGuest]) deriving Show
data MaleGuest = MG (String,[FemaleGuest]) deriving Show
data Guest =  Male MaleGuest  |  Female FemaleGuest deriving Show

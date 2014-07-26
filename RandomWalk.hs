import System.Random
import Control.Concurrent



rPick :: [a] -> IO a
rPick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

--adds an object to a list
ls o l = [o] ++ l

--creates a list that represents a row of the world with x width and y height
--if player is in the same y coord then create a list with length (px - 1)
--add it with the player([pc]) and another list with length (x - px)
--else create a list of object a with length x
createList x y c px py pc = if(y == py) 
	then (take (px - 1) (repeat c)) ++ [pc] ++ (take (x - px) (repeat c))
	else take x (repeat c)

--creates a 2d arr which is used to represent the 2d world
--it recrusively itrates over the y coord till it reachs zero
game gx gy c px py pc arr = if(gy > 0)
	then game gx (gy - 1) c px py pc (ls (createList gx gy c px py pc) arr)
	else arr

--this is the main loop
start gx gy px py = do
	threadDelay 800000	--optional
	print "Moving..."
	mapM_ print ( game gx gy 0 px py 1 []) --prints the world
	m <- rPick [1,2,3,4]	--picks a random number	
	--each possible number picked changes the player position
	if(m == 1 && (py - 1) > 0) then
		start gx gy px (py - 1)
	else if(m == 2 && (py + 1) <= gy) then
		start gx gy px (py + 1)
	else if(m == 3 && (px + 1) <= gx) then
		start gx gy (px + 1) py
	else if(m == 4 && (px - 1) > 0) then
		start gx gy (px - 1) py
	else start gx gy px py

main :: IO ()
main = do
	putStrLn "World Width: "
	gx <-  getLine
	putStrLn "World Height: "
	gy <-  getLine
	putStrLn "Player pos x: "
	px <-  getLine
	putStrLn "Player pos y: "
	py <-  getLine
	start (read gx) (read gy) (read px) (read py)
	

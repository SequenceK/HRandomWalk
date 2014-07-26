import System.Random
--import System.Console.ANSI
import Control.Concurrent

main :: IO ()

rPick :: [a] -> IO a
rPick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

ls o l = [o] ++ l
createList x y c px py pc = if(y == py) 
	then (take (px - 1) (repeat c)) ++ [pc] ++ (take (x - px) (repeat c))
	else take x (repeat c)
game gx gy c px py pc arr = if(gy > 0)
	then game gx (gy - 1) c px py pc (ls (createList gx gy c px py pc) arr)
	else arr



start gx gy px py = do
	threadDelay 800000
	--clearScreen
	print "Moving..."
	mapM_ print ( game gx gy 0 px py 1 [])
	m <- rPick [1,2,3,4]
	if(m == 1 && (py - 1) > 0) then
		start gx gy px (py - 1)
	else if(m == 2 && (py + 1) <= gy) then
		start gx gy px (py + 1)
	else if(m == 3 && (px + 1) <= gx) then
		start gx gy (px + 1) py
	else if(m == 4 && (px - 1) > 0) then
		start gx gy (px - 1) py
	else start gx gy px py

main = do
	putStrLn "Game Width: "
	gx <-  getLine
	putStrLn "Game Height: "
	gy <-  getLine
	putStrLn "Player pos x: "
	px <-  getLine
	putStrLn "Player pos y: "
	py <-  getLine
	setTitle "Moving..."
	start (read gx) (read gy) (read px) (read py)
	

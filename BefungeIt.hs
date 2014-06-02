-- A simple Befunge interpreter
-- Challenge from: http://www.reddit.com/r/dailyprogrammer/comments/270mll/612014_challenge_164_hard_what_the_funge_is_this/

-- I'm going to go crazy and try the state monad

import System.Environment
import System.Random
import System.IO
import Data.Char
import Control.Monad.Trans.State

------------------ Some types we'll use ------------------

data Direction	= DUp | DDown | DLeft | DRight deriving (Show, Enum, Bounded)	-- Ds to prevent collision with Prelude

type Coords = (Int, Int)

type Instruction = Char

type Program = [[Instruction]]

data Interpreter = Interpreter {
					stack		:: [Int],
					position	:: Coords,
					program		:: Program,
					direction	:: Direction,
					stringMode	:: Bool					
				}

type InterpreterFunction = State Interpreter

------------------ Instances so we can get a random direction ------------------

instance Random Direction where
  randomR (from, to) gen = (toEnum num, gen')  						-- Turn a random number into a Direction
  	where (num, gen') = randomR (fromEnum from, fromEnum to) gen 	-- Get a random number in the range
  random g = randomR (minBound, maxBound) g

------------------ Functions to work on our types ------------------

-- Turn our program into something we can display
showProgram :: Program -> String
showProgram p = unlines p		

-- Given all the data in a file, we need to turn that into the program and initial stack
parseProgram :: String -> (Program, [Int])
parseProgram s = (extendedLines ++ extraLines, startingStack)
	where
		(firstLine:otherLines)	= lines s													-- Extract the first line		
		extendLine l			= l ++ (replicate (80 - length l) ' ')						-- Function to pad lines to 80 characters
		extendedLines			= map extendLine otherLines
		extraLines				= replicate (25 - length extendedLines) (replicate 80 ' ')	-- Extra lines to make the program 25 rows
		startingStack			= map read $ words firstLine

-- Find the next position given current coords and current direction
nextPosition :: Coords -> Direction -> Coords
nextPosition (x, y) d = (bigX `mod` 80, bigY `mod` 25)
	where
		(xDelta, yDelta) = case d of
							DLeft	-> (-1, 0)
							DRight	-> (1, 0)
							DUp		-> (0, -1)
							DDown	-> (0, 1)
		(bigX, bigY) = (x + 80 + xDelta, y + 25 + yDelta)							-- Add numbers so we can % without negatives
								
-- Get the instruction at the given spot
findInstruction :: Program -> Coords -> Instruction
findInstruction m (x, y) = m !! y !! x

-- Change an instruction
setInstruction :: Program -> Coords -> Instruction -> Program
setInstruction p (x, y) i = rowsBefore ++ (lineBefore ++ i:lineAfter):rowsAfter
	where
		(rowsBefore, rowToChange:rowsAfter)	= splitAt y p
		(lineBefore, _:lineAfter)			= splitAt x rowToChange
		
-- Get a character from the user
askUserForChar :: IO Char
askUserForChar = getChar

-- Display a character to the user
putCharacter :: Int -> IO ()
putCharacter i = do
					putChar $ chr i
					hFlush stdout

-- Function to get some stuff from our stack
popStack :: Interpreter -> Int -> IO ([Int], Interpreter)
popStack i@(Interpreter st _ _ _ _) num
	| num > length st	= ioError $ userError "Stack too small"
	| otherwise			= return (taken, i{stack = rest})
		where
			(taken, rest) = splitAt num st

-- Function to update the stack
pushStack :: Interpreter -> [Int] -> Interpreter
pushStack i@(Interpreter st _ _ _ _) stuff = i{stack = (reverse stuff) ++ st}

-- Get a program and initial stack from a file
loadProgram :: String -> IO (Program, [Int])
loadProgram f = do
			fileText <- readFile f
			return $ parseProgram fileText

------------------ Our main function, to do the work ------------------

main = do
	args <- getArgs
	
	putStrLn $ "Loading program from " ++ args !! 0
	
	(p, intialStack) <- loadProgram $ args !! 0
	
	putStrLn "Here is the program:"
	
	putStrLn $ showProgram p

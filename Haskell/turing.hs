data Move = MLeft | MRight | Stay deriving (Show, Eq)
data Tape a = Tape a [a] [a]
data Action state val = Action val Move state deriving (Show)
 
instance (Show a) => Show (Tape a) where
  show (Tape x lts rts) = concat $ left ++ [hd] ++ right where
    hd = "[" ++ show x ++ "]"
    left = map show $ reverse $ take 10 lts
    right = map show $ take 10 rts
 
tape blank lts rts | null rts = Tape blank left blanks
                   | otherwise = Tape (head rts) left right where
                    blanks = repeat blank
                    left = reverse lts ++ blanks
                    right = tail rts ++ blanks   
 
step rules (state, Tape x (lh:lts) (rh:rts)) = (state', tape') where 
  Action x' dir state' = rules state x
  tape' = move dir
  move Stay = Tape x' (lh:lts) (rh:rts)
  move MLeft = Tape lh lts (x':rh:rts)
  move MRight = Tape rh (x':lh:lts) rts
 
runUTM rules stop start tape = steps ++ [final] where
  (steps, final:_) = break ((== stop) . fst) $ iterate (step rules) (start, tape)

-- Incrementer
incr "q0" 1 = Action 1 MRight "q0"
incr "q0" 0 = Action 1 Stay "qf"
 
tape1 = tape 0 [] [1,1, 1]
machine1 = runUTM incr "qf" "q0" tape1

-- Beaver
beaver "a" 0 = Action 1 MRight "b"
beaver "a" 1 = Action 1 MLeft  "c"
beaver "b" 0 = Action 1 MLeft  "a"
beaver "b" 1 = Action 1 MRight "b"
beaver "c" 0 = Action 1 MLeft  "b"
beaver "c" 1 = Action 1 Stay   "halt"
 
tape2 = tape 0 [] []
machine2 = runUTM beaver "halt" "a" tape2

main :: IO ()
main = mapM_ print (machine1 ++ machine2)


module Gems where 

import Graphics.UI.WX 

import Data.List

width = 480
height = 700 

runGUI :: IO ()
runGUI = start gui 

---------------------------------------------------------------------------- 
-- Game datatypes 
data GemColor = Blue | Green | Orange
              deriving (Eq, Show)
                       
data GemStatus = Alive        -- a gems normal state
               | Dying Int    -- count down to deletion (Animation counter) 
  deriving (Eq, Show)  
                       
data Gem      = Gem { gemColor  :: GemColor,                        
                      gemStatus :: GemStatus }
              deriving (Show) 
                       
instance Eq Gem where 
  (==) (Gem c1 _) (Gem c2 _) = c1 == c2

                       
type GemStack = [Gem] 
data GameArea = GameArea [GemStack] 
                deriving (Eq, Show)
type Score    = Integer

type GemCluster = [Gem] 

type Pos = (Int,Int) 

----------------------------------------------------------------------------
-- Some game data
gemBlue  = image "./gemblue.png"
gemGreen = image "./gemgreen.png"
gemOrange = image "./gemorange.png"

--gems
blueGem = Gem Blue Alive
greenGem = Gem Green Alive
orangeGem = Gem Orange Alive 

gemWidth = 95
gemHeight = 40

testCluster :: GemCluster 
testCluster = [greenGem,blueGem,orangeGem]    
testClusters = [[blueGem,blueGem,orangeGem],    
                [blueGem,blueGem,blueGem],
                [orangeGem,blueGem,greenGem],
                [orangeGem,orangeGem,blueGem],
                [orangeGem,orangeGem,greenGem]]




emptyGameArea = GameArea [[],[],[],[],[]] 

----------------------------------------------------------------------------
-- What it means to loose. 
lost :: GameArea -> Bool 
lost (GameArea gs) = any ((>= 14) . length) gs


---------------------------------------------------------------------------- 
-- By standard blockgame laws of physics, blocks disappear when in large 
-- enough groups. 

-- mark for deletion when 
{-
   
   G-G-G     -- on both sides of this position is the same thing (all three are marked) 

     G
     | 
     G       -- above and below are same, (all three are marked) 
     | 
     G 
  
-}

markForDeletion :: [GemStack] -> [GemStack]
markForDeletion gs = markForDeletionVert gs'
  where 
    gs' = 
      [[Gem (gemColor ((gs !! x) !! y)) (if (tripplev (x,y) gs) then (Dying 3) else Alive)
       | y <- [0..(length (gs !! x))-1]]| x <- [0..4]]

deleteMarked :: [GemStack] -> [GemStack] 
deleteMarked marked = map (filter living) marked
  where
    living (Gem _ Alive) = True                       
    living _ = False


--------------------------------------------------------------------------
-- Remove large enough groups of Gems
strip :: GameArea -> GameArea 
strip (GameArea gs) = GameArea$ (deleteMarked . markForDeletion) gs
  
strip2 :: GameArea -> GameArea 
strip2 gs | gs' == gs = gs  
          | otherwise = strip2 gs'
  where  gs' = strip gs 
 
--------------------------------------------------------------------------
-- find horizontal triples
tripplev p@(x,_) lls | x == 0 = isLeftMost p lls
                     | x == 4 = isRightMost p lls 
                     | x == 2 = isLeftMost p lls || isRightMost p lls || isCenterPiece p lls 
                     | x == 1 = isLeftMost p lls || isCenterPiece p lls 
                     | x == 3 = isRightMost p lls || isCenterPiece p lls 

                       
check [of0,of1,of2] (x,y) lls 
   = let col1 = lls !! (x + of0)
         col2 = lls !! (x + of1)
         col3 = lls !! (x + of2) 
     in all ((>y) . length) [col1,col2,col3] &&
        (col1 !! y == col2 !! y && 
         col2 !! y == col3 !! y) 

isCenterPiece p lls = check [0,-1,1] p lls 
isLeftMost    p lls = check [0,1,2] p lls 
isRightMost   p lls = check [-2,-1,0] p lls 

-- look for vertical groups of 3 or more to mark for deletion.
markForDeletionVert :: [GemStack] -> [GemStack] 
markForDeletionVert gs = map markColumn gs
  where 
    markColumn [] = [] 
    markColumn [x] = [x]
    markColumn [x,y] = [x,y] 
    markColumn (x:y:z:xs) | x == y && y == z = 
      let ny = Gem (gemColor y) (Dying 3)
          nz = Gem (gemColor z) (Dying 3) 
      in Gem (gemColor x) (Dying 3) : markColumn (ny:nz:xs)
                          | otherwise = x : markColumn(y:z:xs)



-- is position Pos in GameArea occupied ?                       
occupied :: GameArea -> Pos -> Bool 
occupied (GameArea gs) (x,y) = 
  x >= 0 && x <= 4 && length (gs !! x) > y
  
canMoveRight, canMoveLeft :: GameArea -> Pos -> Bool   
canMoveLeft  ga = not . (occupied ga) . moveLeft
canMoveRight ga = not . (occupied ga) . moveRight

-- attach to column
attach :: GameArea -> Int -> [Gem] -> GameArea
attach (GameArea ga) i g = GameArea new
  where 
    col = ga !! i 
    new = ga !!= (i,col++g)
    
ls !!= (i,a) = take i ls ++ [a] ++ drop (i+1) ls


offsetOnTop x = x - 40 

{- 
 |  B  |
 |  G  |
 |  O  |
 |     |  = GameArea [[Blue],[Orange,Blue],[Green],[Green],[Green,Green]]
 | B  G|
 |BOGGG|
  -----
-} 
--testGM = GameArea [[Blue],[Orange,Blue],[Green],[Green],[Green,Green]]
--testGM2 = GameArea [[Blue,Orange,Green],[Blue,Orange,Orange],[Blue,Orange,Green],[Green,Green,Green],[Orange,Green,Green]]
--lostGM = GameArea [[Blue],[Orange,Blue],[Green],[Green],[Green,Green,Blue,Blue,Orange]]

{-

[[Blue,Orange,Green],
 [Blue,Orange,Orange],
 [Blue,Orange,Green],
 [Green,Green,Green],
 [Orange,Green,Green]]

==> (after sweep) 

[[Green],
 [Orange],
 [],
 [Green,Green],
 [Orange,Green]]
 


-}


----------------------------------------------------------------------------
-- Clusters and positions 

validPos (x,y) = x >= 0 && x <= 4 && y >= 0 

-- if a cluster "touches" a stationary gem it "sticks" 
clusterStick :: GameArea -> GemCluster -> Pos -> (GameArea,GemCluster) 
clusterStick ga [] p = (ga,[]) 
clusterStick ga cl (x,y) | occupied ga (x,y-1) = (attach ga x cl,[])
                         | otherwise           = (ga,cl)
  

moveLeft   p@(x,y) = if validPos (x-1,y) then (x-1,y) else p
moveRight  p@(x,y) = if validPos (x+1,y) then (x+1,y) else p
moveDown   p@(x,y) = if validPos (x,y-1) then (x,y-1) else p


----------------------------------------------------------------------------
--

drawGem g dc p = 
  case gemColor g of 
    Blue   -> drawImage dc gemBlue p   [] 
    Orange -> drawImage dc gemOrange p []
    Green  -> drawImage dc gemGreen p  []
    

drawStack :: DC a -> (GemStack,(Int,Int)) -> IO () 
drawStack dc ([],_) = return ()
drawStack dc ((g:gs),(px,py)) = 
  do 
    drawGem g dc (point px py)   
    drawStack dc (gs,(px,offsetOnTop py)) 


drawGameArea :: GameArea -> DC a -> Rect -> IO () 
drawGameArea (GameArea stacks) dc rect = 
  do 
    let bottom = rectHeight rect
        startps = zip [0,95..600] (repeat (bottom-170)) -- Hardcoded
        stacksp = zip stacks startps 
    mapM_ (drawStack dc) stacksp


drawGameAreaVar :: Var GameArea -> DC a -> Rect -> IO () 
drawGameAreaVar ga_var dc rect = 
  do 
    ga <- varGet ga_var
    drawGameArea ga dc rect
    
    
drawCluster _ [] dc rect = return () 
drawCluster (x,y) (g:gs) dc rect = 
      do 
        let posFromBot x = (bottom - 170) - x   
            bottom = rectHeight rect
        drawGem g dc (point (x*gemWidth) (posFromBot ((y*gemHeight))))
        drawCluster (x,y+1) gs dc rect
 

drawClusterVar :: Var Pos -> Var GemCluster -> DC a -> Rect -> IO () 
drawClusterVar vp vgc dc rect = 
  do 
    p <- varGet vp
    gc <- varGet vgc 
          
    drawCluster p gc dc rect
  
draw ga cp gc dc rect = do
  drawGameAreaVar ga dc rect
  drawClusterVar  cp gc dc rect 
  



----------------------------------------------------------------------------
-- The GUI 
gui 
  = do 
       gameArea          <- varCreate emptyGameArea
       currentCluster    <- varCreate testCluster 
       currentClusterPos <- varCreate (3,15) 
       clusters          <- varCreate testClusters            
           
       f     <- frame    [text := "Grid"]
       p     <- panel  f [on paint := draw gameArea currentClusterPos currentCluster] 
       t     <- timer  f [on command := 
                           do 
                             gc <- varGet currentCluster 
                             cp <- varGet currentClusterPos 
                             ga <- varGet gameArea 
                             let (ga',clust) = clusterStick ga gc cp
                                 
                             varSet gameArea ga'
                             if (clust == [])  
                               then 
                                 do 
                                   clusts <- varGet clusters 
                                   varSet currentClusterPos (3,15)
                                   varSet currentCluster (head clusts)
                                   varSet clusters (tail clusts ++ [gc])
                                   let stripped_ga = strip2 ga'
                                   varSet gameArea stripped_ga
                               else 
                                 do
                                   -- putStrLn $ show clust
                                   varSet currentClusterPos (moveDown cp)
                             if (lost ga') 
                               then close f -- bit drastic :)
                               else return ()
                             repaint p
                         ] 
                
       quit  <- button f [text := "Quit", on command := close f]
       
       let lay = column 5 [minsize (sz width height) $ widget p, 
                           row 1 [widget quit]] 
       
       set t [ interval := 250 ]
       set p [ on downKey := return () -- putStrLn "down" 
             , on upKey := return () -- putStrLn "up"
             , on leftKey := 
                do 
                   cp <- varGet currentClusterPos 
                   ga <- varGet gameArea
                   if canMoveLeft ga cp 
                     then varSet currentClusterPos (moveLeft cp) 
                     else return () 
                   repaint p
                   
             , on rightKey := 
                 do 
                   cp <- varGet currentClusterPos
                   ga <- varGet gameArea
                   if canMoveRight ga cp
                     then varSet currentClusterPos (moveRight cp)
                     else return ()
                   repaint p
             , on (charKey 'r') := 
                 do
                   cc <- varGet currentCluster
                   varSet currentCluster (tail cc ++ [head cc])
                   repaint p 
             ]
       
       set f [layout := lay]
       return f
    
    


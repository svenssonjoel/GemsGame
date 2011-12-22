


module Gems where 

import Graphics.UI.WX hiding (stop,position)

import Data.List

width = 600
height = 800 

runGUI :: IO ()
runGUI = start gui 

---------------------------------------------------------------------------- 
-- Game datatypes 
data Gem      = Blue | Green | Orange
              deriving (Eq, Show)
type GemStack = [Gem] 
data GameArea = GameArea [GemStack] 
type Score    = Integer

type GemCluster = [[Gem]] 

type Pos = (Int,Int) 

----------------------------------------------------------------------------
-- Some game data
gemBlue  = image "./gemblue.png"
gemGreen = image "./gemgreen.png"
gemOrange = image "./gemorange.png"


testCluster :: GemCluster 
testCluster = [[Green,Blue,Orange]]    

{- testCluster represents  
   O
   | 
   B 
   | 
   G 
-} 

testCluster2 :: GemCluster 
testCluster2 = [[Green],[Blue],[Orange]] 

{- testCluster2 represents
  
   G-B-O
-} 


emptyGameArea = GameArea [[],[],[],[],[]] 

----------------------------------------------------------------------------
-- What it means to loose. 
lost :: GameArea -> Bool 
lost (GameArea gs) = any ((>= 10) . length) gs


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
markForDeletion :: [GemStack] -> [[(Gem,Bool)]] 
markForDeletion gs = 
  [[((gs !! x) !! y,  tripplev (x,y) gs) | y <- [0..(length (gs !! x))-1]]| x <- [0..4]]
    
  
deleteMarked :: [[(Gem,Bool)]] -> [GemStack] 
deleteMarked marked = map (map fst . (filter (not . snd))) marked

strip :: GameArea -> GameArea 
strip (GameArea gs) = GameArea$ (deleteMarked . markForDeletion) gs
  
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

                       
occupied :: GameArea -> Pos -> Bool 
occupied (GameArea gs) (x,y) = 
  x >= 0 && x <= 4 && length (gs !! x) > y
  

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
testGM = GameArea [[Blue],[Orange,Blue],[Green],[Green],[Green,Green]]
testGM2 = GameArea [[Blue,Orange,Green],[Blue,Orange,Orange],[Blue,Orange,Green],[Green,Green,Green],[Orange,Green,Green]]
lostGM = GameArea [[Blue],[Orange,Blue],[Green],[Green],[Green,Green,Blue,Blue,Orange]]

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

-- if a cluster "touches" a stationary gem it "sticks" (or part of)  
clusterStick :: GameArea -> GemCluster -> Pos -> (GameArea,GemCluster) 
clusterStick ga [[]] p = (ga,[]) 
clusterStick ga cl@[(g:gs)] (x,y) | occupied ga (x,y-1) = (attach ga x (g:gs),[[]])
                                  | otherwise           = (ga,cl)
clusterStick ga _ (x,y) = error$ show x ++ " " ++ show (y-1)


moveLeft   p@(x,y) = if validPos (x-1,y) then (x-1,y) else p
moveRight  p@(x,y) = if validPos (x+1,y) then (x+1,y) else p
moveDown   p@(x,y) = if validPos (x,y-1) then (x,y-1) else p


----------------------------------------------------------------------------
--


drawGem Blue   dc p = drawImage dc gemBlue p   [] 
drawGem Orange dc p = drawImage dc gemOrange p []
drawGem Green  dc p = drawImage dc gemGreen p  []


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
    
    
-- needs work
--  TODO: draw clusters of both vertical and horizontal orientation
drawCluster :: Pos -> GemCluster -> DC a -> Rect -> IO () 
drawCluster (x,y) [[g1,g2,g3]] dc rect = 
  do 
    let bottom = rectHeight rect
        posFromBot x = (bottom - 170) - x   
    drawGem g1 dc (point (x*95) (posFromBot ((y*40))))
    drawGem g2 dc (point (x*95) (posFromBot ((y+1)*40)))
    drawGem g3 dc (point (x*95) (posFromBot ((y+2)*40)))
  
     

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
    
       let ga = strip testGM2 
           (ga1,_) = clusterStick ga testCluster (3,2)
           (ga2,_) = clusterStick ga1 testCluster (2,0) 
           (ga3,_) = clusterStick ga2 testCluster (4,2)
           
           
       gameArea          <- varCreate emptyGameArea
       currentCluster    <- varCreate testCluster 
       currentClusterPos <- varCreate (3,15) 
                  
           
       f     <- frame    [text := "Grid"]
       p     <- panel  f [on paint := draw gameArea currentClusterPos currentCluster] 
       t     <- timer  f [on command := 
                           do 
                             gc <- varGet currentCluster 
                             cp <- varGet currentClusterPos 
                             ga <- varGet gameArea 
                             let (ga',clust) = clusterStick ga gc cp
                                 
                             varSet gameArea ga'
                             if (clust == [[]])  
                               then 
                                 do 
                                   varSet currentClusterPos (3,15)
                                   let stripped_ga = strip ga'
                                   varSet gameArea stripped_ga
                               else varSet currentClusterPos (moveDown cp)
                               
                             
                             
                             repaint p
                             putStrLn "hej"]
                
       quit  <- button f [text := "Quit", on command := close f]
       
       let lay = column 5 [minsize (sz width height) $ widget p, 
                           row 1 [widget quit]] 
       
       set t [ interval := 250 ]
       set p [ on downKey := putStrLn "down" 
             , on upKey := putStrLn "up"
             , on leftKey := 
                do 
                   cp <- varGet currentClusterPos 
                   varSet currentClusterPos (moveLeft cp) 
                   repaint p
                   
             , on rightKey := 
                 do 
                   cp <- varGet currentClusterPos
                   varSet currentClusterPos (moveRight cp)
                   repaint p
             ]
       
       set f [layout := lay]
       return f
    
    


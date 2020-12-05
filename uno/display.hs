-- link used: http://unocardinfo.victorhomedia.com/
-- don't forget about this step, otherwise you get an error: https://stackoverflow.com/questions/8956387/cant-get-freeglut-to-work-with-haskell-on-windows
import Graphics.Gloss

-- make this dependent upon how many people are playing the game
-- how do you change the size of the cards?
-- how do you anchor the cards into a certain position?
displayCard :: IO ()
displayCard = do
 picture_proc_1 <- loadBMP "./cards/uno-card-blue8.bmp"
 picture_proc_2 <- loadBMP "./cards/uno-card-red5.bmp"
 picture_proc_3 <- loadBMP "./cards/uno-card-green2.bmp"
 picture_proc_4 <- loadBMP "./cards/uno-card-yellow0.bmp"
 let picture1 = translate (-150) 0 picture_proc_1
 let picture2 = translate (-50) 0 picture_proc_2
 let picture3 = translate 50 0 picture_proc_3
 let picture4 = translate 150 0 picture_proc_4
 let big_picture = pictures [picture1, picture2, picture3, picture4]
 -- string is the title, first tuple is the size, second tuple is where it shows up on the computer screen
 -- we are going to do full-screen: but, is everyone else's screen going to be the same size?
 display (InWindow "Pic" (1000,1000) (100,100)) white big_picture
  
  
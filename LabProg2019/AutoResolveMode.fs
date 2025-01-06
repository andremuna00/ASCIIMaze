module LabProg2019.AutoResolveMode

open System
open Gfx
open Engine
open Maze


let mutable w = 21
let mutable h= 21
let mutable mode = "Playable"

type state = {
    player : sprite
}


let main () = 
   let engine = new engine (w, h)
   Console.Clear()
   Gfx.system_console_raster.at (0,0,Color.Black)

   //generate the maze and call the autoresolve method
   let mymaze = new maze (w,h)
   mymaze.generate
   mymaze.generateUscitaAutoResolve mymaze.walls
   mymaze.autoresolve mymaze.walls

   //loop for both game-modes Solution and Playable
   let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
       let dx, dy =
           match key.KeyChar with 
           //victory case
           | 'w' | 'a' |'s'|'d' when st.player.x = float (w-1)  || st.player.y = float (h-1) -> 
               let victory = engine.create_and_register_sprite(image.rectangle (w, h, pixel.filled_wall Color.Black, pixel.filled_wall Color.Black), 0, 0, 2) 
               0., 0.
           //chek not to go out of bounds and cover the solution if mode = Playable
           | 'w' when st.player.y<>0.&&(not(Array2D.get (mymaze.walls) (int st.player.x) (int st.player.y-1))) -> if(mode="Playable"&&(Array2D.get mymaze.resolved (int st.player.x) (int st.player.y) = 0)) then ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), (int st.player.x), (int st.player.y), 2)
                                                                                                                  0., -1.                                                                                                 
           | 's' when (not(Array2D.get (mymaze.walls) (int st.player.x) (int st.player.y+1))) -> if(mode="Playable"&&(Array2D.get mymaze.resolved (int st.player.x) (int st.player.y) = 0)) then ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), (int st.player.x), (int st.player.y), 2)
                                                                                                 0., 1.
           | 'a' when (not(Array2D.get (mymaze.walls) (int st.player.x-1) (int st.player.y))) -> if(mode="Playable"&&(Array2D.get mymaze.resolved (int st.player.x) (int st.player.y) = 0)) then ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), (int st.player.x), (int st.player.y), 2)
                                                                                                 -1., 0.
           | 'd' when (not(Array2D.get (mymaze.walls) (int st.player.x+1) (int st.player.y))) -> if(mode="Playable"&&(Array2D.get mymaze.resolved (int st.player.x) (int st.player.y) = 0)) then ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), (int st.player.x), (int st.player.y), 2)
                                                                                                 1., 0.
           |_ -> 0., 0.
       st.player.move_by (dx, dy)
       st, key.KeyChar = '\027' || key.KeyChar = 'q'

    //loop for animation mode
   let my_update_animation (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            match key.KeyChar with 
            //chek not to go out of bounds and move the player (not dipending on the pressed key) in the next cell of Risolved Matrix with value 0
            |_ when Array2D.get mymaze.resolved (int st.player.x) (int st.player.y+1) = 0-> Array2D.set mymaze.resolved (int st.player.x) (int st.player.y+1) 4
                                                                                            ignore <| engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Yellow, pixel.filled_player Color.Yellow), (int st.player.x), (int st.player.y+1), 2)
                                                                                            0., 1.
            |_ when Array2D.get mymaze.resolved (int st.player.x) (int st.player.y-1) = 0-> Array2D.set mymaze.resolved (int st.player.x) (int st.player.y-1) 4
                                                                                            ignore <| engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Yellow, pixel.filled_player Color.Yellow), (int st.player.x), (int st.player.y-1), 2)
                                                                                            0., -1.
            |_ when Array2D.get mymaze.resolved (int st.player.x+1) (int st.player.y) = 0-> Array2D.set mymaze.resolved (int st.player.x+1) (int st.player.y) 4
                                                                                            ignore <| engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Yellow, pixel.filled_player Color.Yellow), (int st.player.x+1), (int st.player.y), 2)
                                                                                            1., 0.
            |_ when Array2D.get mymaze.resolved (int st.player.x-1) (int st.player.y) = 0-> Array2D.set mymaze.resolved (int st.player.x-1) (int st.player.y) 4
                                                                                            ignore <| engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Yellow, pixel.filled_player Color.Yellow), (int st.player.x-1), (int st.player.y), 2)
                                                                                            -1., 0.
            |_ -> 0.,0.
        st.player.move_by (dx, dy)
        st, key.KeyChar = '\027' || key.KeyChar = 'q'
     
   //drawing the maze
   for i=0 to (w-1) do
       for j=0 to (h-1) do
           if(Array2D.get (mymaze.walls) i j) then
               ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.wall), i, j, 0)    
           else 
                ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), i, j, 0)
           //draw the solution if the mode <> "Animation"
           if(Array2D.get mymaze.resolved i j = 0&&mode<>"Animation") then
                ignore <| engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Yellow,pixel.filled_player Color.Yellow), i, j, 1)
   
   //variable useful for solution mode where the player doesn't have to been seen
   let mutable visibility = 3
   if(mode<>"Solution") then visibility <- 3 else visibility <- (-1)
   
   //Creating the player's sprite
   let player = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Cyan), 1, 0, visibility)  

   let st0 = { 
           player = player
           }

   //choose the loop
   if(mode<>"Animation")then
    engine.loop_on_key my_update st0
   else
    engine.loop_on_key my_update_animation st0



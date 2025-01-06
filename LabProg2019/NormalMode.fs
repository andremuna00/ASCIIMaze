

module LabProg2019.NormalMode

open System
open Gfx
open Engine
open Maze

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let mutable w = 21
let mutable h = 21

let main () =       
    
    //create the engine and clear the console
    let engine = new engine (w, h)
    Console.Clear()
    Gfx.system_console_raster.at (0,0,Color.Black)
    
    //create the maze and the exit
    let mymaze = new maze (w,h)
    mymaze.generate
    mymaze.generateUscitaCasuale mymaze.walls

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            match key.KeyChar with 
            //victory case
            | 'w' | 'a' |'s'|'d' when st.player.x = 0. || st.player.y = 0. || st.player.x = float (w-1)  || st.player.y = float (h-1) -> 
                let victory = engine.create_and_register_sprite(image.rectangle (w, h, pixel.filled_wall Color.Black, pixel.filled_wall Color.Black), 0, 0, 2) 
                victory.draw_text ("SUCCESS", ((w-7)/2), ((h-3)/2), Color.White)
                victory.draw_text ("PRESS-Q/ESC-to", ((w-14)/2+1), ((h-3)/2+1), Color.White, Color.Black)
                victory.draw_text ("RETURN-TO-MENU", ((w-14)/2+1), ((h-3)/2+2), Color.White, Color.Black)
                0., 0.
            //checks if you go out of bounds
            | 'w' when (not(Array2D.get (mymaze.walls) (int st.player.x) (int st.player.y-1))) -> 0., -1.
            | 's' when (not(Array2D.get (mymaze.walls) (int st.player.x) (int st.player.y+1))) -> 0., 1.
            | 'a' when (not(Array2D.get (mymaze.walls) (int st.player.x-1) (int st.player.y))) -> -1., 0.
            | 'd' when (not(Array2D.get (mymaze.walls) (int st.player.x+1) (int st.player.y))) -> 1., 0.
            |_ -> 0., 0.
        st.player.move_by (dx, dy)
        //value to leave the loop: ESC|q
        st, key.KeyChar = '\027' || key.KeyChar = 'q'
    
    //drawing the maze
    for i=0 to (w-1) do
        for j=0 to (h-1) do
            if(Array2D.get (mymaze.walls) i j) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.wall), i, j, 0)    
            else 
                 ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), i, j, 0)
    
    //initializing player coords
    let x_init = fst (mymaze.StartPlayer mymaze.walls)
    let y_init = snd (mymaze.StartPlayer mymaze.walls)
    //creating the player's sprite
    let player = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled_player Color.Cyan, pixel.filled_player Color.Cyan), x_init, y_init, 1)

    let st0 = { 
        player = player
        }

    engine.loop_on_key my_update st0


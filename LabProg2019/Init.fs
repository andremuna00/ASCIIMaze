module LabProg2019.Init

open System
open Gfx
open Engine
open Maze

type state = {
    player : sprite
}

let main () =    
    let w = 27
    let h = 23

    //size of the maze
    let w_maze = 25
    let h_maze = 9

    let mutable engine = new engine (w, h)
    Console.Clear()
    Gfx.system_console_raster.at (0,0,Color.Black)

    //generates the maze
    let mymaze = new maze (w_maze,h_maze)
    mymaze.generate

    //draws the maze
    for i=0 to (w_maze-1) do
        for j=0 to (h_maze-1) do
            if(Array2D.get (mymaze.walls) i j) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.wall), i+1, j+2, 0)    
            else 
                 ignore <| engine.create_and_register_sprite (image.rectangle (1,1, External.CharInfo.path), i+1, j+2, 0)
    //the player is the rectangle box
    let player = engine.create_and_register_sprite (image.rectangle (15,3, pixel.filled_player Color.Red), 7, 15, 0)

    //creates the sprite of the various element of the menu
    let title = engine.create_and_register_sprite (image.rectangle (13,1, pixel.filled_player Color.Black), 7, 1, 0)
    title.draw_text( "  THE  MAZE  " , 0,0, Color.White)

    let scritta_music = engine.create_and_register_sprite (image.rectangle (13,1, pixel.filled_player Color.Black), 8, 12, 0)
    scritta_music.draw_text( "Music: "+Menu.music_mode+"  ->" , 0,0, Color.White)

    let scritta_play = engine.create_and_register_sprite (image.rectangle (13,1, pixel.filled_player Color.Black), 8, 16, 0)
    scritta_play.draw_text( "    PLAY     ", 0,0, Color.White)

    let scritta_tre = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 20, 0)
    scritta_tre.draw_text( "  QUIT  GAME  ", 0,0, Color.White)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            //the player can only move above each option of the menu
            match key.KeyChar with 
            |'w' when st.player.y<> 11.-> 0.,-4.
            |'s' when st.player.y<> 19.-> 0.,4.
            //Music mode = on|off
            |'d' when st.player.y = 11. && Menu.music_mode = "on" ->  Menu.music_mode <- "off" 
                                                                      scritta_music.draw_text( "Music: "+Menu.music_mode+" <- " , 0,0, Color.White)
                                                                      0., 0.
            |'a' when st.player.y = 11. && Menu.music_mode = "off" -> Menu.music_mode <- "on" 
                                                                      scritta_music.draw_text( "Music: "+Menu.music_mode+"  -> " , 0,0, Color.White)
                                                                      0., 0.
            |'\013' when st.player.y = 15. ->   Menu.main()
                                                0., -4.
            |_-> 0., 0.
        st.player.move_by (dx, dy)
        //update the engine (necesssarry to navigate from a menu to another)
        engine <- new engine (w, h)
        //Exit case (if you press Enter on the "QUIT" button)|ESC
        if(st.player.x = 7. && st.player.y = 19.) then
            st, key.KeyChar = '\013' || key.KeyChar = '\027'
        else
             st, key.KeyChar = '\027'

    let st0 = { 
        player = player
        }

    engine.loop_on_key my_update st0


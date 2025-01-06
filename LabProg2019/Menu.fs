
module LabProg2019.Menu

open System
open Gfx
open Engine
open Maze
open System.Media

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

//variable needed to set the music
let mutable music_mode = "on"

let main () =    
    
    let w = 28
    let h = 17
    
    let mutable engine = new engine (w, h)
    Console.Clear()
    Gfx.system_console_raster.at (0,0,Color.Black)
    //initializing the music
    let musica = new SoundPlayer ("soundtrack.wav")
    if(music_mode = "on") then musica.PlayLooping()
    else musica.Stop()

    //the player is the rectangle box
    let player = engine.create_and_register_sprite (image.rectangle (16,3, pixel.filled_player Color.Red), 7, 4, 0)

    //create the sprite of the various element of the menu
    let scritta_normal = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 5, 1)
    scritta_normal.draw_text( " NORMAL  MODE ", 0,0, Color.White)

    let scritta_automatic = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 11, 1)
    scritta_automatic.draw_text( "AUTOMATIC MODE", 0,0, Color.White)
    
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            //the player can only move above each option of the menu
            match key.KeyChar with 
            |'w' when st.player.y<> 4.-> 0.,-6.
            |'s' when st.player.y<> 10.-> 0.,6.
            |'\013' when st.player.y = 4. ->   MenuNormalMode.main()
                                               0., 0.
            |'\013' when st.player.y = 10. ->  MenuAutoResolveMode.main()
                                               0., -6.
            |_-> 0., 0.
        st.player.move_by (dx, dy)
        
        //update the engine and reset the variable
        engine <- new engine (w, h)
        Engine.ShowFPS <- false
        //Setting the exit case to  ESC
        st, key.KeyChar = '\027'

    let st0 = { 
        player = player
        }

    engine.loop_on_key my_update st0
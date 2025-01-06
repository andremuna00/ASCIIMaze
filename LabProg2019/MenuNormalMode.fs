module LabProg2019.MenuNormalMode

open System
open Gfx
open Engine

type state = {
    player : sprite
}

let main () =    
    let w = 28
    let h = 21

    let mutable engine = new engine (w, h)
    Console.Clear()
    Gfx.system_console_raster.at (0,0,Color.Black)
    //the player is the rectangle box
    let player = engine.create_and_register_sprite (image.rectangle (16,3, pixel.filled_player Color.Red), 7, 3, 0)
    
    //create the sprite of the various element of the menu
    let scritta_larghezza = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 4, 0)
    scritta_larghezza.draw_text( "<- WIDTH:"+(string NormalMode.w)+" ->" , 0,0, Color.White)

    let scritta_altezza = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 8, 0)
    scritta_altezza.draw_text( "<-HEIGHT:"+(string NormalMode.h)+" ->", 0,0, Color.White)

    let scritta_fps = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 12, 0)
    scritta_fps.draw_text( "SHOW FPS", 0,0, Color.White)
    scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Red)

    let scritta_Confirm = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 16, 0)
    scritta_Confirm.draw_text( "CONFIRM", 0,0, Color.White)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            //the player can only move above each option of the menu
            match key.KeyChar with 
            |'w' when st.player.y<> 3.-> 0.,-4.
            |'s' when st.player.y<> 15.-> 0.,4.
            //width: RANGE: [21-199]
            |'d' when st.player.y = 3. && NormalMode.w<199 ->  NormalMode.w <- NormalMode.w + 2
                                                               //if width>100 remove one "space" in order to make the dimension correct
                                                               if (NormalMode.w<100) then
                                                                scritta_larghezza.draw_text( "<- WIDTH:"+(string NormalMode.w)+" ->" , 0,0, Color.White)
                                                               else
                                                                scritta_larghezza.draw_text( "<- WIDTH:"+(string NormalMode.w)+"->" , 0,0, Color.White)
                                                               0., 0.
            |'a' when st.player.y = 3. && NormalMode.w>21 ->   NormalMode.w<-NormalMode.w-2                        
                                                               if (NormalMode.w<100) then
                                                                  scritta_larghezza.draw_text( "<- WIDTH:"+(string NormalMode.w)+" ->" , 0,0, Color.White)
                                                               else
                                                                  scritta_larghezza.draw_text( "<- WIDTH:"+(string NormalMode.w)+"->" , 0,0, Color.White)
                                                               0., 0.
            //height: RANGE: [21-45]
            |'d' when st.player.y = 7. && NormalMode.h<45 ->   NormalMode.h<-NormalMode.h+2                        
                                                               scritta_altezza.draw_text( "<-HEIGHT:"+(string NormalMode.h)+" ->", 0,0, Color.White)
                                                               0., 0.
            |'a' when st.player.y = 7. && NormalMode.h>21 ->   NormalMode.h<-NormalMode.h-2                        
                                                               scritta_altezza.draw_text( "<-HEIGHT:"+(string NormalMode.h)+" ->", 0,0, Color.White)
                                                               0., 0.
            //change the color of the sprite depending if you show or not the FPS
            |'\013' when st.player.y = 11. && not(Engine.ShowFPS)->Engine.ShowFPS<-true
                                                                   scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Green)
                                                                   0., 0.
            |'\013' when st.player.y = 11. && Engine.ShowFPS->   Engine.ShowFPS<-false
                                                                 scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Red)
                                                                 0., 0.
            |'\013' when st.player.y = 15. ->   NormalMode.main()
                                                0., -12.
            |_-> 0., 0.
        st.player.move_by (dx, dy)
        
        //update the engine (necesssarry to navigate from a menu to another)
        engine <- new engine (w, h)

        st, key.KeyChar = '\027'

    let st0 = { 
        player = player
        }

    engine.loop_on_key my_update st0

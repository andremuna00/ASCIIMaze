module LabProg2019.MenuAutoResolveMode

open System
open Gfx
open Engine

type state = {
    player : sprite
}

let main () =    
    let w = 28
    let h = 25

    let mutable engine = new engine (w, h)
    Console.Clear()
    Gfx.system_console_raster.at (0,0,Color.Black)
    //the player is the rectangle box
    let player = engine.create_and_register_sprite (image.rectangle (16,3, pixel.filled_player Color.Red), 7, 3, 0)

    //create the sprite of the various element of the menu
    let scritta_larghezza = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 4, 0)
    scritta_larghezza.draw_text( "<- WIDTH:"+(string AutoResolveMode.w)+" ->" , 0,0, Color.White)

    let scritta_altezza = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 8, 0)
    scritta_altezza.draw_text( "<-HEIGHT:"+(string AutoResolveMode.h)+" ->", 0,0, Color.White)

    let scritta_fps = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 12, 0)
    scritta_fps.draw_text( "SHOW FPS", 0,0, Color.White)
    scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Red)

    let scritta_mode = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 16, 0)
    scritta_mode.draw_text( "<- "+AutoResolveMode.mode+" ->", 0,0, Color.White)

    let scritta_Confirm = engine.create_and_register_sprite (image.rectangle (14,1, pixel.filled_player Color.Black), 8, 20, 0)
    scritta_Confirm.draw_text( "CONFIRM", 0,0, Color.White)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster)  (st : state) =
        let dx, dy =
            //the player can only move above each option of the menu
            match key.KeyChar with 
            |'w' when st.player.y<> 3.-> 0.,-4.
            |'s' when st.player.y<> 19.-> 0.,4.
            //width: RANGE: [21-199]
            |'d' when st.player.y = 3. && AutoResolveMode.w<199 ->  AutoResolveMode.w <- AutoResolveMode.w + 2  
                                                                    //if the width>100 remove one "space" in order to make the dimension correct
                                                                    if (AutoResolveMode.w<100) then
                                                                     scritta_larghezza.draw_text( "<- WIDTH:"+(string AutoResolveMode.w)+" ->" , 0,0, Color.White)
                                                                    else
                                                                     scritta_larghezza.draw_text( "<- WIDTH:"+(string AutoResolveMode.w)+"->" , 0,0, Color.White)
                                                                    0., 0.
            |'a' when st.player.y = 3. && AutoResolveMode.w>21 ->   AutoResolveMode.w<-AutoResolveMode.w-2                        
                                                                    if (AutoResolveMode.w<100) then
                                                                        scritta_larghezza.draw_text( "<- WIDTH:"+(string AutoResolveMode.w)+" ->" , 0,0, Color.White)
                                                                    else
                                                                        scritta_larghezza.draw_text( "<- WIDTH:"+(string AutoResolveMode.w)+"->" , 0,0, Color.White)
                                                                    0., 0.
            //height: RANGE: [21-45]
            |'d' when st.player.y = 7. && AutoResolveMode.h<45 ->   AutoResolveMode.h<-AutoResolveMode.h+2                        
                                                                    scritta_altezza.draw_text( "<-HEIGHT:"+(string AutoResolveMode.h)+" ->", 0,0, Color.White)
                                                                    0., 0.
            |'a' when st.player.y = 7. && AutoResolveMode.h>21 ->   AutoResolveMode.h<-AutoResolveMode.h-2                        
                                                                    scritta_altezza.draw_text( "<-HEIGHT:"+(string AutoResolveMode.h)+" ->", 0,0, Color.White)
                                                                    0., 0.
            //change the color of the sprite depending if you show or not the FPS
            |'\013' when st.player.y = 11. && not(Engine.ShowFPS)->Engine.ShowFPS<-true
                                                                   scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Green)
                                                                   0., 0.
            |'\013' when st.player.y = 11. && Engine.ShowFPS->   Engine.ShowFPS<-false
                                                                 scritta_fps.draw_rectangle(12, 0, 1,1, pixel.filled_player Color.Red)
                                                                 0., 0.
            //Select one mode between Solution - Playable - Animation and change the text on the menu
            |'d' when st.player.y = 15.-> if(AutoResolveMode.mode = "Playable") then    
                                               AutoResolveMode.mode<- "Animation"
                                               scritta_mode.draw_text( "<- "+AutoResolveMode.mode+"  ", 0,0, Color.White)
                                               0., 0.
                                          else if(AutoResolveMode.mode = "Solution") then 
                                                AutoResolveMode.mode<- "Playable"
                                                scritta_mode.draw_text( "<- "+AutoResolveMode.mode+" ->", 0,0, Color.White)
                                                0., 0.
                                          else
                                                0.,0.
            |'a' when st.player.y = 15.-> if(AutoResolveMode.mode = "Playable") then    
                                                AutoResolveMode.mode<- "Solution"
                                                scritta_mode.draw_text( "   "+AutoResolveMode.mode+" ->", 0,0, Color.White)
                                                0., 0.
                                          else if(AutoResolveMode.mode = "Animation") then 
                                                AutoResolveMode.mode<- "Playable"
                                                scritta_mode.draw_text( "<- "+AutoResolveMode.mode+" ->", 0,0, Color.White)
                                                0., 0.
                                          else
                                                0.,0.
            |'\013' when st.player.y = 19. ->   AutoResolveMode.main()
                                                0., -12.
            |_-> 0., 0.
        st.player.move_by (dx, dy)
        //update the engine (necesssarry to navigate from a menu to another)
        Console.Clear()
        engine <- new engine (w, h)

        st, key.KeyChar = '\027'

    let st0 = { 
        player = player
        }

    engine.loop_on_key my_update st0

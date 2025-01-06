(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx
open System

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.DarkGray)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled_wall Color.White
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall


type maze (w, h) as this =

    //TASK 1 (NORMAL MODE)
    //matrix whose element true = wall|false = path / visited cell
    member val walls = Array2D.create w h true with get,set

    //TASK 2 (AUTOMATIC MODES)
    //matrix for resolution with all values = 3. 
    (*Value: 
        |3 initial value
        |2 partial solution path
        |1 already visited cell
        |0 solution path
    *)
    member val resolved = Array2D.create w h 3 with get,set

    member __.generate =
        //starting point
        Array2D.set this.walls 1 1 false

        let rec create walls visitedcell position i j=
            //induction base case
            if(visitedcell<=((w/2)*(h/2))-1) then 

                //check that at least one of the sorrounding cells has never been visited
                if((i<>1&&Array2D.get walls (i-2) j)||(i<>(w-2)&&Array2D.get walls (i+2) j)||(j<>1&&Array2D.get walls i (j-2))||
                    (j<>(h-2)&&Array2D.get walls i (j+2))) then

                    let rec choose_direction n=
                        //make sure not to go beyond the limits of the matrix
                        if ((i=1&&n=1)||(i=w-2)&&n=2||(j=1)&&n=3||(j=h-2)&&n=4) then
                            choose_direction (rnd_int 1 4)
                        //check that the chosen cell has not already been visited
                        else if((n=1&&not(Array2D.get walls (i-2) j))||(n=2&&not(Array2D.get walls (i+2) j))||(n=3&&not(Array2D.get walls i (j-2)))||
                                (n=4&&not(Array2D.get walls i (j+2)))) then
                                choose_direction (rnd_int 1 4)
                             else n

                    //random value that chooses the direction
                    let n = choose_direction (rnd_int 1 4)

                    //UP
                    if (n=1&&Array2D.get walls (i-2) j) then
                            Array2D.set walls (i-2) j false //the cell becomes visited
                            Array2D.set walls (i-1) j false //deletes the wall between the current cell and the visited one                           
                            create walls (visitedcell+1) (((i-2),j)::position) (i-2) j //increase the cell counter and add the latest position to the list
                    //DOWN
                    else if (n=2&&Array2D.get walls (i+2) j) then
                            Array2D.set walls (i+2) j false 
                            Array2D.set walls (i+1) j false
                            create walls (visitedcell+1) (((i+2),j)::position) (i+2) j 
                    //LEFT
                    else if (n=3&&Array2D.get walls i (j-2)) then
                            Array2D.set walls i (j-2) false
                            Array2D.set walls i (j-1) false
                            create walls (visitedcell+1) ((i,(j-2))::position) i (j-2)   
                    //RIGHT
                    else if (n=4&&Array2D.get walls i (j+2)) then
                            Array2D.set walls i (j+2) false
                            Array2D.set walls i (j+1) false
                            create walls (visitedcell+1) ((i,(j+2))::position) i (j+2) 
                
                else
                    //BACKTRACKING
                    create walls (visitedcell) (List.tail position) (fst (List.head (List.tail position))) (snd (List.head (List.tail position)))
            
            else
                ()
        //Start on cell (1;1)
        create this.walls 1 [(1,1)] 1 1


    member __.generateUscitaCasuale walls = 
        //make sure the exit is reachable
        let rec uscita walls lato x y=
            if(lato=1&&not(Array2D.get walls x 1)) then
                Array2D.set walls x 0 false
            else if(lato=2&&not(Array2D.get walls (w-2) y)) then
                Array2D.set walls (w-1) y false
            else if(lato=3&&not(Array2D.get walls 1 y)) then
                Array2D.set walls 0 y false
            else if(lato=3&&not(Array2D.get walls x (h-2))) then
                Array2D.set walls x (h-1) false
            else
                uscita walls (rnd_int 1 4) (rnd_int 1 (w-2)) (rnd_int 1 (h-2))

        in uscita this.walls (rnd_int 1 4) (rnd_int 1 (w-2)) (rnd_int 1 (h-2))
    

    member __.generateUscitaAutoResolve walls =
        //Set exit and starting point
        Array2D.set this.walls (w-2) (h-1) false
        Array2D.set this.walls 1 0 false


    member __.StartPlayer walls = 
        //Generate random starting player coordinates
        let rec init x_init y_init= 
            if(Array2D.get walls (x_init) (y_init)) then
                    init (rnd_int 1 (w-2)) (rnd_int 1 (h-2))
            else
                    x_init, y_init
        in init (rnd_int 1 (w-2)) (rnd_int 1 (h-2))


    //TASK 2 (AUTOMATIC MODES)
    //matrix for resolution with all value = 3. 
    (*Value: 
        |3 initial value
        |2 partial solution path
        |1 already visited cell
        |0 solution path
    *)
    member __.autoresolve (walls) = 
        //find all the deadpoints and set the value of the matrix "resolved" with 2
        let deadpoint resolved walls=
            for i=1 to (w-2) do
                for j=1 to (h-2) do
                    if(not(Array2D.get walls i j)) then
                        //Exit on the bottom
                        if( (Array2D.get walls i (j-1)) && (Array2D.get walls (i-1) j) && (Array2D.get walls (i+1) j) ) then
                            Array2D.set resolved i j 2  
                        //Exit on the left
                        else if( (Array2D.get walls i (j-1)) && (Array2D.get walls (i+1) j) && (Array2D.get walls i (j+1)) ) then
                            Array2D.set resolved i j 2 
                        //Exit on the right
                        else if( (Array2D.get walls i (j-1)) && (Array2D.get walls (i-1) j) && (Array2D.get walls i (j+1)) ) then
                            Array2D.set resolved i j 2 
                        //Exit on the top
                        else if( (Array2D.get walls i (j+1)) && (Array2D.get walls (i-1) j) && (Array2D.get walls (i+1) j) ) then
                            Array2D.set resolved i j 2 
                        else
                            Array2D.set resolved i j 0

        in deadpoint this.resolved walls


        //check if an element "n" is present in the matrix "resolved"
        let rec isPresent resolved n i j=
            if(j<h-1) then
                if(i<w-1) then
                    (Array2D.get resolved i j = 2 )||isPresent resolved n (i+1) j
                else
                    isPresent resolved n 0 (j+1)
            else
                false          
                

        //complete the path from the deadpoint util the first crossroads           
        let rec fino_al_bivio resolved walls i=

            //induction base case
            if(isPresent resolved 2 0 0) then 
                for i=1 to (w-2) do
                    for j=1 to (h-2) do
                        //if the selected cell has the value 2 and there is only one possible path--> next cell<-2 and current cell<-1
                        if(Array2D.get resolved i j = 2) then

                            if(not(Array2D.get walls i (j-1)) && (Array2D.get walls i (j+1)||Array2D.get resolved i (j+1)=1) && (Array2D.get walls (i-1) j ||
                                    Array2D.get resolved (i-1) j=1) && (Array2D.get walls (i+1) j||Array2D.get resolved (i+1) j=1)) then//via libera solo sopra
                                Array2D.set resolved i (j-1) 2
                                Array2D.set resolved i j 1

                            else if((Array2D.get walls i (j-1)||Array2D.get resolved i (j-1)=1) && not(Array2D.get walls i (j+1)) && (Array2D.get walls (i-1) j ||
                                     Array2D.get resolved (i-1) j=1) && (Array2D.get walls (i+1) j||Array2D.get resolved (i+1) j=1)) then//via libera solo sotto
                                Array2D.set resolved i (j+1) 2
                                Array2D.set resolved i j 1

                            else if((Array2D.get walls i (j-1)||Array2D.get resolved i (j-1)=1) && (Array2D.get walls i (j+1)||Array2D.get resolved i (j+1)=1) &&
                                    (Array2D.get walls (i-1) j||Array2D.get resolved (i-1) j=1) && not(Array2D.get walls (i+1) j)) then//via libera solo destra
                                Array2D.set resolved (i+1) j 2
                                Array2D.set resolved i j 1

                            else if((Array2D.get walls i (j-1)||Array2D.get resolved i (j-1)=1) && (Array2D.get walls i (j+1)||Array2D.get resolved i (j+1)=1) &&
                                    not(Array2D.get walls (i-1) j) && (Array2D.get walls (i+1) j||Array2D.get resolved (i+1) j=1)) then//via libera solo sinistra
                                Array2D.set resolved (i-1) j 2
                                Array2D.set resolved i j 1

                            else
                                Array2D.set resolved i j 0    
                                
                fino_al_bivio resolved walls (i+1)

        fino_al_bivio this.resolved walls 0

open Vector3d.Vector3d
open Scene
open Parser


module Graphx
: sig
  
  val show_and_save : string -> (int * int * int) list list -> unit

end
= struct

    let rec loop () = Unix.sleep 1; loop ()
    
    let to_array list = 
        let
            rgb_list = List.map (List.map ( fun (r, g, b) -> Graphics.rgb r g b )) list
        in
            Array.of_list @@ List.map Array.of_list rgb_list
    
    let show_and_save name list = 
        let
            array = to_array list
        in
            try
                Graphics.open_graph "";
                Graphics.resize_window (Array.length array.(0)) (Array.length array); 
                let 
                    g_image = Graphics.make_image array
                in
                    Png.save name [] (Images.Rgb24 (Graphic_image.image_of g_image));
                    Graphics.draw_image g_image 0 0;
                    loop ();
            with 
                _ -> Graphics.close_graph ()
end;;



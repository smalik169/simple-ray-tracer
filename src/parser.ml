open Vector3d.Vector3d
open Object
open Scene
open Camera

module type PARSER 
= sig

    val parse_scene : string -> Camera.t * Scene.t * string

end;;

module Parser : PARSER
= struct
    
    open Yojson.Basic.Util;;
    
    let make_shape json = 
        match json |> member "type" |> to_string with
            | "sphere" -> 
                    let
                        center = json |> member "center" |> to_list |> List.map to_float |> vector_from_list
                    and
                        radius = json |> member "radius" |> to_float
                    in
                        Object.Sphere (center, radius)
            | "plane" ->
                    let
                        origin = json |> member "origin" |> to_list |> List.map to_float |> vector_from_list
                    and
                        normal = json |> member "normal" |> to_list |> List.map to_float |> vector_from_list
                    in
                        Object.Plane (origin, normalize normal)
            | "hyperbolic_paraboloid" ->
                    let
                        origin = json |> member "origin" |> to_list |> List.map to_float |> vector_from_list
                    and
                        q = json |> member "q" |> to_float
                    and
                        p = json |> member "p" |> to_float
                    in
                        Object.HyperbolicParaboloid (origin, q, p)
            | name -> failwith ("Unknonw shape: " ^ name)
    
    
    let make_material json =
        let
            color = json |> member "color" |> to_list |> List.map to_float |> vector_from_list
        and
            glow = json |> member "glow" |> to_list |> List.map to_float |> vector_from_list
        and
            diffusion = json |> member "diffusion" |> to_float
        and
            reflection = json |> member "reflection" |> to_float
        in
            if diffusion +. reflection > 1.0
            then failwith "Incorrect setting: diffusion + reflection >= 1.0"
            else 
                Object.{color = color; glow = glow; diffusion = diffusion; reflection = reflection}
    
    let make_object json =
        Object.{shape = make_shape (member "shape" json); material = make_material (member "material" json)}
                
    let make_light json = 
        match json |> member "type" |> to_string with
            | "light_bulb" -> 
                    let
                        position = json |> member "position" |> to_list |> List.map to_float |> vector_from_list
                    and
                        color = json |> member "color" |> to_list |> List.map to_float |> vector_from_list
                    and
                        intensity = json |> member "intensity" |> to_float
                    in
                        Scene.LightBulb (position, color, intensity )
            | "sun_light" -> 
                    let
                        direction = json |> member "direction" |> to_list |> List.map to_float |> vector_from_list
                    and
                        color = json |> member "color" |> to_list |> List.map to_float |> vector_from_list
                    and
                        intensity = json |> member "intensity" |> to_float
                    in
                        Scene.SunLight (direction, color, intensity )
            | name -> failwith ("Unknonw light: " ^ name)
    
    let make_camera json =
        let
            focus = json |> member "focus" |> to_list |> List.map to_float |> vector_from_list
        and
            target = json |> member "target" |> to_list |> List.map to_float |> vector_from_list
        and
            sky = json |> member "sky" |> to_list |> List.map to_float |> vector_from_list
        and
            field_of_view = json |> member "field_of_view" |> to_float
        and
            gamma = json |> member "gamma"
        in
            match json |> member "resolution" |> to_list |> List.map to_int with
                | [res_x; res_y] ->
                        if gamma = `Null
                        then
                            Camera.set_camera focus target sky field_of_view infinity (res_x, res_y)
                        else
                            Camera.set_camera ~gamma:(to_float gamma) focus target sky field_of_view infinity (res_x, res_y)
                | _ -> failwith "resolution should be passed as 2-element int list"
    
    let parse_scene file_name = 
        let
            json = Yojson.Basic.from_file file_name
        and
            scene = Scene.new_scene ()
        in
            let
                scene = List.fold_left (fun scene oj -> Scene.add_object scene (make_object oj)) scene (json |> member "objects" |> to_list)
            in
                let
                    scene = List.fold_left (fun scene lj -> Scene.add_light scene (make_light lj)) scene (json |> member "lights" |> to_list)
                and
                    camera = make_camera (json |> member "camera")
                and
                    name = json |> member "name" |> to_string
                in
                    (camera, scene, name)

end;;

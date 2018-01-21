open Vector3d.Vector3d
open Object
open Scene


module type CAMERA = 
sig
    
    type t
    
    module Scene : SCENE
    
    val set_camera : ?gamma:float -> vector -> vector -> vector -> float -> float -> int * int -> t
    val ray_trace : t -> Scene.t -> (int * int * int) list list
    
end;;

module Camera : (CAMERA with module Scene = Scene)
= struct
    
    type t = {
        focal_point: vector;
        direction: vector; 
        right: vector; 
        up: vector; 
        h: float; 
        w: float; 
        resolution: int*int;
        aperture: float;
        gamma: float
        }
    
    module Scene = Scene
    
    let set_camera ?(gamma = 2.2) focal_point target_point sky_direction field_of_view aperture resolution =
        let
            direction = normalize (target_point -- focal_point)
        in
            let
                right = normalize (cross direction sky_direction)
            in
                let
                    h = tanh field_of_view
                in
                    {
                        focal_point = focal_point; direction = direction; 
                        right = right; up = cross right direction; h = h; 
                        w = h *. (float_of_int (fst resolution)) /. (float_of_int (snd resolution));
                        resolution = resolution;
                        aperture = aperture;
                        gamma = gamma;
                    }

    let gamma_correction gamma v = vector (v.x ** gamma, v.y ** gamma, v.z ** gamma)

    let get_first_hit ray scene = 
        let closer acc obj =
            match acc, Object.hit ray obj with
                | Some (x, _), Some y -> 
                    if x < y
                    then acc
                    else Some (y, obj)
                | None, Some y -> Some (y, obj)
                | _, None -> acc
        in
            List.fold_left closer None (Scene.objects_list scene)

    let any_hit ray scene max_t= 
        List.exists (fun obj -> match Object.hit ray obj with None -> false | Some t -> t < max_t) (Scene.objects_list scene)

    let diffused_light point normal light scene =
        match light with
            | Scene.LightBulb (light_position, color, intensity) ->
                let
                    direction = normalize (light_position -- point)
                in
                    let
                        shadow_ray = {origin = Object.min_t *& normal ++ point; direction = direction}
                    in
                        let
                            dist = len (light_position -- shadow_ray.origin)
                        in
                            if any_hit shadow_ray scene dist 
                            then vector (0.0, 0.0, 0.0)
                            else (intensity /. (dist ** 2.0)) *. (abs_float (dot normal shadow_ray.direction)) *& color
            | Scene.SunLight (light_direction, color, intensity) ->
                    let
                        shadow_ray = {origin = Object.min_t *& normal ++ point; direction = normalize light_direction}
                    in
                        if any_hit shadow_ray scene infinity
                        then vector (0.0, 0.0, 0.0)
                        else intensity *. (abs_float (dot normal shadow_ray.direction)) *& color

    let direct_light point obj scene =
        let
            normal = Object.get_normal point obj
        and
            color = obj.Object.material.Object.color
        in
            let
                color_vec = List.fold_left (fun acc light -> acc ++ ((diffused_light point normal light scene) *% color)) (vector (0.0, 0.0, 0.0)) (Scene.lights_list scene)
            in
                color_vec
    
    let rec reflected_light max_depth (in_ray: ray) point obj scene = 
        if max_depth <= 0
        then vector (0.0, 0.0, 0.0)
        else
            let
                normal = Object.get_normal point obj
            in
                let 
                    direction = normalize ( in_ray.direction -- (2.0 *. (dot normal in_ray.direction) *& normal))
                in
                    let
                        out_ray = {origin = Object.min_t *& direction ++ point; direction = direction}
                    in
                        get_color_vector ~max_depth:(max_depth - 1) out_ray scene
    
    and get_color_vector ?(max_depth=5) ray scene = 
        match get_first_hit ray scene with
            | None -> vector (0.0, 0.0, 0.0)
            | Some (t, obj) -> 
                    let 
                        hit_point = t *& ray.direction ++ ray.origin
                    and
                        c_d = obj.Object.material.Object.diffusion
                    and
                        c_r = obj.Object.material.Object.reflection
                    in
                        let 
                            d_light = if c_d > 1e-12 then (direct_light hit_point obj scene) else vector (0.0, 0.0, 0.0)
                        and
                            r_light = if c_r > 1e-12 then (reflected_light max_depth ray hit_point obj scene) else vector (0.0, 0.0, 0.0)
                        and
                            glow = (1.0 /. (len (hit_point -- ray.origin)) ** 2.0) *& obj.Object.material.Object.glow
                        in
                            (c_d *& d_light) ++ (c_r *& r_light) ++ glow 
            
    let get_row row_num camera scene =
        let
            (width, height) = (float_of_int (fst camera.resolution), float_of_int (snd camera.resolution))
        in
            let rec get_pixel acc j = 
                if j < 0 
                then acc 
                else
                    let
                        (x, y) = ( 2.0 *. (float_of_int j) /. width -. 1.0, 
                                   1.0 -. 2.0 *. (float_of_int row_num) /. height)
                    in
                        let
                            direction = camera.direction ++ 
                                        (camera.w *. x *& camera.right) ++ 
                                        (camera.h *. y *& camera.up)
                        in
                            let ray = {
                                origin = camera.focal_point; 
                                direction = normalize direction}
                            in
                                let     
                                    color_vec = gamma_correction (1.0 /. camera.gamma) (clamp (get_color_vector ray scene))
                                in
                                    let
                                        color = (int_of_float (255.0 *. color_vec.x), int_of_float (255.0 *. color_vec.y), int_of_float (255.0 *. color_vec.z))
                                    in
                                        get_pixel (color::acc) (j-1)
            in
                get_pixel [] (fst camera.resolution - 1)
    
    let ray_trace camera scene = 
        let rec get_picture acc i = 
            if i < 0
            then acc
            else
                get_picture ((get_row i camera scene)::acc) (i-1)
        in
            get_picture [] (snd camera.resolution - 1)
            
    
    
end;;

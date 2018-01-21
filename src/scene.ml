open Vector3d.Vector3d
open Object

module type SCENE =
sig
    
    type light = | LightBulb of vector * vector * float (* position * color * intensity *)
                 | SunLight of vector * vector * float (* direction * color * intensity *)
    type object_t = Object.t
    type t
    
    val new_scene : unit -> t
    val add_light : t -> light -> t
    val add_object : t -> object_t -> t
    
    val objects_list : t -> object_t list
    val lights_list : t -> light list

end;;

module Scene : SCENE
= struct
    
    type light = | LightBulb of vector * vector * float (* position * color * intensity *)
                 | SunLight of vector * vector * float (* direction * color * intensity *)
    type object_t = Object.t
    type t = {objects: object_t list; lights: light list}
    
    let new_scene () = {objects = []; lights = []}
    
    let add_light scene new_light = {objects = scene.objects; lights = new_light::scene.lights}
    let add_object scene new_object = {objects = new_object::scene.objects; lights = scene.lights}
    
    let objects_list scene = scene.objects
    let lights_list scene = scene.lights

end;;

module Vector3d =
struct

    type vector = {x: float; y: float; z: float}
    
    let vector (x, y, z) = {x=x; y=y; z=z}

    let vector_from_list = function
        | [x; y; z] -> vector (x, y, z)
        | _ -> failwith "Can create vectors only from 3-elemnt list"
    
    let ( *& ) a v = {x = a *. v.x; y = a *. v.y; z = a *. v.z}

    let ( ++ ) u v = {x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z}
    let ( -- ) u v = {x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z}
    let ( *% ) u v = {x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z}

    let dot u v = u.x *. v.x +. u.y *. v.y +. u.z *. v.z
    let cross u v = {x = u.y *. v.z -. u.z *. v.y; y = u.z *. v.x -. u.x *. v.z; z = u.x *. v.y -. u.y *. v.x}
    let len v = sqrt (dot v v)
    let normalize v = (1.0 /. (len v)) *& v
    let clamp ?(lo=0.0) ?(hi=1.0) v = {x = min (max v.x lo) hi; y = min (max v.y lo) hi; z = min (max v.z lo) hi}
    
    let str_of_vec v = Printf.sprintf "%f %f %f" v.x v.y v.z

end;;

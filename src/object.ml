open Vector3d.Vector3d

type ray = {origin: vector; direction: vector}

module Object =
struct
    
    let min_t = 1e-8;;

    type shape = Plane of vector * vector | Sphere of vector * float | HyperbolicParaboloid of vector * float * float
    type material = {color: vector; glow: vector; diffusion: float; reflection: float;} (*!!! diffusion + reflection <= 1 !!!*)
    type t = {shape: shape; material: material}


    let solve_quadratic a b c = 
        let 
            discriminant = b ** 2.0 -. 4.0 *. a *. c
        in
            if discriminant < 0.0
            then None
            else
                let
                    t1 = (-.b -. sqrt discriminant) /. (2.0 *. a)
                and
                    t2 = (-.b +. sqrt discriminant) /. (2.0 *. a)
                in
                    let 
                        t1, t2 = min t1 t2, max t1 t2
                    in
                        Some (t1, t2)

    
    let hit ray obj = 
        match obj.shape with
            | Plane (origin, normal) -> 
                let 
                    dot_product = dot ray.direction normal
                in
                    if dot_product=0.0
                    then None
                    else 
                        let 
                            t = (( dot (origin -- ray.origin) normal ) /. dot_product)
                        in
                            if t < min_t
                            then None
                            else Some t

            | Sphere (center, radius) ->
                let
                    origin' = ray.origin -- center
                in
                    let
                        a = dot ray.direction ray.direction
                    and
                        b = 2.0 *. (dot origin' ray.direction)
                    and
                        c = (dot origin' origin') -. radius ** 2.0
                    in
                        (match solve_quadratic a b c with
                            | None -> None
                            | Some (t1, t2) -> 
                                    if t2 < min_t
                                    then None
                                    else
                                        if t1 >= min_t
                                        then Some t1
                                        else Some t2)
            | HyperbolicParaboloid (origin, q, p) ->
                let
                    origin' = ray.origin -- origin
                in
                    let
                        a = ((ray.direction.x ** 2.0) /. q) -. ((ray.direction.y ** 2.0) /. p )
                    and
                        b = 2.0 *. (origin'.x *. ray.direction.x /. q -. origin'.y *. ray.direction.y /. p -. ray.direction.z)
                    and
                        c = ((origin'.x ** 2.0) /. q) -. ((origin'.y ** 2.0) /. p) -. (2.0 *. origin'.z)
                    in
                        match solve_quadratic a b c with
                            | None -> None
                            | Some (t1, t2) -> 
                                    if t2 < min_t
                                    then None
                                    else
                                        if t1 >= min_t
                                        then Some t1
                                        else Some t2
    
    let get_normal point obj =
            match obj.shape with
                | Plane (_, normal) -> normal
                | Sphere (center, _) -> normalize (point -- center)
                | HyperbolicParaboloid (origin, q, p) -> 
                        let
                            off = point -- origin
                        in
                            normalize @@ vector (2.0 *. off.x /. q, -2.0 *. off.y /. p, -2.0)


end;;


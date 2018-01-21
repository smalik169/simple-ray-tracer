open Vector3d.Vector3d
open Camera
open Parser

open Graphx

let (camera, scene, name) = Parser.parse_scene Sys.argv.(1);;

Graphx.show_and_save name @@ Camera.ray_trace camera scene;;

open States
open Blockblast

let () = Random.self_init ()
let () = States.TitleState.(setup () |> loop)

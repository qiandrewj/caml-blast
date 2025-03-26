open States

let () = Random.self_init ()
let () = States.TitleState.(setup () |> loop)
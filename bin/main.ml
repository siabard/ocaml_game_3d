open Game_3d


let () = let g = new Game.game in
         let _ = (g#init 1024 768) in
         g#gameloop 

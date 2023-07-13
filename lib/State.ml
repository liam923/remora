open! Base

module Identity = struct
  include Monad.Make2 (struct
    type ('a, 'e) t = 'a

    let return x = x
    let bind m ~f = f m
    let map = `Custom (fun m ~f -> f m)
  end)

  type ('a, 'e) t = 'a
end

include StateT.Make2 (Identity)

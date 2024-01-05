open! Base
open Remora
open MResult.Let_syntax

let%test_unit "both" =
  [%test_eq: (int * int, int) MResult.t] (MResult.both (MOk 1) (MOk 2)) (MOk (1, 2));
  [%test_eq: (int * int, int) MResult.t]
    (MResult.both (MOk 1) (MResult.err 2))
    (MResult.err 2);
  [%test_eq: (int * int, int) MResult.t]
    (MResult.both (MResult.err 1) (MOk 2))
    (MResult.err 1);
  [%test_eq: (int * int, int) MResult.t]
    (MResult.both (MResult.err 1) (MResult.err 2))
    (Errors [ 1; 2 ]);
  [%test_eq: (int * int, int) MResult.t]
    (MResult.both (MResult.err 1) (MResult.Errors [ 2; 3; 4 ]))
    (Errors [ 1; 2; 3; 4 ]);
  [%test_eq: (int * int, int) MResult.t]
    (MResult.both (MResult.Errors [ 1; 2 ]) (MResult.Errors [ 3; 4; 5 ]))
    (Errors [ 1; 2; 3; 4; 5 ])
;;

let%test_unit "all" =
  [%test_eq: (int list, int) MResult.t] (MResult.all [ MOk 1; MOk 2 ]) (MOk [ 1; 2 ]);
  [%test_eq: (int list, int) MResult.t]
    (MResult.all [ MOk 1; MResult.err 2 ])
    (MResult.err 2);
  [%test_eq: (int list, int) MResult.t]
    (MResult.all [ MResult.err 1; MOk 2 ])
    (MResult.err 1);
  [%test_eq: (int list, int) MResult.t]
    (MResult.all [ MResult.err 1; MResult.err 2 ])
    (Errors [ 1; 2 ]);
  [%test_eq: (int list, int) MResult.t]
    (MResult.all [ MResult.err 1; MResult.err 2; MResult.err 3 ])
    (Errors [ 1; 2; 3 ]);
  [%test_eq: (int list, int) MResult.t] (MResult.all [ MResult.err 1 ]) (Errors [ 1 ])
;;

let%test_unit "let" =
  [%test_eq: (int * int, int) MResult.t]
    (let%map a = MResult.MOk 1
     and b = MResult.MOk 2 in
     a, b)
    (MOk (1, 2));
  [%test_eq: (int * int, int) MResult.t]
    (let%map a = MResult.MOk 1
     and b = MResult.err 2 in
     a, b)
    (MResult.err 2);
  [%test_eq: (int * int, int) MResult.t]
    (let%map a = MResult.err 1
     and b = MResult.MOk 2 in
     a, b)
    (MResult.err 1);
  [%test_eq: (int * int, int) MResult.t]
    (let%map a = MResult.err 1
     and b = MResult.err 2 in
     a, b)
    (Errors [ 1; 2 ])
;;

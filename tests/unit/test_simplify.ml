open! Base
open Remora

let%expect_test "check simplifying" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
  checkAndPrint {| 5 |};
  [%expect {|
    5 |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect {|
    3 |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (let ((+arg1.55 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.57 +arg1.55)) (+ +arg1.57 4))
        (body-matcher map-result.56) (map-result (map-result.56))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (t-fn (@t) "hello"){int| }
  |};
  [%expect {|
    (frame 'h' 'e' 'l' 'l' 'o') |}];
  checkAndPrint {|
    (define (id{@t| } [x @t]) x)
    (id{int| } 5)
  |};
  [%expect {|
    5 |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      (id{(Forall (@t) (-> (@t) @t))| } id)
    |};
  [%expect {|
    (values) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app (id{(Forall (@t) (-> (@t) @t))| } id) int) 5)
    |};
  [%expect {|
    5 |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect {|
    10 |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect {|
    5 |}];
  checkAndPrint {| 
    (reduce{int | 4 []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (let ((reduce-arg.58 (frame 1 2 3 4 5)))
     (#1
      (loop-block (frame-shape 5)
       (map ((reduce-arg.59 reduce-arg.58)) (values reduce-arg.59))
       (body-matcher (reduce-arg.57)) (map-result ())
       (consumer
        (reduce (reduce-arg1.52 reduce-arg2.53 reduce-arg.57)
         (+ reduce-arg1.52 reduce-arg2.53)))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.59)
       (map-result (map-result.59)) (consumer (values))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      (+
        (length{int | 5 []} [1 2 3 4 5])
        (length{char | 2 [2]} ["hi" "ih"]))
    |};
  [%expect {|
    7 |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.85 (frame 5 6 7)) (+arg1.91 (frame 1 2 3)))
     (let
      ((y.94
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.93 +arg1.91)) (+ +arg1.93 4))
           (body-matcher map-result.92) (map-result (map-result.92))
           (consumer (values)))))))
      (let ((+arg1.100 y.94) (+arg2.101 y.94))
       (let
        ((+arg2.106
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.103 +arg1.100) (+arg2.104 +arg2.101))
              (+ +arg1.103 +arg2.104))
             (body-matcher map-result.102) (map-result (map-result.102))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.87 x.85))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.108 +arg2.106)) (+ x.87 +arg2.108))
               (body-matcher map-result.107) (map-result (map-result.107))
               (consumer (values))))))
           (body-matcher map-result.86) (map-result (map-result.86))
           (consumer (values))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x y))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.75 (frame 5 6 7)) (+arg1.81 (frame 1 2 3)))
     (let
      ((+arg2.88
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.83 +arg1.81)) (+ +arg1.83 4))
           (body-matcher map-result.82) (map-result (map-result.82))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.77 x.75))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.90 +arg2.88)) (+ x.77 +arg2.90))
             (body-matcher map-result.89) (map-result (map-result.89))
             (consumer (values))))))
         (body-matcher map-result.76) (map-result (map-result.76))
         (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo 5)
    |};
  [%expect
    {|
    (let ((+arg1.86 (frame 1 2 3)))
     (let
      ((y.89
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.88 +arg1.86)) (+ +arg1.88 4))
           (body-matcher map-result.87) (map-result (map-result.87))
           (consumer (values)))))))
      (let ((+arg1.95 y.89) (+arg2.96 y.89))
       (let
        ((+arg2.101
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.98 +arg1.95) (+arg2.99 +arg2.96))
              (+ +arg1.98 +arg2.99))
             (body-matcher map-result.97) (map-result (map-result.97))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.103 +arg2.101)) (+ 5 +arg2.103))
           (body-matcher map-result.102) (map-result (map-result.102))
           (consumer (values))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] x))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.85 (frame 5 6 7)) (+arg1.91 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.87 x.85))
         (let
          ((y.94
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.93 +arg1.91)) (+ +arg1.93 x.87))
               (body-matcher map-result.92) (map-result (map-result.92))
               (consumer (values)))))))
          (let ((+arg1.100 y.94) (+arg2.101 y.94))
           (let
            ((+arg2.106
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.103 +arg1.100) (+arg2.104 +arg2.101))
                  (+ +arg1.103 +arg2.104))
                 (body-matcher map-result.102) (map-result (map-result.102))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.108 +arg2.106)) (+ x.87 +arg2.108))
               (body-matcher map-result.107) (map-result (map-result.107))
               (consumer (values)))))))))
        (body-matcher map-result.86) (map-result (map-result.86))
        (consumer (values)))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ 3 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.77 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.79 x.77)) (+ x.79 14))
        (body-matcher map-result.78) (map-result (map-result.78))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.57 (frame 3 4)) (+arg2.58 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.60 +arg1.57) (+arg2.61 +arg2.58)) (+ +arg1.60 +arg2.61))
         (body-matcher map-result.59) (map-result (map-result.59))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.77 (frame 1 2)) (+arg2.78 (frame 3 4)) (+arg1.85 (frame 1 2))
      (+arg2.86 (frame 3 4)) (+arg1.93 (frame 1 2)) (+arg2.94 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78)) (+ +arg1.80 +arg2.81))
          (body-matcher map-result.79) (map-result (map-result.79))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.88 +arg1.85) (+arg2.89 +arg2.86)) (+ +arg1.88 +arg2.89))
          (body-matcher map-result.87) (map-result (map-result.87))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.96 +arg1.93) (+arg2.97 +arg2.94)) (+ +arg1.96 +arg2.97))
          (body-matcher map-result.95) (map-result (map-result.95))
          (consumer (values))))))
      (frame (frame 4 5) (frame 6 7) (frame 8 9)))) |}];
  checkAndPrint {| [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]] |};
  [%expect
    {|
    (frame (frame (frame 1 2) (frame 3 4) (frame 5 6))
     (frame (frame 7 8) (frame 9 10) (frame 11 12))) |}];
  checkAndPrint "(append{int | 3 2 []} [1 2 3] [4 5])";
  [%expect {| (frame 1 2 3 4 5) |}];
  checkAndPrint "(append{int | 3 2 [1]} [[1] [2] [3]] [[4] [5]])";
  [%expect {| (frame (frame 1) (frame 2) (frame 3) (frame 4) (frame 5)) |}];
  checkAndPrint "[[1 1] [2 2] (+ [2 2] 1)]";
  [%expect
    {|
    (let ((+arg1.55 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.57 +arg1.55)) (+ +arg1.57 1))
         (body-matcher map-result.56) (map-result (map-result.56))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.62 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.64 +arg1.62)) (+ +arg1.64 1))
         (body-matcher map-result.63) (map-result (map-result.63))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.46)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.48 : iota.46))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.50 : iota.48)) iota.50)
               (body-matcher map-result.49) (map-result (map-result.49))
               (consumer (values))))))
           (body-matcher map-result.47) (map-result (map-result.47))
           (consumer (values))))))
       (body-matcher map-result.45) (map-result (map-result.45))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (index
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.53)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.55 : iota.53))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.57 : iota.55))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.59 : iota.57))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.61 : iota.59)) iota.61)
                        (body-matcher map-result.60) (map-result (map-result.60))
                        (consumer (values))))))
                    (body-matcher map-result.58) (map-result (map-result.58))
                    (consumer (values))))))
                (body-matcher map-result.56) (map-result (map-result.56))
                (consumer (values))))))
            (body-matcher map-result.54) (map-result (map-result.54))
            (consumer (values))))))
        (body-matcher map-result.52) (map-result (map-result.52))
        (consumer (values)))))
     (frame 0 1 0)) |}];
  checkAndPrint
    {|
    (define (foo [x int])
      (+ [1 2 3] 4))
    (foo (array [0] int))
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 0)
       (map ()
        (let ((+arg1.66 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3) (map ((+arg1.68 +arg1.66)) (+ +arg1.68 4))
            (body-matcher map-result.67) (map-result (map-result.67))
            (consumer (values)))))))
       (body-matcher map-result.62) (map-result (map-result.62))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.52 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.54 index-value.52))
         (index-let ((i.44 runtime-value index-value.54))
          (box (i.44)
           (#0
            (#0
             (loop-block (frame-shape i.44) (map () 5)
              (body-matcher map-result.58) (map-result (map-result.58))
              (consumer (values))))))))
        (body-matcher map-result.53) (map-result (map-result.53))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.44 runtime-value (frame 1 2 3)))
     (box ((shape @i.44))
      (#0
       (#0
        (loop-block (frame-shape @i.44) (map () 5) (body-matcher map-result.56)
         (map-result (map-result.56)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((reduce-arg.69 (frame 1 2 3))
      (+arg2.77
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.75) iota.75)
          (body-matcher map-result.74) (map-result (map-result.74))
          (consumer (values)))))))
     (let
      ((x.71
        (#1
         (loop-block (frame-shape 3)
          (map ((reduce-arg.70 reduce-arg.69)) (values reduce-arg.70))
          (body-matcher (reduce-arg.68)) (map-result ())
          (consumer
           (reduce (reduce-arg1.58 reduce-arg2.59 reduce-arg.68)
            (+ reduce-arg1.58 reduce-arg2.59)))))))
      (#0
       (#0
        (loop-block (frame-shape 1001)
         (map ((+arg2.79 +arg2.77)) (+ x.71 +arg2.79))
         (body-matcher map-result.78) (map-result (map-result.78))
         (consumer (values))))))) |}]
;;

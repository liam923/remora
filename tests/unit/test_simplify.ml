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
    (let ((+arg1.73 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.75 +arg1.73)) (+ +arg1.75 4))
        (body-matcher map-result.74) (map-result (map-result.74))
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
    (let ((reduce-arg.76 (frame 1 2 3 4 5)))
     (#1
      (loop-block (frame-shape 5)
       (map ((reduce-arg.77 reduce-arg.76)) (values reduce-arg.77))
       (body-matcher (reduce-arg.75)) (map-result ())
       (consumer
        (reduce (reduce-arg1.70 reduce-arg2.71 reduce-arg.75)
         (+ reduce-arg1.70 reduce-arg2.71)))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.77)
       (map-result (map-result.77)) (consumer (values))))) |}];
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
    (let ((x.103 (frame 5 6 7)) (+arg1.109 (frame 1 2 3)))
     (let
      ((y.112
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.111 +arg1.109)) (+ +arg1.111 4))
           (body-matcher map-result.110) (map-result (map-result.110))
           (consumer (values)))))))
      (let ((+arg1.118 y.112) (+arg2.119 y.112))
       (let
        ((+arg2.124
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.121 +arg1.118) (+arg2.122 +arg2.119))
              (+ +arg1.121 +arg2.122))
             (body-matcher map-result.120) (map-result (map-result.120))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.105 x.103))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.126 +arg2.124)) (+ x.105 +arg2.126))
               (body-matcher map-result.125) (map-result (map-result.125))
               (consumer (values))))))
           (body-matcher map-result.104) (map-result (map-result.104))
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
    (let ((x.93 (frame 5 6 7)) (+arg1.99 (frame 1 2 3)))
     (let
      ((+arg2.106
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.101 +arg1.99)) (+ +arg1.101 4))
           (body-matcher map-result.100) (map-result (map-result.100))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.95 x.93))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.108 +arg2.106)) (+ x.95 +arg2.108))
             (body-matcher map-result.107) (map-result (map-result.107))
             (consumer (values))))))
         (body-matcher map-result.94) (map-result (map-result.94))
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
    (let ((+arg1.104 (frame 1 2 3)))
     (let
      ((y.107
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.106 +arg1.104)) (+ +arg1.106 4))
           (body-matcher map-result.105) (map-result (map-result.105))
           (consumer (values)))))))
      (let ((+arg1.113 y.107) (+arg2.114 y.107))
       (let
        ((+arg2.119
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.116 +arg1.113) (+arg2.117 +arg2.114))
              (+ +arg1.116 +arg2.117))
             (body-matcher map-result.115) (map-result (map-result.115))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.121 +arg2.119)) (+ 5 +arg2.121))
           (body-matcher map-result.120) (map-result (map-result.120))
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
    (let ((x.103 (frame 5 6 7)) (+arg1.109 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.105 x.103))
         (let
          ((y.112
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.111 +arg1.109)) (+ +arg1.111 x.105))
               (body-matcher map-result.110) (map-result (map-result.110))
               (consumer (values)))))))
          (let ((+arg1.118 y.112) (+arg2.119 y.112))
           (let
            ((+arg2.124
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.121 +arg1.118) (+arg2.122 +arg2.119))
                  (+ +arg1.121 +arg2.122))
                 (body-matcher map-result.120) (map-result (map-result.120))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.126 +arg2.124)) (+ x.105 +arg2.126))
               (body-matcher map-result.125) (map-result (map-result.125))
               (consumer (values)))))))))
        (body-matcher map-result.104) (map-result (map-result.104))
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
    (let ((x.95 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.97 x.95)) (+ x.97 14))
        (body-matcher map-result.96) (map-result (map-result.96))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.75 (frame 3 4)) (+arg2.76 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.78 +arg1.75) (+arg2.79 +arg2.76)) (+ +arg1.78 +arg2.79))
         (body-matcher map-result.77) (map-result (map-result.77))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.95 (frame 1 2)) (+arg2.96 (frame 3 4)) (+arg1.103 (frame 1 2))
      (+arg2.104 (frame 3 4)) (+arg1.111 (frame 1 2)) (+arg2.112 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.98 +arg1.95) (+arg2.99 +arg2.96)) (+ +arg1.98 +arg2.99))
          (body-matcher map-result.97) (map-result (map-result.97))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.106 +arg1.103) (+arg2.107 +arg2.104))
           (+ +arg1.106 +arg2.107))
          (body-matcher map-result.105) (map-result (map-result.105))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.114 +arg1.111) (+arg2.115 +arg2.112))
           (+ +arg1.114 +arg2.115))
          (body-matcher map-result.113) (map-result (map-result.113))
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
    (let ((+arg1.73 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.75 +arg1.73)) (+ +arg1.75 1))
         (body-matcher map-result.74) (map-result (map-result.74))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.80 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.82 +arg1.80)) (+ +arg1.82 1))
         (body-matcher map-result.81) (map-result (map-result.81))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.64)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.66 : iota.64))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.68 : iota.66)) iota.68)
               (body-matcher map-result.67) (map-result (map-result.67))
               (consumer (values))))))
           (body-matcher map-result.65) (map-result (map-result.65))
           (consumer (values))))))
       (body-matcher map-result.63) (map-result (map-result.63))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (index
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.71)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.73 : iota.71))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.75 : iota.73))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.77 : iota.75))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.79 : iota.77)) iota.79)
                        (body-matcher map-result.78) (map-result (map-result.78))
                        (consumer (values))))))
                    (body-matcher map-result.76) (map-result (map-result.76))
                    (consumer (values))))))
                (body-matcher map-result.74) (map-result (map-result.74))
                (consumer (values))))))
            (body-matcher map-result.72) (map-result (map-result.72))
            (consumer (values))))))
        (body-matcher map-result.70) (map-result (map-result.70))
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
        (let ((+arg1.84 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3) (map ((+arg1.86 +arg1.84)) (+ +arg1.86 4))
            (body-matcher map-result.85) (map-result (map-result.85))
            (consumer (values)))))))
       (body-matcher map-result.80) (map-result (map-result.80))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.70 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.72 index-value.70))
         (index-let ((i.62 runtime-value index-value.72))
          (box (i.62)
           (#0
            (#0
             (loop-block (frame-shape i.62) (map () 5)
              (body-matcher map-result.76) (map-result (map-result.76))
              (consumer (values))))))))
        (body-matcher map-result.71) (map-result (map-result.71))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.62 runtime-value (frame 1 2 3)))
     (box ((shape @i.62))
      (#0
       (#0
        (loop-block (frame-shape @i.62) (map () 5) (body-matcher map-result.74)
         (map-result (map-result.74)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((reduce-arg.87 (frame 1 2 3))
      (+arg2.95
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.93) iota.93)
          (body-matcher map-result.92) (map-result (map-result.92))
          (consumer (values)))))))
     (let
      ((x.89
        (#1
         (loop-block (frame-shape 3)
          (map ((reduce-arg.88 reduce-arg.87)) (values reduce-arg.88))
          (body-matcher (reduce-arg.86)) (map-result ())
          (consumer
           (reduce (reduce-arg1.76 reduce-arg2.77 reduce-arg.86)
            (+ reduce-arg1.76 reduce-arg2.77)))))))
      (#0
       (#0
        (loop-block (frame-shape 1001)
         (map ((+arg2.97 +arg2.95)) (+ x.89 +arg2.97))
         (body-matcher map-result.96) (map-result (map-result.96))
         (consumer (values))))))) |}]
;;

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
    (let ((+arg1.50 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.52 +arg1.50)) (+ +arg1.52 4))
        (body-matcher map-result.51) (map-result (map-result.51))
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
    (let ((reduce-arg.53 (frame 1 2 3 4 5)))
     (#1
      (loop-block (frame-shape 5)
       (map ((reduce-arg.54 reduce-arg.53)) (values reduce-arg.54))
       (body-matcher (reduce-arg.52)) (map-result ())
       (consumer
        (reduce (reduce-arg1.47 reduce-arg2.48 reduce-arg.52)
         (+ reduce-arg1.47 reduce-arg2.48)))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.54)
       (map-result (map-result.54)) (consumer (values))))) |}];
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
    (let ((x.80 (frame 5 6 7)) (+arg1.86 (frame 1 2 3)))
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
           (map ((x.82 x.80))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.103 +arg2.101)) (+ x.82 +arg2.103))
               (body-matcher map-result.102) (map-result (map-result.102))
               (consumer (values))))))
           (body-matcher map-result.81) (map-result (map-result.81))
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
    (let ((x.70 (frame 5 6 7)) (+arg1.76 (frame 1 2 3)))
     (let
      ((+arg2.83
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.78 +arg1.76)) (+ +arg1.78 4))
           (body-matcher map-result.77) (map-result (map-result.77))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.72 x.70))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.85 +arg2.83)) (+ x.72 +arg2.85))
             (body-matcher map-result.84) (map-result (map-result.84))
             (consumer (values))))))
         (body-matcher map-result.71) (map-result (map-result.71))
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
    (let ((+arg1.81 (frame 1 2 3)))
     (let
      ((y.84
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.83 +arg1.81)) (+ +arg1.83 4))
           (body-matcher map-result.82) (map-result (map-result.82))
           (consumer (values)))))))
      (let ((+arg1.90 y.84) (+arg2.91 y.84))
       (let
        ((+arg2.96
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.93 +arg1.90) (+arg2.94 +arg2.91))
              (+ +arg1.93 +arg2.94))
             (body-matcher map-result.92) (map-result (map-result.92))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg2.98 +arg2.96)) (+ 5 +arg2.98))
           (body-matcher map-result.97) (map-result (map-result.97))
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
    (let ((x.80 (frame 5 6 7)) (+arg1.86 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.82 x.80))
         (let
          ((y.89
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.88 +arg1.86)) (+ +arg1.88 x.82))
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
               (map ((+arg2.103 +arg2.101)) (+ x.82 +arg2.103))
               (body-matcher map-result.102) (map-result (map-result.102))
               (consumer (values)))))))))
        (body-matcher map-result.81) (map-result (map-result.81))
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
    (let ((x.72 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.74 x.72)) (+ x.74 14))
        (body-matcher map-result.73) (map-result (map-result.73))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.52 (frame 3 4)) (+arg2.53 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.55 +arg1.52) (+arg2.56 +arg2.53)) (+ +arg1.55 +arg2.56))
         (body-matcher map-result.54) (map-result (map-result.54))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.72 (frame 1 2)) (+arg2.73 (frame 3 4)) (+arg1.80 (frame 1 2))
      (+arg2.81 (frame 3 4)) (+arg1.88 (frame 1 2)) (+arg2.89 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.75 +arg1.72) (+arg2.76 +arg2.73)) (+ +arg1.75 +arg2.76))
          (body-matcher map-result.74) (map-result (map-result.74))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.83 +arg1.80) (+arg2.84 +arg2.81)) (+ +arg1.83 +arg2.84))
          (body-matcher map-result.82) (map-result (map-result.82))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.91 +arg1.88) (+arg2.92 +arg2.89)) (+ +arg1.91 +arg2.92))
          (body-matcher map-result.90) (map-result (map-result.90))
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
    (let ((+arg1.50 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.52 +arg1.50)) (+ +arg1.52 1))
         (body-matcher map-result.51) (map-result (map-result.51))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.57 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.59 +arg1.57)) (+ +arg1.59 1))
         (body-matcher map-result.58) (map-result (map-result.58))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.41)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.43 : iota.41))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.45 : iota.43)) iota.45)
               (body-matcher map-result.44) (map-result (map-result.44))
               (consumer (values))))))
           (body-matcher map-result.42) (map-result (map-result.42))
           (consumer (values))))))
       (body-matcher map-result.40) (map-result (map-result.40))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (index
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.48)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.50 : iota.48))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.52 : iota.50))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.54 : iota.52))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.56 : iota.54)) iota.56)
                        (body-matcher map-result.55) (map-result (map-result.55))
                        (consumer (values))))))
                    (body-matcher map-result.53) (map-result (map-result.53))
                    (consumer (values))))))
                (body-matcher map-result.51) (map-result (map-result.51))
                (consumer (values))))))
            (body-matcher map-result.49) (map-result (map-result.49))
            (consumer (values))))))
        (body-matcher map-result.47) (map-result (map-result.47))
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
        (let ((+arg1.61 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3) (map ((+arg1.63 +arg1.61)) (+ +arg1.63 4))
            (body-matcher map-result.62) (map-result (map-result.62))
            (consumer (values)))))))
       (body-matcher map-result.57) (map-result (map-result.57))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.47 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.49 index-value.47))
         (index-let ((i.39 runtime-value index-value.49))
          (box (i.39)
           (#0
            (#0
             (loop-block (frame-shape i.39) (map () 5)
              (body-matcher map-result.53) (map-result (map-result.53))
              (consumer (values))))))))
        (body-matcher map-result.48) (map-result (map-result.48))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.39 runtime-value (frame 1 2 3)))
     (box ((shape @i.39))
      (#0
       (#0
        (loop-block (frame-shape @i.39) (map () 5) (body-matcher map-result.51)
         (map-result (map-result.51)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((reduce-arg.64 (frame 1 2 3))
      (+arg2.72
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.70) iota.70)
          (body-matcher map-result.69) (map-result (map-result.69))
          (consumer (values)))))))
     (let
      ((x.66
        (#1
         (loop-block (frame-shape 3)
          (map ((reduce-arg.65 reduce-arg.64)) (values reduce-arg.65))
          (body-matcher (reduce-arg.63)) (map-result ())
          (consumer
           (reduce (reduce-arg1.53 reduce-arg2.54 reduce-arg.63)
            (+ reduce-arg1.53 reduce-arg2.54)))))))
      (#0
       (#0
        (loop-block (frame-shape 1001)
         (map ((+arg2.74 +arg2.72)) (+ x.66 +arg2.74))
         (body-matcher map-result.73) (map-result (map-result.73))
         (consumer (values))))))) |}]
;;

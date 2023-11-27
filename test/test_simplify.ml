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
    (let ((+arg1.56 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.58 +arg1.56)) (+ +arg1.58 4))
        (body-matcher map-result.57) (map-result (map-result.57))
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
    (reduce{int | 4 [] []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (let ((reduce-arg.59 (frame 1 2 3 4 5)))
     (#1
      (loop-block (frame-shape 5)
       (map ((reduce-arg.60 reduce-arg.59)) (values reduce-arg.60))
       (body-matcher (reduce-arg.58)) (map-result ())
       (consumer
        (reduce (shape) (reduce-arg1.53 reduce-arg2.54 reduce-arg.58)
         (+ reduce-arg1.53 reduce-arg2.54)))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.60)
       (map-result (map-result.60)) (consumer (values))))) |}];
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
    (let ((x.86 (frame 5 6 7)) (+arg1.92 (frame 1 2 3)))
     (let
      ((y.95
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.94 +arg1.92)) (+ +arg1.94 4))
           (body-matcher map-result.93) (map-result (map-result.93))
           (consumer (values)))))))
      (let ((+arg1.101 y.95) (+arg2.102 y.95))
       (let
        ((+arg2.107
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.104 +arg1.101) (+arg2.105 +arg2.102))
              (+ +arg1.104 +arg2.105))
             (body-matcher map-result.103) (map-result (map-result.103))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.88 x.86))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.109 +arg2.107)) (+ x.88 +arg2.109))
               (body-matcher map-result.108) (map-result (map-result.108))
               (consumer (values))))))
           (body-matcher map-result.87) (map-result (map-result.87))
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
    (let ((x.76 (frame 5 6 7)) (+arg1.82 (frame 1 2 3)))
     (let
      ((+arg2.89
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.84 +arg1.82)) (+ +arg1.84 4))
           (body-matcher map-result.83) (map-result (map-result.83))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.78 x.76))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.91 +arg2.89)) (+ x.78 +arg2.91))
             (body-matcher map-result.90) (map-result (map-result.90))
             (consumer (values))))))
         (body-matcher map-result.77) (map-result (map-result.77))
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
    (let ((+arg1.87 (frame 1 2 3)))
     (let
      ((y.90
        (#0
         (#0
          (loop-block (frame-shape 3) (map ((+arg1.89 +arg1.87)) (+ +arg1.89 4))
           (body-matcher map-result.88) (map-result (map-result.88))
           (consumer (values)))))))
      (let ((+arg1.96 y.90) (+arg2.97 y.90))
       (let
        ((+arg2.102
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.99 +arg1.96) (+arg2.100 +arg2.97))
              (+ +arg1.99 +arg2.100))
             (body-matcher map-result.98) (map-result (map-result.98))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.104 +arg2.102)) (+ 5 +arg2.104))
           (body-matcher map-result.103) (map-result (map-result.103))
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
    (let ((x.86 (frame 5 6 7)) (+arg1.92 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.88 x.86))
         (let
          ((y.95
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.94 +arg1.92)) (+ +arg1.94 x.88))
               (body-matcher map-result.93) (map-result (map-result.93))
               (consumer (values)))))))
          (let ((+arg1.101 y.95) (+arg2.102 y.95))
           (let
            ((+arg2.107
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.104 +arg1.101) (+arg2.105 +arg2.102))
                  (+ +arg1.104 +arg2.105))
                 (body-matcher map-result.103) (map-result (map-result.103))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.109 +arg2.107)) (+ x.88 +arg2.109))
               (body-matcher map-result.108) (map-result (map-result.108))
               (consumer (values)))))))))
        (body-matcher map-result.87) (map-result (map-result.87))
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
    (let ((x.78 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.80 x.78)) (+ x.80 14))
        (body-matcher map-result.79) (map-result (map-result.79))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect{| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect{|
    (let ((+arg1.58 (frame 3 4)) (+arg2.59 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.61 +arg1.58) (+arg2.62 +arg2.59)) (+ +arg1.61 +arg2.62))
         (body-matcher map-result.60) (map-result (map-result.60))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect{| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect{|
    (let
     ((+arg1.78 (frame 1 2)) (+arg2.79 (frame 3 4)) (+arg1.86 (frame 1 2))
      (+arg2.87 (frame 3 4)) (+arg1.94 (frame 1 2)) (+arg2.95 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.81 +arg1.78) (+arg2.82 +arg2.79)) (+ +arg1.81 +arg2.82))
          (body-matcher map-result.80) (map-result (map-result.80))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.89 +arg1.86) (+arg2.90 +arg2.87)) (+ +arg1.89 +arg2.90))
          (body-matcher map-result.88) (map-result (map-result.88))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.97 +arg1.94) (+arg2.98 +arg2.95)) (+ +arg1.97 +arg2.98))
          (body-matcher map-result.96) (map-result (map-result.96))
          (consumer (values))))))
      (frame (frame 4 5) (frame 6 7) (frame 8 9)))) |}];
  checkAndPrint {| [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]] |};
  [%expect{|
    (frame (frame (frame 1 2) (frame 3 4) (frame 5 6))
     (frame (frame 7 8) (frame 9 10) (frame 11 12))) |}];
  checkAndPrint "(append{int | 3 2 []} [1 2 3] [4 5])";
  [%expect{| (frame 1 2 3 4 5) |}];
  checkAndPrint "(append{int | 3 2 [1]} [[1] [2] [3]] [[4] [5]])";
  [%expect{| (frame (frame 1) (frame 2) (frame 3) (frame 4) (frame 5)) |}];
  checkAndPrint "[[1 1] [2 2] (+ [2 2] 1)]";
  [%expect{|
    (let ((+arg1.56 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.58 +arg1.56)) (+ +arg1.58 1))
         (body-matcher map-result.57) (map-result (map-result.57))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect{|
    (let ((+arg1.63 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.65 +arg1.63)) (+ +arg1.65 1))
         (body-matcher map-result.64) (map-result (map-result.64))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect{|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.47)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.49 : iota.47))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.51 : iota.49)) iota.51)
               (body-matcher map-result.50) (map-result (map-result.50))
               (consumer (values))))))
           (body-matcher map-result.48) (map-result (map-result.48))
           (consumer (values))))))
       (body-matcher map-result.46) (map-result (map-result.46))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect{|
    (index
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.54)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.56 : iota.54))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.58 : iota.56))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.60 : iota.58))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.62 : iota.60)) iota.62)
                        (body-matcher map-result.61) (map-result (map-result.61))
                        (consumer (values))))))
                    (body-matcher map-result.59) (map-result (map-result.59))
                    (consumer (values))))))
                (body-matcher map-result.57) (map-result (map-result.57))
                (consumer (values))))))
            (body-matcher map-result.55) (map-result (map-result.55))
            (consumer (values))))))
        (body-matcher map-result.53) (map-result (map-result.53))
        (consumer (values)))))
     (frame 0 1 0)) |}];
  checkAndPrint
    {|
    (define (foo [x int])
      (+ [1 2 3] 4))
    (foo (array [0] int))
    |};
  [%expect{|
    (#0
     (#0
      (loop-block (frame-shape 0)
       (map ()
        (let ((+arg1.67 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3) (map ((+arg1.69 +arg1.67)) (+ +arg1.69 4))
            (body-matcher map-result.68) (map-result (map-result.68))
            (consumer (values)))))))
       (body-matcher map-result.63) (map-result (map-result.63))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect{|
    (let ((index-value.53 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.55 index-value.53))
         (index-let ((i.45 runtime-value index-value.55))
          (box (i.45)
           (#0
            (#0
             (loop-block (frame-shape i.45) (map () 5)
              (body-matcher map-result.59) (map-result (map-result.59))
              (consumer (values))))))))
        (body-matcher map-result.54) (map-result (map-result.54))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect{|
    (index-let ((@i.45 runtime-value (frame 1 2 3)))
     (box ((shape @i.45))
      (#0
       (#0
        (loop-block (frame-shape @i.45) (map () 5) (body-matcher map-result.57)
         (map-result (map-result.57)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect{|
    (let
     ((reduce-arg.70 (frame 1 2 3))
      (+arg2.78
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.76) iota.76)
          (body-matcher map-result.75) (map-result (map-result.75))
          (consumer (values)))))))
     (let
      ((x.72
        (#1
         (loop-block (frame-shape 3)
          (map ((reduce-arg.71 reduce-arg.70)) (values reduce-arg.71))
          (body-matcher (reduce-arg.69)) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.59 reduce-arg2.60 reduce-arg.69)
            (+ reduce-arg1.59 reduce-arg2.60)))))))
      (#0
       (#0
        (loop-block (frame-shape 1001)
         (map ((+arg2.80 +arg2.78)) (+ x.72 +arg2.80))
         (body-matcher map-result.79) (map-result (map-result.79))
         (consumer (values))))))) |}]
;;

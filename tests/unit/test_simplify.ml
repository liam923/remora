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
    (let ((+arg1.110 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.112 +arg1.110)) (+ +arg1.112 4))
        (body-matcher map-result.111) (map-result (map-result.111))
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
    (let ((arr.273 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.305
        (contiguous-subarray arr.273 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.306 reduce-arg.305)) (values reduce-arg.306))
        (body-matcher (reduce-arg.299)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.273 (frame 0) (shape 5) (shape))
          (reduce-arg1.235 reduce-arg2.238 reduce-arg.299)
          (+ reduce-arg1.235 reduce-arg2.238))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.115)
       (map-result (map-result.115)) (consumer (values))))) |}];
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
    (let ((x.155 (frame 5 6 7)) (+arg1.161 (frame 1 2 3)))
     (let
      ((y.164
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.163 +arg1.161)) (+ +arg1.163 4))
           (body-matcher map-result.162) (map-result (map-result.162))
           (consumer (values)))))))
      (let ((+arg1.170 y.164) (+arg2.171 y.164))
       (let
        ((+arg2.176
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.173 +arg1.170) (+arg2.174 +arg2.171))
              (+ +arg1.173 +arg2.174))
             (body-matcher map-result.172) (map-result (map-result.172))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.157 x.155))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.178 +arg2.176)) (+ x.157 +arg2.178))
               (body-matcher map-result.177) (map-result (map-result.177))
               (consumer (values))))))
           (body-matcher map-result.156) (map-result (map-result.156))
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
    (let ((x.140 (frame 5 6 7)) (+arg1.146 (frame 1 2 3)))
     (let
      ((+arg2.153
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.148 +arg1.146)) (+ +arg1.148 4))
           (body-matcher map-result.147) (map-result (map-result.147))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.142 x.140))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.155 +arg2.153)) (+ x.142 +arg2.155))
             (body-matcher map-result.154) (map-result (map-result.154))
             (consumer (values))))))
         (body-matcher map-result.141) (map-result (map-result.141))
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
    (let ((+arg1.156 (frame 1 2 3)))
     (let
      ((y.159
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.158 +arg1.156)) (+ +arg1.158 4))
           (body-matcher map-result.157) (map-result (map-result.157))
           (consumer (values)))))))
      (let ((+arg1.165 y.159) (+arg2.166 y.159))
       (let
        ((+arg2.171
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.168 +arg1.165) (+arg2.169 +arg2.166))
              (+ +arg1.168 +arg2.169))
             (body-matcher map-result.167) (map-result (map-result.167))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.173 +arg2.171)) (+ 5 +arg2.173))
           (body-matcher map-result.172) (map-result (map-result.172))
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
    (let ((x.155 (frame 5 6 7)) (+arg1.161 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.157 x.155))
         (let
          ((y.164
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.163 +arg1.161)) (+ +arg1.163 x.157))
               (body-matcher map-result.162) (map-result (map-result.162))
               (consumer (values)))))))
          (let ((+arg1.170 y.164) (+arg2.171 y.164))
           (let
            ((+arg2.176
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.173 +arg1.170) (+arg2.174 +arg2.171))
                  (+ +arg1.173 +arg2.174))
                 (body-matcher map-result.172) (map-result (map-result.172))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.178 +arg2.176)) (+ x.157 +arg2.178))
               (body-matcher map-result.177) (map-result (map-result.177))
               (consumer (values)))))))))
        (body-matcher map-result.156) (map-result (map-result.156))
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
    (let ((x.143 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.145 x.143)) (+ x.145 14))
        (body-matcher map-result.144) (map-result (map-result.144))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.112 (frame 3 4)) (+arg2.113 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.115 +arg1.112) (+arg2.116 +arg2.113))
          (+ +arg1.115 +arg2.116))
         (body-matcher map-result.114) (map-result (map-result.114))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.132 (frame 1 2)) (+arg2.133 (frame 3 4)) (+arg1.140 (frame 1 2))
      (+arg2.141 (frame 3 4)) (+arg1.148 (frame 1 2)) (+arg2.149 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.135 +arg1.132) (+arg2.136 +arg2.133))
           (+ +arg1.135 +arg2.136))
          (body-matcher map-result.134) (map-result (map-result.134))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.143 +arg1.140) (+arg2.144 +arg2.141))
           (+ +arg1.143 +arg2.144))
          (body-matcher map-result.142) (map-result (map-result.142))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.151 +arg1.148) (+arg2.152 +arg2.149))
           (+ +arg1.151 +arg2.152))
          (body-matcher map-result.150) (map-result (map-result.150))
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
    (let ((+arg1.110 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.112 +arg1.110)) (+ +arg1.112 1))
         (body-matcher map-result.111) (map-result (map-result.111))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.117 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.119 +arg1.117)) (+ +arg1.119 1))
         (body-matcher map-result.118) (map-result (map-result.118))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.101)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.103 : iota.101))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.105 : iota.103)) iota.105)
               (body-matcher map-result.104) (map-result (map-result.104))
               (consumer (values))))))
           (body-matcher map-result.102) (map-result (map-result.102))
           (consumer (values))))))
       (body-matcher map-result.100) (map-result (map-result.100))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.120)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.122 : iota.120))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.124 : iota.122))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.126 : iota.124))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.128 : iota.126)) iota.128)
                        (body-matcher map-result.127)
                        (map-result (map-result.127)) (consumer (values))))))
                    (body-matcher map-result.125) (map-result (map-result.125))
                    (consumer (values))))))
                (body-matcher map-result.123) (map-result (map-result.123))
                (consumer (values))))))
            (body-matcher map-result.121) (map-result (map-result.121))
            (consumer (values))))))
        (body-matcher map-result.119) (map-result (map-result.119))
        (consumer (values)))))
     (frame 0 1 0) (shape 1 2 3) (shape)) |}];
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
        (let ((+arg1.126 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.128 +arg1.126)) (+ +arg1.128 4))
            (body-matcher map-result.127) (map-result (map-result.127))
            (consumer (values)))))))
       (body-matcher map-result.122) (map-result (map-result.122))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.125 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.127 index-value.125))
         (index-let ((i.99 runtime-value index-value.127))
          (box (i.99)
           (#0
            (#0
             (loop-block (frame-shape i.99) (map () 5)
              (body-matcher map-result.133) (map-result (map-result.133))
              (consumer (values))))))))
        (body-matcher map-result.126) (map-result (map-result.126))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.99 runtime-value (frame 1 2 3)))
     (box ((shape @i.99))
      (#0
       (#0
        (loop-block (frame-shape @i.99) (map () 5) (body-matcher map-result.131)
         (map-result (map-result.131)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.284 (frame 1 2 3))
      (+arg2.324
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.322) iota.322)
          (body-matcher map-result.321) (map-result (map-result.321))
          (consumer (values)))))))
     (let
      ((reduce-arg.316
        (contiguous-subarray arr.284 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.318
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.317 reduce-arg.316)) (values reduce-arg.317))
           (body-matcher (reduce-arg.310)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.284 (frame 0) (shape 3) (shape))
             (reduce-arg1.241 reduce-arg2.244 reduce-arg.310)
             (+ reduce-arg1.241 reduce-arg2.244)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.326 +arg2.324)) (+ x.318 +arg2.326))
          (body-matcher map-result.325) (map-result (map-result.325))
          (consumer (values)))))))) |}]
;;

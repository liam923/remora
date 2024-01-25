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
    (let ((+arg1.134 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.136 +arg1.134)) (+ +arg1.136 4))
        (body-matcher map-result.135) (map-result (map-result.135))
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
    (let ((arr.297 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.329
        (contiguous-subarray arr.297 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.330 reduce-arg.329)) (values reduce-arg.330))
        (body-matcher (reduce-arg.323)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.297 (frame 0) (shape 5) (shape))
          (reduce-arg1.259 reduce-arg2.262 reduce-arg.323)
          (+ reduce-arg1.259 reduce-arg2.262))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.139)
       (map-result (map-result.139)) (consumer (values))))) |}];
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
    (let ((x.179 (frame 5 6 7)) (+arg1.185 (frame 1 2 3)))
     (let
      ((y.188
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.187 +arg1.185)) (+ +arg1.187 4))
           (body-matcher map-result.186) (map-result (map-result.186))
           (consumer (values)))))))
      (let ((+arg1.194 y.188) (+arg2.195 y.188))
       (let
        ((+arg2.200
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.197 +arg1.194) (+arg2.198 +arg2.195))
              (+ +arg1.197 +arg2.198))
             (body-matcher map-result.196) (map-result (map-result.196))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.181 x.179))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.202 +arg2.200)) (+ x.181 +arg2.202))
               (body-matcher map-result.201) (map-result (map-result.201))
               (consumer (values))))))
           (body-matcher map-result.180) (map-result (map-result.180))
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
    (let ((x.164 (frame 5 6 7)) (+arg1.170 (frame 1 2 3)))
     (let
      ((+arg2.177
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.172 +arg1.170)) (+ +arg1.172 4))
           (body-matcher map-result.171) (map-result (map-result.171))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.166 x.164))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.179 +arg2.177)) (+ x.166 +arg2.179))
             (body-matcher map-result.178) (map-result (map-result.178))
             (consumer (values))))))
         (body-matcher map-result.165) (map-result (map-result.165))
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
    (let ((+arg1.180 (frame 1 2 3)))
     (let
      ((y.183
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.182 +arg1.180)) (+ +arg1.182 4))
           (body-matcher map-result.181) (map-result (map-result.181))
           (consumer (values)))))))
      (let ((+arg1.189 y.183) (+arg2.190 y.183))
       (let
        ((+arg2.195
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.192 +arg1.189) (+arg2.193 +arg2.190))
              (+ +arg1.192 +arg2.193))
             (body-matcher map-result.191) (map-result (map-result.191))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.197 +arg2.195)) (+ 5 +arg2.197))
           (body-matcher map-result.196) (map-result (map-result.196))
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
    (let ((x.179 (frame 5 6 7)) (+arg1.185 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.181 x.179))
         (let
          ((y.188
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.187 +arg1.185)) (+ +arg1.187 x.181))
               (body-matcher map-result.186) (map-result (map-result.186))
               (consumer (values)))))))
          (let ((+arg1.194 y.188) (+arg2.195 y.188))
           (let
            ((+arg2.200
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.197 +arg1.194) (+arg2.198 +arg2.195))
                  (+ +arg1.197 +arg2.198))
                 (body-matcher map-result.196) (map-result (map-result.196))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.202 +arg2.200)) (+ x.181 +arg2.202))
               (body-matcher map-result.201) (map-result (map-result.201))
               (consumer (values)))))))))
        (body-matcher map-result.180) (map-result (map-result.180))
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
    (let ((x.167 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.169 x.167)) (+ x.169 14))
        (body-matcher map-result.168) (map-result (map-result.168))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.136 (frame 3 4)) (+arg2.137 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.139 +arg1.136) (+arg2.140 +arg2.137))
          (+ +arg1.139 +arg2.140))
         (body-matcher map-result.138) (map-result (map-result.138))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.156 (frame 1 2)) (+arg2.157 (frame 3 4)) (+arg1.164 (frame 1 2))
      (+arg2.165 (frame 3 4)) (+arg1.172 (frame 1 2)) (+arg2.173 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.159 +arg1.156) (+arg2.160 +arg2.157))
           (+ +arg1.159 +arg2.160))
          (body-matcher map-result.158) (map-result (map-result.158))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.167 +arg1.164) (+arg2.168 +arg2.165))
           (+ +arg1.167 +arg2.168))
          (body-matcher map-result.166) (map-result (map-result.166))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.175 +arg1.172) (+arg2.176 +arg2.173))
           (+ +arg1.175 +arg2.176))
          (body-matcher map-result.174) (map-result (map-result.174))
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
    (let ((+arg1.134 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.136 +arg1.134)) (+ +arg1.136 1))
         (body-matcher map-result.135) (map-result (map-result.135))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.141 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.143 +arg1.141)) (+ +arg1.143 1))
         (body-matcher map-result.142) (map-result (map-result.142))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.125)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.127 : iota.125))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.129 : iota.127)) iota.129)
               (body-matcher map-result.128) (map-result (map-result.128))
               (consumer (values))))))
           (body-matcher map-result.126) (map-result (map-result.126))
           (consumer (values))))))
       (body-matcher map-result.124) (map-result (map-result.124))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.144)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.146 : iota.144))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.148 : iota.146))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.150 : iota.148))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.152 : iota.150)) iota.152)
                        (body-matcher map-result.151)
                        (map-result (map-result.151)) (consumer (values))))))
                    (body-matcher map-result.149) (map-result (map-result.149))
                    (consumer (values))))))
                (body-matcher map-result.147) (map-result (map-result.147))
                (consumer (values))))))
            (body-matcher map-result.145) (map-result (map-result.145))
            (consumer (values))))))
        (body-matcher map-result.143) (map-result (map-result.143))
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
        (let ((+arg1.150 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.152 +arg1.150)) (+ +arg1.152 4))
            (body-matcher map-result.151) (map-result (map-result.151))
            (consumer (values)))))))
       (body-matcher map-result.146) (map-result (map-result.146))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.149 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.151 index-value.149))
         (index-let ((i.123 runtime-value index-value.151))
          (box (i.123)
           (#0
            (#0
             (loop-block (frame-shape i.123) (map () 5)
              (body-matcher map-result.157) (map-result (map-result.157))
              (consumer (values))))))))
        (body-matcher map-result.150) (map-result (map-result.150))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.123 runtime-value (frame 1 2 3)))
     (box ((shape @i.123))
      (#0
       (#0
        (loop-block (frame-shape @i.123) (map () 5) (body-matcher map-result.155)
         (map-result (map-result.155)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.308 (frame 1 2 3))
      (+arg2.348
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.346) iota.346)
          (body-matcher map-result.345) (map-result (map-result.345))
          (consumer (values)))))))
     (let
      ((reduce-arg.340
        (contiguous-subarray arr.308 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.342
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.341 reduce-arg.340)) (values reduce-arg.341))
           (body-matcher (reduce-arg.334)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.308 (frame 0) (shape 3) (shape))
             (reduce-arg1.265 reduce-arg2.268 reduce-arg.334)
             (+ reduce-arg1.265 reduce-arg2.268)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.350 +arg2.348)) (+ x.342 +arg2.350))
          (body-matcher map-result.349) (map-result (map-result.349))
          (consumer (values)))))))) |}]
;;

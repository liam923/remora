open! Base
open Remora

let%expect_test "check fusing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module FuseAndSimplify.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let fuseAndPrint = TestPipeline.runAndPrint pipeline in
  fuseAndPrint "(+ [1 2 3] (+ [4 5 6] [7 8 9]))";
  [%expect
    {|
    (let
     ((+arg1.98 (frame 4 5 6)) (+arg2.100 (frame 7 8 9))
      (+arg1.102 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.67 +arg1.98) (+arg2.68 +arg2.100) (+arg1.73 +arg1.102))
         (+ +arg1.73 (+ +arg1.67 +arg2.68)))
        (body-matcher map-result.72) (map-result (map-result.72))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.157 (frame 1 2 3)) (+arg2.159 (frame 7 8 9))
      (+arg2.161 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.84 +arg2.157) (+arg2.103 +arg2.159) (+arg2.94 +arg2.161))
         (let ((fusion-target-map-result.146 (+ 1 +arg2.84)))
          (+ (+ fusion-target-map-result.146 +arg2.94)
           (+ fusion-target-map-result.146 +arg2.103))))
        (body-matcher map-result.107) (map-result (map-result.107))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.89 (frame 1 2 3)))
     (#1
      (loop-block (frame-shape 3) (map ((+arg2.64 +arg2.89)) (+ 1 +arg2.64))
       (body-matcher reduce-arg.66) (map-result ())
       (consumer
        (reduce (reduce-arg1.55 reduce-arg2.56 reduce-arg.66)
         (+ reduce-arg1.55 reduce-arg2.56)))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.185 (frame 1 2 3)))
     (let
      ((consumer-result.186
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.96 +arg2.185))
           (let ((fusion-target-map-result.169 (+ 4 +arg2.96)))
            (values (+ 5 fusion-target-map-result.169)
             (+ 6 fusion-target-map-result.169))))
          (body-matcher (reduce-arg.108 reduce-arg.121)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.161 fused-reduce-arg2.162
             (reduce-arg.108 reduce-arg.121))
            (values
             (+ (#0 (unzip fused-reduce-arg1.161))
              (#0 (unzip fused-reduce-arg2.162)))
             (+ (#1 (unzip fused-reduce-arg1.161))
              (#1 (unzip fused-reduce-arg2.162))))))))))
      (+ (#0 consumer-result.186) (#1 consumer-result.186)))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (define (add-3 [a [int 3]] [b [int 3]])
        (+ a b))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 [3]} add-3 (+ [[6 7 8] [6 7 8] [6 7 8]] x)))
    |};
  [%expect
    {|
    (let
     ((+arg2.259 (frame 1 2 3))
      (+arg1.261 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((consumer-result.262
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.118 +arg2.259) (+arg1.144 +arg1.261))
           (let
            ((fusion-target-map-result.227 (+ 4 +arg2.118))
             (+arg1.254 +arg1.144))
            (values (+ 5 fusion-target-map-result.227)
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.147 +arg1.254))
                 (+ +arg1.147 fusion-target-map-result.227))
                (body-matcher map-result.146) (map-result (map-result.146))
                (consumer (values))))))))
          (body-matcher (reduce-arg.131 reduce-arg.149)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.219 fused-reduce-arg2.220
             (reduce-arg.131 reduce-arg.149))
            (let
             ((+arg1.242 (#1 (unzip fused-reduce-arg1.219)))
              (+arg2.244 (#1 (unzip fused-reduce-arg2.220))))
             (values
              (+ (#0 (unzip fused-reduce-arg1.219))
               (#0 (unzip fused-reduce-arg2.220)))
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.156 +arg1.242) (+arg2.157 +arg2.244))
                  (+ +arg1.156 +arg2.157))
                 (body-matcher map-result.155) (map-result (map-result.155))
                 (consumer (values)))))))))))))
      (let ((+arg2.234 (#1 consumer-result.262)))
       (#0
        (#0
         (loop-block (frame-shape 3)
          (map ((+arg2.163 +arg2.234)) (+ (#0 consumer-result.262) +arg2.163))
          (body-matcher map-result.162) (map-result (map-result.162))
          (consumer (values)))))))) |}]
;;

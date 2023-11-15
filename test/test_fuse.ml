open! Base
open Remora

let%expect_test "check fusing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module FuseAndSimplify.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let fuseAndPrint = TestPipeline.runAndPrint pipeline in
  fuseAndPrint "(+ [1 2 3] (+ [4 5 6] [7 8 9]))";
  [%expect
    {|
    (#0
     (#0
      (let
       ((+arg1.98 (frame 4 5 6)) (+arg2.100 (frame 7 8 9))
        (+arg1.102 (frame 1 2 3)))
       (loop-block (frame-shape 3)
        (map ((+arg1.69 +arg1.98) (+arg2.70 +arg2.100) (+arg1.73 +arg1.102))
         (+ +arg1.73 (+ +arg1.69 +arg2.70)))
        (body-matcher map-result.72) (map-result (map-result.72))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (#0
     (#0
      (let
       ((+arg2.151 (frame 1 2 3)) (+arg2.153 (frame 7 8 9))
        (+arg2.155 (frame 4 5 6)))
       (loop-block (frame-shape 3)
        (map ((+arg2.87 +arg2.151) (+arg2.99 +arg2.153) (+arg2.93 +arg2.155))
         (let ((fusion-target-map-result.140 (+ 1 +arg2.87)))
          (+ (+ fusion-target-map-result.140 +arg2.93)
           (+ fusion-target-map-result.140 +arg2.99))))
        (body-matcher map-result.101) (map-result (map-result.101))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (#1
     (let ((+arg2.89 (frame 1 2 3)))
      (loop-block (frame-shape 3) (map ((+arg2.66 +arg2.89)) (+ 1 +arg2.66))
       (body-matcher reduce-arg.63) (map-result ())
       (consumer
        (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.63)
         (+ reduce-arg1.61 reduce-arg2.62)))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let
     ((fused-block-result.157
       (#1
        (let ((+arg2.175 (frame 1 2 3)))
         (loop-block (frame-shape 3)
          (map ((+arg2.99 +arg2.175))
           (let
            ((fusion-target-map-result.155
              (let ((fusion-target-map-result.150 (+ 4 +arg2.99)))
               (values fusion-target-map-result.150
                (+ 5 fusion-target-map-result.150)))))
            (values (#1 fusion-target-map-result.155)
             (+ 6 (#0 fusion-target-map-result.155)))))
          (body-matcher (reduce-arg.101 reduce-arg.107)) (map-result ())
          (consumer
           (reduce (shape)
            (fused-reduce-arg1.159 fused-reduce-arg2.160
             (reduce-arg.101 reduce-arg.107))
            (values
             (+ (#0 (unzip fused-reduce-arg1.159))
              (#0 (unzip fused-reduce-arg2.160)))
             (+ (#1 (unzip fused-reduce-arg1.159))
              (#1 (unzip fused-reduce-arg2.160)))))))))))
     (+ (#0 fused-block-result.157) (#1 fused-block-result.157))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [3] []} + (+ [[6 7 8] [6 7 8] [6 7 8]] x)))
    |};
  [%expect
    {|
    (let
     ((fused-block-result.180
       (let
        ((loop-block-result.210
          (let ((+arg2.209 (frame 1 2 3)))
           (loop-block (frame-shape 3)
            (map ((+arg2.106 +arg2.209))
             (let ((fusion-target-map-result.178 (+ 4 +arg2.106)))
              (values fusion-target-map-result.178
               (+ 5 fusion-target-map-result.178))))
            (body-matcher (map-result.105 reduce-arg.108))
            (map-result (map-result.105))
            (consumer
             (reduce (shape) (reduce-arg1.85 reduce-arg2.86 reduce-arg.108)
              (+ reduce-arg1.85 reduce-arg2.86)))))))
        (values (#0 (#0 loop-block-result.210)) (#1 loop-block-result.210)))))
     (#0
      (#0
       (let
        ((+arg2.185
          (#1
           (let
            ((+arg2.197 (#0 fused-block-result.180))
             (+arg1.199 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
            (loop-block (frame-shape 3)
             (map ((+arg2.119 +arg2.197) (+arg1.120 +arg1.199))
              (#0
               (#0
                (let ((+arg1.193 +arg1.120))
                 (loop-block (frame-shape 3)
                  (map ((+arg1.123 +arg1.193)) (+ +arg1.123 +arg2.119))
                  (body-matcher map-result.122) (map-result (map-result.122))
                  (consumer (values)))))))
             (body-matcher reduce-arg.115) (map-result ())
             (consumer
              (reduce (shape 3) (reduce-arg1.99 reduce-arg2.100 reduce-arg.115)
               (+ reduce-arg1.99 reduce-arg2.100))))))))
        (loop-block (frame-shape 3)
         (map ((+arg2.128 +arg2.185)) (+ (#1 fused-block-result.180) +arg2.128))
         (body-matcher map-result.127) (map-result (map-result.127))
         (consumer (values))))))) |}]
;;

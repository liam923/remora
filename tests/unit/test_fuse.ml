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
     ((+arg1.121 (frame 4 5 6)) (+arg2.123 (frame 7 8 9))
      (+arg1.125 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.90 +arg1.121) (+arg2.91 +arg2.123) (+arg1.96 +arg1.125))
         (+ +arg1.96 (+ +arg1.90 +arg2.91)))
        (body-matcher map-result.95) (map-result (map-result.95))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.180 (frame 1 2 3)) (+arg2.182 (frame 7 8 9))
      (+arg2.184 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.107 +arg2.180) (+arg2.126 +arg2.182) (+arg2.117 +arg2.184))
         (let ((fusion-target-map-result.169 (+ 1 +arg2.107)))
          (+ (+ fusion-target-map-result.169 +arg2.117)
           (+ fusion-target-map-result.169 +arg2.126))))
        (body-matcher map-result.130) (map-result (map-result.130))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.112 (frame 1 2 3)))
     (#1
      (loop-block (frame-shape 3) (map ((+arg2.87 +arg2.112)) (+ 1 +arg2.87))
       (body-matcher reduce-arg.89) (map-result ())
       (consumer
        (reduce (reduce-arg1.78 reduce-arg2.79 reduce-arg.89)
         (+ reduce-arg1.78 reduce-arg2.79)))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.208 (frame 1 2 3)))
     (let
      ((consumer-result.209
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.119 +arg2.208))
           (let ((fusion-target-map-result.192 (+ 4 +arg2.119)))
            (values (+ 5 fusion-target-map-result.192)
             (+ 6 fusion-target-map-result.192))))
          (body-matcher (reduce-arg.131 reduce-arg.144)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.184 fused-reduce-arg2.185
             (reduce-arg.131 reduce-arg.144))
            (values (+ (#0 fused-reduce-arg1.184) (#0 fused-reduce-arg2.185))
             (+ (#1 fused-reduce-arg1.184) (#1 fused-reduce-arg2.185)))))))))
      (+ (#0 consumer-result.209) (#1 consumer-result.209)))) |}];
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
     ((+arg2.282 (frame 1 2 3))
      (+arg1.284 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((consumer-result.285
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.141 +arg2.282) (+arg1.167 +arg1.284))
           (let
            ((fusion-target-map-result.250 (+ 4 +arg2.141))
             (+arg1.277 +arg1.167))
            (values (+ 5 fusion-target-map-result.250)
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.170 +arg1.277))
                 (+ +arg1.170 fusion-target-map-result.250))
                (body-matcher map-result.169) (map-result (map-result.169))
                (consumer (values))))))))
          (body-matcher (reduce-arg.154 reduce-arg.172)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.242 fused-reduce-arg2.243
             (reduce-arg.154 reduce-arg.172))
            (let
             ((+arg1.265 (#1 fused-reduce-arg1.242))
              (+arg2.267 (#1 fused-reduce-arg2.243)))
             (values (+ (#0 fused-reduce-arg1.242) (#0 fused-reduce-arg2.243))
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.179 +arg1.265) (+arg2.180 +arg2.267))
                  (+ +arg1.179 +arg2.180))
                 (body-matcher map-result.178) (map-result (map-result.178))
                 (consumer (values)))))))))))))
      (let ((+arg2.257 (#1 consumer-result.285)))
       (#0
        (#0
         (loop-block (frame-shape 3)
          (map ((+arg2.186 +arg2.257)) (+ (#0 consumer-result.285) +arg2.186))
          (body-matcher map-result.185) (map-result (map-result.185))
          (consumer (values)))))))) |}]
;;

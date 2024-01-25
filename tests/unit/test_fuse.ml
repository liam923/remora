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
     ((+arg1.191 (frame 4 5 6)) (+arg2.193 (frame 7 8 9))
      (+arg1.195 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.160 +arg1.191) (+arg2.161 +arg2.193) (+arg1.166 +arg1.195))
         (+ +arg1.166 (+ +arg1.160 +arg2.161)))
        (body-matcher map-result.165) (map-result (map-result.165))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.250 (frame 1 2 3)) (+arg2.252 (frame 7 8 9))
      (+arg2.254 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.177 +arg2.250) (+arg2.196 +arg2.252) (+arg2.187 +arg2.254))
         (let ((fusion-target-map-result.239 (+ 1 +arg2.177)))
          (+ (+ fusion-target-map-result.239 +arg2.187)
           (+ fusion-target-map-result.239 +arg2.196))))
        (body-matcher map-result.200) (map-result (map-result.200))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.317 (frame 1 2 3)))
     (let
      ((arr.320
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.319 +arg2.317)) (+ 1 +arg2.319))
           (body-matcher map-result.318) (map-result (map-result.318))
           (consumer (values)))))))
      (let
       ((reduce-arg.352
         (contiguous-subarray arr.320 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.353 reduce-arg.352)) (values reduce-arg.353))
         (body-matcher (reduce-arg.346)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.320 (frame 0) (shape 3) (shape))
           (reduce-arg1.276 reduce-arg2.279 reduce-arg.346)
           (+ reduce-arg1.276 reduce-arg2.279)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.511 (frame 1 2 3)))
     (let
      ((fused-block-result.638
        (loop-block (frame-shape 3)
         (map ((+arg2.513 +arg2.511))
          (let ((fusion-target-map-result.633 (+ 4 +arg2.513)))
           (let
            ((fusion-target-map-result.637
              (values fusion-target-map-result.633
               (+ 5 fusion-target-map-result.633))))
            (values fusion-target-map-result.637
             (+ 6 (#0 fusion-target-map-result.637))))))
         (body-matcher ((map-result.512 map-result.522) map-result.565))
         (map-result (map-result.512 map-result.522 map-result.565))
         (consumer (values)))))
      (let
       ((reduce-arg.599
         (contiguous-subarray (#2 (#0 fused-block-result.638)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.556
         (contiguous-subarray (#1 (#0 fused-block-result.638)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.557 reduce-arg.556)) (values reduce-arg.557))
          (body-matcher (reduce-arg.550)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.638)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.327 reduce-arg2.330 reduce-arg.550)
            (+ reduce-arg1.327 reduce-arg2.330)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.600 reduce-arg.599)) (values reduce-arg.600))
          (body-matcher (reduce-arg.593)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.638)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.471 reduce-arg2.474 reduce-arg.593)
            (+ reduce-arg1.471 reduce-arg2.474))))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (define (add-3 [a [int 3]] [b [int 3]])
        (+ a b))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] [3]} add-3 (+ [[6 7 8] [6 7 8] [6 7 8]] x)))
    |};
  [%expect
    {|
    (let
     ((+arg2.539 (frame 1 2 3))
      (+arg1.594 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.702
        (loop-block (frame-shape 3)
         (map ((+arg2.541 +arg2.539) (+arg1.597 +arg1.594))
          (let
           ((fusion-target-map-result.697 (+ 4 +arg2.541)) (+arg1.598 +arg1.597))
           (let
            ((fusion-target-map-result.701
              (values fusion-target-map-result.697
               (+ 5 fusion-target-map-result.697))))
            (values fusion-target-map-result.701
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.600 +arg1.598))
                 (+ +arg1.600 (#0 fusion-target-map-result.701)))
                (body-matcher map-result.599) (map-result (map-result.599))
                (consumer (values)))))))))
         (body-matcher ((map-result.540 map-result.551) map-result.595))
         (map-result (map-result.540 map-result.551 map-result.595))
         (consumer (values)))))
      (let
       ((reduce-arg.641
         (contiguous-subarray (#2 (#0 fused-block-result.702)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.585
         (contiguous-subarray (#1 (#0 fused-block-result.702)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.644
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.642 reduce-arg.641)) (values reduce-arg.642))
            (body-matcher (reduce-arg.627)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.702)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.494 reduce-arg2.499 reduce-arg.627)
              (let ((+arg1.636 reduce-arg1.494) (+arg2.637 reduce-arg2.499))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.639 +arg1.636) (+arg2.640 +arg2.637))
                   (+ +arg1.639 +arg2.640))
                  (body-matcher map-result.638) (map-result (map-result.638))
                  (consumer (values)))))))))))
         (+arg1.587
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.586 reduce-arg.585)) (values reduce-arg.586))
            (body-matcher (reduce-arg.579)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.702)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.338 reduce-arg2.341 reduce-arg.579)
              (+ reduce-arg1.338 reduce-arg2.341)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.646 +arg2.644)) (+ +arg1.587 +arg2.646))
           (body-matcher map-result.645) (map-result (map-result.645))
           (consumer (values))))))))) |}]
;;

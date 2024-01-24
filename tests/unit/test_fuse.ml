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
     ((+arg1.158 (frame 4 5 6)) (+arg2.160 (frame 7 8 9))
      (+arg1.162 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.127 +arg1.158) (+arg2.128 +arg2.160) (+arg1.133 +arg1.162))
         (+ +arg1.133 (+ +arg1.127 +arg2.128)))
        (body-matcher map-result.132) (map-result (map-result.132))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.217 (frame 1 2 3)) (+arg2.219 (frame 7 8 9))
      (+arg2.221 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.144 +arg2.217) (+arg2.163 +arg2.219) (+arg2.154 +arg2.221))
         (let ((fusion-target-map-result.206 (+ 1 +arg2.144)))
          (+ (+ fusion-target-map-result.206 +arg2.154)
           (+ fusion-target-map-result.206 +arg2.163))))
        (body-matcher map-result.167) (map-result (map-result.167))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.284 (frame 1 2 3)))
     (let
      ((arr.287
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.286 +arg2.284)) (+ 1 +arg2.286))
           (body-matcher map-result.285) (map-result (map-result.285))
           (consumer (values)))))))
      (let
       ((reduce-arg.319
         (contiguous-subarray arr.287 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.320 reduce-arg.319)) (values reduce-arg.320))
         (body-matcher (reduce-arg.313)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.287 (frame 0) (shape 3) (shape))
           (reduce-arg1.243 reduce-arg2.246 reduce-arg.313)
           (+ reduce-arg1.243 reduce-arg2.246)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.478 (frame 1 2 3)))
     (let
      ((fused-block-result.605
        (loop-block (frame-shape 3)
         (map ((+arg2.480 +arg2.478))
          (let ((fusion-target-map-result.600 (+ 4 +arg2.480)))
           (let
            ((fusion-target-map-result.604
              (values fusion-target-map-result.600
               (+ 5 fusion-target-map-result.600))))
            (values fusion-target-map-result.604
             (+ 6 (#0 fusion-target-map-result.604))))))
         (body-matcher ((map-result.479 map-result.489) map-result.532))
         (map-result (map-result.479 map-result.489 map-result.532))
         (consumer (values)))))
      (let
       ((reduce-arg.566
         (contiguous-subarray (#2 (#0 fused-block-result.605)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.523
         (contiguous-subarray (#1 (#0 fused-block-result.605)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.524 reduce-arg.523)) (values reduce-arg.524))
          (body-matcher (reduce-arg.517)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.605)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.294 reduce-arg2.297 reduce-arg.517)
            (+ reduce-arg1.294 reduce-arg2.297)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.567 reduce-arg.566)) (values reduce-arg.567))
          (body-matcher (reduce-arg.560)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.605)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.438 reduce-arg2.441 reduce-arg.560)
            (+ reduce-arg1.438 reduce-arg2.441))))))))) |}];
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
     ((+arg2.506 (frame 1 2 3))
      (+arg1.561 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.669
        (loop-block (frame-shape 3)
         (map ((+arg2.508 +arg2.506) (+arg1.564 +arg1.561))
          (let
           ((fusion-target-map-result.664 (+ 4 +arg2.508)) (+arg1.565 +arg1.564))
           (let
            ((fusion-target-map-result.668
              (values fusion-target-map-result.664
               (+ 5 fusion-target-map-result.664))))
            (values fusion-target-map-result.668
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.567 +arg1.565))
                 (+ +arg1.567 (#0 fusion-target-map-result.668)))
                (body-matcher map-result.566) (map-result (map-result.566))
                (consumer (values)))))))))
         (body-matcher ((map-result.507 map-result.518) map-result.562))
         (map-result (map-result.507 map-result.518 map-result.562))
         (consumer (values)))))
      (let
       ((reduce-arg.608
         (contiguous-subarray (#2 (#0 fused-block-result.669)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.552
         (contiguous-subarray (#1 (#0 fused-block-result.669)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.611
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.609 reduce-arg.608)) (values reduce-arg.609))
            (body-matcher (reduce-arg.594)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.669)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.461 reduce-arg2.466 reduce-arg.594)
              (let ((+arg1.603 reduce-arg1.461) (+arg2.604 reduce-arg2.466))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.606 +arg1.603) (+arg2.607 +arg2.604))
                   (+ +arg1.606 +arg2.607))
                  (body-matcher map-result.605) (map-result (map-result.605))
                  (consumer (values)))))))))))
         (+arg1.554
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.553 reduce-arg.552)) (values reduce-arg.553))
            (body-matcher (reduce-arg.546)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.669)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.305 reduce-arg2.308 reduce-arg.546)
              (+ reduce-arg1.305 reduce-arg2.308)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.613 +arg2.611)) (+ +arg1.554 +arg2.613))
           (body-matcher map-result.612) (map-result (map-result.612))
           (consumer (values))))))))) |}]
;;

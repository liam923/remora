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
     ((+arg1.170 (frame 4 5 6)) (+arg2.172 (frame 7 8 9))
      (+arg1.174 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.139 +arg1.170) (+arg2.140 +arg2.172) (+arg1.145 +arg1.174))
         (+ +arg1.145 (+ +arg1.139 +arg2.140)))
        (body-matcher map-result.144) (map-result (map-result.144))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.229 (frame 1 2 3)) (+arg2.231 (frame 7 8 9))
      (+arg2.233 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.156 +arg2.229) (+arg2.175 +arg2.231) (+arg2.166 +arg2.233))
         (let ((fusion-target-map-result.218 (+ 1 +arg2.156)))
          (+ (+ fusion-target-map-result.218 +arg2.166)
           (+ fusion-target-map-result.218 +arg2.175))))
        (body-matcher map-result.179) (map-result (map-result.179))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.296 (frame 1 2 3)))
     (let
      ((arr.299
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.298 +arg2.296)) (+ 1 +arg2.298))
           (body-matcher map-result.297) (map-result (map-result.297))
           (consumer (values)))))))
      (let
       ((reduce-arg.331
         (contiguous-subarray arr.299 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.332 reduce-arg.331)) (values reduce-arg.332))
         (body-matcher (reduce-arg.325)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.299 (frame 0) (shape 3) (shape))
           (reduce-arg1.255 reduce-arg2.258 reduce-arg.325)
           (+ reduce-arg1.255 reduce-arg2.258)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.490 (frame 1 2 3)))
     (let
      ((fused-block-result.617
        (loop-block (frame-shape 3)
         (map ((+arg2.492 +arg2.490))
          (let ((fusion-target-map-result.612 (+ 4 +arg2.492)))
           (let
            ((fusion-target-map-result.616
              (values fusion-target-map-result.612
               (+ 5 fusion-target-map-result.612))))
            (values fusion-target-map-result.616
             (+ 6 (#0 fusion-target-map-result.616))))))
         (body-matcher ((map-result.491 map-result.501) map-result.544))
         (map-result (map-result.491 map-result.501 map-result.544))
         (consumer (values)))))
      (let
       ((reduce-arg.578
         (contiguous-subarray (#2 (#0 fused-block-result.617)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.535
         (contiguous-subarray (#1 (#0 fused-block-result.617)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.536 reduce-arg.535)) (values reduce-arg.536))
          (body-matcher (reduce-arg.529)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.617)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.306 reduce-arg2.309 reduce-arg.529)
            (+ reduce-arg1.306 reduce-arg2.309)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.579 reduce-arg.578)) (values reduce-arg.579))
          (body-matcher (reduce-arg.572)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.617)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.450 reduce-arg2.453 reduce-arg.572)
            (+ reduce-arg1.450 reduce-arg2.453))))))))) |}];
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
     ((+arg2.518 (frame 1 2 3))
      (+arg1.573 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.681
        (loop-block (frame-shape 3)
         (map ((+arg2.520 +arg2.518) (+arg1.576 +arg1.573))
          (let
           ((fusion-target-map-result.676 (+ 4 +arg2.520)) (+arg1.577 +arg1.576))
           (let
            ((fusion-target-map-result.680
              (values fusion-target-map-result.676
               (+ 5 fusion-target-map-result.676))))
            (values fusion-target-map-result.680
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.579 +arg1.577))
                 (+ +arg1.579 (#0 fusion-target-map-result.680)))
                (body-matcher map-result.578) (map-result (map-result.578))
                (consumer (values)))))))))
         (body-matcher ((map-result.519 map-result.530) map-result.574))
         (map-result (map-result.519 map-result.530 map-result.574))
         (consumer (values)))))
      (let
       ((reduce-arg.620
         (contiguous-subarray (#2 (#0 fused-block-result.681)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.564
         (contiguous-subarray (#1 (#0 fused-block-result.681)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.623
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.621 reduce-arg.620)) (values reduce-arg.621))
            (body-matcher (reduce-arg.606)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.681)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.473 reduce-arg2.478 reduce-arg.606)
              (let ((+arg1.615 reduce-arg1.473) (+arg2.616 reduce-arg2.478))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.618 +arg1.615) (+arg2.619 +arg2.616))
                   (+ +arg1.618 +arg2.619))
                  (body-matcher map-result.617) (map-result (map-result.617))
                  (consumer (values)))))))))))
         (+arg1.566
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.565 reduce-arg.564)) (values reduce-arg.565))
            (body-matcher (reduce-arg.558)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.681)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.317 reduce-arg2.320 reduce-arg.558)
              (+ reduce-arg1.317 reduce-arg2.320)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.625 +arg2.623)) (+ +arg1.566 +arg2.625))
           (body-matcher map-result.624) (map-result (map-result.624))
           (consumer (values))))))))) |}]
;;

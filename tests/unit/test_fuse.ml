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
     ((+arg1.207 (frame 4 5 6)) (+arg2.209 (frame 7 8 9))
      (+arg1.211 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.176 +arg1.207) (+arg2.177 +arg2.209) (+arg1.182 +arg1.211))
         (+ +arg1.182 (+ +arg1.176 +arg2.177)))
        (body-matcher map-result.181) (map-result (map-result.181))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.266 (frame 1 2 3)) (+arg2.268 (frame 7 8 9))
      (+arg2.270 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.193 +arg2.266) (+arg2.212 +arg2.268) (+arg2.203 +arg2.270))
         (let ((fusion-target-map-result.255 (+ 1 +arg2.193)))
          (+ (+ fusion-target-map-result.255 +arg2.203)
           (+ fusion-target-map-result.255 +arg2.212))))
        (body-matcher map-result.216) (map-result (map-result.216))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.333 (frame 1 2 3)))
     (let
      ((arr.336
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.335 +arg2.333)) (+ 1 +arg2.335))
           (body-matcher map-result.334) (map-result (map-result.334))
           (consumer (values)))))))
      (let
       ((reduce-arg.368
         (contiguous-subarray arr.336 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.369 reduce-arg.368)) (values reduce-arg.369))
         (body-matcher (reduce-arg.362)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.336 (frame 0) (shape 3) (shape))
           (reduce-arg1.292 reduce-arg2.295 reduce-arg.362)
           (+ reduce-arg1.292 reduce-arg2.295)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.527 (frame 1 2 3)))
     (let
      ((fused-block-result.654
        (loop-block (frame-shape 3)
         (map ((+arg2.529 +arg2.527))
          (let ((fusion-target-map-result.649 (+ 4 +arg2.529)))
           (let
            ((fusion-target-map-result.653
              (values fusion-target-map-result.649
               (+ 5 fusion-target-map-result.649))))
            (values fusion-target-map-result.653
             (+ 6 (#0 fusion-target-map-result.653))))))
         (body-matcher ((map-result.528 map-result.538) map-result.581))
         (map-result (map-result.528 map-result.538 map-result.581))
         (consumer (values)))))
      (let
       ((reduce-arg.615
         (contiguous-subarray (#2 (#0 fused-block-result.654)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.572
         (contiguous-subarray (#1 (#0 fused-block-result.654)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.573 reduce-arg.572)) (values reduce-arg.573))
          (body-matcher (reduce-arg.566)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.654)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.343 reduce-arg2.346 reduce-arg.566)
            (+ reduce-arg1.343 reduce-arg2.346)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.616 reduce-arg.615)) (values reduce-arg.616))
          (body-matcher (reduce-arg.609)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.654)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.487 reduce-arg2.490 reduce-arg.609)
            (+ reduce-arg1.487 reduce-arg2.490))))))))) |}];
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
     ((+arg2.555 (frame 1 2 3))
      (+arg1.610 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.718
        (loop-block (frame-shape 3)
         (map ((+arg2.557 +arg2.555) (+arg1.613 +arg1.610))
          (let
           ((fusion-target-map-result.713 (+ 4 +arg2.557)) (+arg1.614 +arg1.613))
           (let
            ((fusion-target-map-result.717
              (values fusion-target-map-result.713
               (+ 5 fusion-target-map-result.713))))
            (values fusion-target-map-result.717
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.616 +arg1.614))
                 (+ +arg1.616 (#0 fusion-target-map-result.717)))
                (body-matcher map-result.615) (map-result (map-result.615))
                (consumer (values)))))))))
         (body-matcher ((map-result.556 map-result.567) map-result.611))
         (map-result (map-result.556 map-result.567 map-result.611))
         (consumer (values)))))
      (let
       ((reduce-arg.657
         (contiguous-subarray (#2 (#0 fused-block-result.718)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.601
         (contiguous-subarray (#1 (#0 fused-block-result.718)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.660
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.658 reduce-arg.657)) (values reduce-arg.658))
            (body-matcher (reduce-arg.643)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.718)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.510 reduce-arg2.515 reduce-arg.643)
              (let ((+arg1.652 reduce-arg1.510) (+arg2.653 reduce-arg2.515))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.655 +arg1.652) (+arg2.656 +arg2.653))
                   (+ +arg1.655 +arg2.656))
                  (body-matcher map-result.654) (map-result (map-result.654))
                  (consumer (values)))))))))))
         (+arg1.603
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.602 reduce-arg.601)) (values reduce-arg.602))
            (body-matcher (reduce-arg.595)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.718)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.354 reduce-arg2.357 reduce-arg.595)
              (+ reduce-arg1.354 reduce-arg2.357)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.662 +arg2.660)) (+ +arg1.603 +arg2.662))
           (body-matcher map-result.661) (map-result (map-result.661))
           (consumer (values))))))))) |}]
;;

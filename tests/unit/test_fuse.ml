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
     ((+arg1.182 (frame 4 5 6)) (+arg2.184 (frame 7 8 9))
      (+arg1.186 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.151 +arg1.182) (+arg2.152 +arg2.184) (+arg1.157 +arg1.186))
         (+ +arg1.157 (+ +arg1.151 +arg2.152)))
        (body-matcher map-result.156) (map-result (map-result.156))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.241 (frame 1 2 3)) (+arg2.243 (frame 7 8 9))
      (+arg2.245 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.168 +arg2.241) (+arg2.187 +arg2.243) (+arg2.178 +arg2.245))
         (let ((fusion-target-map-result.230 (+ 1 +arg2.168)))
          (+ (+ fusion-target-map-result.230 +arg2.178)
           (+ fusion-target-map-result.230 +arg2.187))))
        (body-matcher map-result.191) (map-result (map-result.191))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.308 (frame 1 2 3)))
     (let
      ((arr.311
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.310 +arg2.308)) (+ 1 +arg2.310))
           (body-matcher map-result.309) (map-result (map-result.309))
           (consumer (values)))))))
      (let
       ((reduce-arg.343
         (contiguous-subarray arr.311 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.344 reduce-arg.343)) (values reduce-arg.344))
         (body-matcher (reduce-arg.337)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.311 (frame 0) (shape 3) (shape))
           (reduce-arg1.267 reduce-arg2.270 reduce-arg.337)
           (+ reduce-arg1.267 reduce-arg2.270)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.502 (frame 1 2 3)))
     (let
      ((fused-block-result.629
        (loop-block (frame-shape 3)
         (map ((+arg2.504 +arg2.502))
          (let ((fusion-target-map-result.624 (+ 4 +arg2.504)))
           (let
            ((fusion-target-map-result.628
              (values fusion-target-map-result.624
               (+ 5 fusion-target-map-result.624))))
            (values fusion-target-map-result.628
             (+ 6 (#0 fusion-target-map-result.628))))))
         (body-matcher ((map-result.503 map-result.513) map-result.556))
         (map-result (map-result.503 map-result.513 map-result.556))
         (consumer (values)))))
      (let
       ((reduce-arg.590
         (contiguous-subarray (#2 (#0 fused-block-result.629)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.547
         (contiguous-subarray (#1 (#0 fused-block-result.629)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.548 reduce-arg.547)) (values reduce-arg.548))
          (body-matcher (reduce-arg.541)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.629)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.318 reduce-arg2.321 reduce-arg.541)
            (+ reduce-arg1.318 reduce-arg2.321)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.591 reduce-arg.590)) (values reduce-arg.591))
          (body-matcher (reduce-arg.584)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.629)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.462 reduce-arg2.465 reduce-arg.584)
            (+ reduce-arg1.462 reduce-arg2.465))))))))) |}];
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
     ((+arg2.530 (frame 1 2 3))
      (+arg1.585 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.693
        (loop-block (frame-shape 3)
         (map ((+arg2.532 +arg2.530) (+arg1.588 +arg1.585))
          (let
           ((fusion-target-map-result.688 (+ 4 +arg2.532)) (+arg1.589 +arg1.588))
           (let
            ((fusion-target-map-result.692
              (values fusion-target-map-result.688
               (+ 5 fusion-target-map-result.688))))
            (values fusion-target-map-result.692
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.591 +arg1.589))
                 (+ +arg1.591 (#0 fusion-target-map-result.692)))
                (body-matcher map-result.590) (map-result (map-result.590))
                (consumer (values)))))))))
         (body-matcher ((map-result.531 map-result.542) map-result.586))
         (map-result (map-result.531 map-result.542 map-result.586))
         (consumer (values)))))
      (let
       ((reduce-arg.632
         (contiguous-subarray (#2 (#0 fused-block-result.693)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.576
         (contiguous-subarray (#1 (#0 fused-block-result.693)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.635
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.633 reduce-arg.632)) (values reduce-arg.633))
            (body-matcher (reduce-arg.618)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.693)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.485 reduce-arg2.490 reduce-arg.618)
              (let ((+arg1.627 reduce-arg1.485) (+arg2.628 reduce-arg2.490))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.630 +arg1.627) (+arg2.631 +arg2.628))
                   (+ +arg1.630 +arg2.631))
                  (body-matcher map-result.629) (map-result (map-result.629))
                  (consumer (values)))))))))))
         (+arg1.578
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.577 reduce-arg.576)) (values reduce-arg.577))
            (body-matcher (reduce-arg.570)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.693)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.329 reduce-arg2.332 reduce-arg.570)
              (+ reduce-arg1.329 reduce-arg2.332)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.637 +arg2.635)) (+ +arg1.578 +arg2.637))
           (body-matcher map-result.636) (map-result (map-result.636))
           (consumer (values))))))))) |}]
;;

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
     ((+arg1.151 (frame 4 5 6)) (+arg2.153 (frame 7 8 9))
      (+arg1.155 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.120 +arg1.151) (+arg2.121 +arg2.153) (+arg1.126 +arg1.155))
         (+ +arg1.126 (+ +arg1.120 +arg2.121)))
        (body-matcher map-result.125) (map-result (map-result.125))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.210 (frame 1 2 3)) (+arg2.212 (frame 7 8 9))
      (+arg2.214 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.137 +arg2.210) (+arg2.156 +arg2.212) (+arg2.147 +arg2.214))
         (let ((fusion-target-map-result.199 (+ 1 +arg2.137)))
          (+ (+ fusion-target-map-result.199 +arg2.147)
           (+ fusion-target-map-result.199 +arg2.156))))
        (body-matcher map-result.160) (map-result (map-result.160))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.172 (frame 1 2 3)))
     (let
      ((arr.175
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.174 +arg2.172)) (+ 1 +arg2.174))
           (body-matcher map-result.173) (map-result (map-result.173))
           (consumer (values)))))))
      (let
       ((reduce-arg.191
         (contiguous-subarray arr.175 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.192 reduce-arg.191)) (values reduce-arg.192))
         (body-matcher (reduce-arg.190)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.175 (frame 0) (shape 3) (shape))
           (reduce-arg1.154 reduce-arg2.155 reduce-arg.190)
           (+ reduce-arg1.154 reduce-arg2.155)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.261 (frame 1 2 3)))
     (let
      ((fused-block-result.356
        (loop-block (frame-shape 3)
         (map ((+arg2.263 +arg2.261))
          (let ((fusion-target-map-result.351 (+ 4 +arg2.263)))
           (let
            ((fusion-target-map-result.355
              (values fusion-target-map-result.351
               (+ 5 fusion-target-map-result.351))))
            (values fusion-target-map-result.355
             (+ 6 (#0 fusion-target-map-result.355))))))
         (body-matcher ((map-result.262 map-result.272) map-result.299))
         (map-result (map-result.262 map-result.272 map-result.299))
         (consumer (values)))))
      (let
       ((reduce-arg.317
         (contiguous-subarray (#2 (#0 fused-block-result.356)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.290
         (contiguous-subarray (#1 (#0 fused-block-result.356)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.291 reduce-arg.290)) (values reduce-arg.291))
          (body-matcher (reduce-arg.289)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.356)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.189 reduce-arg2.190 reduce-arg.289)
            (+ reduce-arg1.189 reduce-arg2.190)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.318 reduce-arg.317)) (values reduce-arg.318))
          (body-matcher (reduce-arg.316)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.356)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.244 reduce-arg2.245 reduce-arg.316)
            (+ reduce-arg1.244 reduce-arg2.245))))))))) |}];
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
     ((+arg2.290 (frame 1 2 3))
      (+arg1.329 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.421
        (loop-block (frame-shape 3)
         (map ((+arg2.292 +arg2.290) (+arg1.332 +arg1.329))
          (let
           ((fusion-target-map-result.416 (+ 4 +arg2.292)) (+arg1.333 +arg1.332))
           (let
            ((fusion-target-map-result.420
              (values fusion-target-map-result.416
               (+ 5 fusion-target-map-result.416))))
            (values fusion-target-map-result.420
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.335 +arg1.333))
                 (+ +arg1.335 (#0 fusion-target-map-result.420)))
                (body-matcher map-result.334) (map-result (map-result.334))
                (consumer (values)))))))))
         (body-matcher ((map-result.291 map-result.302) map-result.330))
         (map-result (map-result.291 map-result.302 map-result.330))
         (consumer (values)))))
      (let
       ((reduce-arg.360
         (contiguous-subarray (#2 (#0 fused-block-result.421)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.320
         (contiguous-subarray (#1 (#0 fused-block-result.421)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.363
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.361 reduce-arg.360)) (values reduce-arg.361))
            (body-matcher (reduce-arg.351)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.421)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.268 reduce-arg2.271 reduce-arg.351)
              (let ((+arg1.355 reduce-arg1.268) (+arg2.356 reduce-arg2.271))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.358 +arg1.355) (+arg2.359 +arg2.356))
                   (+ +arg1.358 +arg2.359))
                  (body-matcher map-result.357) (map-result (map-result.357))
                  (consumer (values)))))))))))
         (+arg1.322
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.321 reduce-arg.320)) (values reduce-arg.321))
            (body-matcher (reduce-arg.319)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.421)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.200 reduce-arg2.201 reduce-arg.319)
              (+ reduce-arg1.200 reduce-arg2.201)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.365 +arg2.363)) (+ +arg1.322 +arg2.365))
           (body-matcher map-result.364) (map-result (map-result.364))
           (consumer (values))))))))) |}]
;;

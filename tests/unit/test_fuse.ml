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
    (let ((+arg2.146 (frame 1 2 3)))
     (let
      ((arr.149
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.148 +arg2.146)) (+ 1 +arg2.148))
           (body-matcher map-result.147) (map-result (map-result.147))
           (consumer (values)))))))
      (let
       ((reduce-arg.165
         (contiguous-subarray arr.149 (frame 1) (shape 3) (shape 2))))
       (#1
        (loop-block (frame-shape 2)
         (map ((reduce-arg.166 reduce-arg.165)) (values reduce-arg.166))
         (body-matcher (reduce-arg.164)) (map-result ())
         (consumer
          (reduce-zero (contiguous-subarray arr.149 (frame 0) (shape 3) (shape))
           (reduce-arg1.132 reduce-arg2.133 reduce-arg.164)
           (+ reduce-arg1.132 reduce-arg2.133)))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.209 (frame 1 2 3)))
     (let
      ((fused-block-result.304
        (loop-block (frame-shape 3)
         (map ((+arg2.211 +arg2.209))
          (let ((fusion-target-map-result.299 (+ 4 +arg2.211)))
           (let
            ((fusion-target-map-result.303
              (values fusion-target-map-result.299
               (+ 5 fusion-target-map-result.299))))
            (values fusion-target-map-result.303
             (+ 6 (#0 fusion-target-map-result.303))))))
         (body-matcher ((map-result.210 map-result.220) map-result.247))
         (map-result (map-result.210 map-result.220 map-result.247))
         (consumer (values)))))
      (let
       ((reduce-arg.265
         (contiguous-subarray (#2 (#0 fused-block-result.304)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.238
         (contiguous-subarray (#1 (#0 fused-block-result.304)) (frame 1)
          (shape 3) (shape 2))))
       (+
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.239 reduce-arg.238)) (values reduce-arg.239))
          (body-matcher (reduce-arg.237)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#1 (#0 fused-block-result.304)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.167 reduce-arg2.168 reduce-arg.237)
            (+ reduce-arg1.167 reduce-arg2.168)))))
        (#1
         (loop-block (frame-shape 2)
          (map ((reduce-arg.266 reduce-arg.265)) (values reduce-arg.266))
          (body-matcher (reduce-arg.264)) (map-result ())
          (consumer
           (reduce-zero
            (contiguous-subarray (#2 (#0 fused-block-result.304)) (frame 0)
             (shape 3) (shape))
            (reduce-arg1.196 reduce-arg2.197 reduce-arg.264)
            (+ reduce-arg1.196 reduce-arg2.197))))))))) |}];
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
     ((+arg2.231 (frame 1 2 3))
      (+arg1.270 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((fused-block-result.362
        (loop-block (frame-shape 3)
         (map ((+arg2.233 +arg2.231) (+arg1.273 +arg1.270))
          (let
           ((fusion-target-map-result.357 (+ 4 +arg2.233)) (+arg1.274 +arg1.273))
           (let
            ((fusion-target-map-result.361
              (values fusion-target-map-result.357
               (+ 5 fusion-target-map-result.357))))
            (values fusion-target-map-result.361
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.276 +arg1.274))
                 (+ +arg1.276 (#0 fusion-target-map-result.361)))
                (body-matcher map-result.275) (map-result (map-result.275))
                (consumer (values)))))))))
         (body-matcher ((map-result.232 map-result.243) map-result.271))
         (map-result (map-result.232 map-result.243 map-result.271))
         (consumer (values)))))
      (let
       ((reduce-arg.301
         (contiguous-subarray (#2 (#0 fused-block-result.362)) (frame 1)
          (shape 3) (shape 2)))
        (reduce-arg.261
         (contiguous-subarray (#1 (#0 fused-block-result.362)) (frame 1)
          (shape 3) (shape 2))))
       (let
        ((+arg2.304
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.302 reduce-arg.301)) (values reduce-arg.302))
            (body-matcher (reduce-arg.292)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#2 (#0 fused-block-result.362)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.213 reduce-arg2.216 reduce-arg.292)
              (let ((+arg1.296 reduce-arg1.213) (+arg2.297 reduce-arg2.216))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map ((+arg1.299 +arg1.296) (+arg2.300 +arg2.297))
                   (+ +arg1.299 +arg2.300))
                  (body-matcher map-result.298) (map-result (map-result.298))
                  (consumer (values)))))))))))
         (+arg1.263
          (#1
           (loop-block (frame-shape 2)
            (map ((reduce-arg.262 reduce-arg.261)) (values reduce-arg.262))
            (body-matcher (reduce-arg.260)) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#1 (#0 fused-block-result.362)) (frame 0)
               (shape 3) (shape))
              (reduce-arg1.178 reduce-arg2.179 reduce-arg.260)
              (+ reduce-arg1.178 reduce-arg2.179)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.306 +arg2.304)) (+ +arg1.263 +arg2.306))
           (body-matcher map-result.305) (map-result (map-result.305))
           (consumer (values))))))))) |}]
;;

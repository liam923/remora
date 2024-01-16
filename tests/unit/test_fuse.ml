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
     ((+arg1.103 (frame 4 5 6)) (+arg2.105 (frame 7 8 9))
      (+arg1.107 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.72 +arg1.103) (+arg2.73 +arg2.105) (+arg1.78 +arg1.107))
         (+ +arg1.78 (+ +arg1.72 +arg2.73)))
        (body-matcher map-result.77) (map-result (map-result.77))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.162 (frame 1 2 3)) (+arg2.164 (frame 7 8 9))
      (+arg2.166 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.89 +arg2.162) (+arg2.108 +arg2.164) (+arg2.99 +arg2.166))
         (let ((fusion-target-map-result.151 (+ 1 +arg2.89)))
          (+ (+ fusion-target-map-result.151 +arg2.99)
           (+ fusion-target-map-result.151 +arg2.108))))
        (body-matcher map-result.112) (map-result (map-result.112))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.94 (frame 1 2 3)))
     (#1
      (loop-block (frame-shape 3) (map ((+arg2.69 +arg2.94)) (+ 1 +arg2.69))
       (body-matcher reduce-arg.71) (map-result ())
       (consumer
        (reduce (reduce-arg1.60 reduce-arg2.61 reduce-arg.71)
         (+ reduce-arg1.60 reduce-arg2.61)))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 []} + (+ 5 x))
        (reduce{int | 2 []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let ((+arg2.190 (frame 1 2 3)))
     (let
      ((consumer-result.191
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.101 +arg2.190))
           (let ((fusion-target-map-result.174 (+ 4 +arg2.101)))
            (values (+ 5 fusion-target-map-result.174)
             (+ 6 fusion-target-map-result.174))))
          (body-matcher (reduce-arg.113 reduce-arg.126)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.166 fused-reduce-arg2.167
             (reduce-arg.113 reduce-arg.126))
            (values (+ (#0 fused-reduce-arg1.166) (#0 fused-reduce-arg2.167))
             (+ (#1 fused-reduce-arg1.166) (#1 fused-reduce-arg2.167)))))))))
      (+ (#0 consumer-result.191) (#1 consumer-result.191)))) |}];
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
     ((+arg2.264 (frame 1 2 3))
      (+arg1.266 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((consumer-result.267
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.123 +arg2.264) (+arg1.149 +arg1.266))
           (let
            ((fusion-target-map-result.232 (+ 4 +arg2.123))
             (+arg1.259 +arg1.149))
            (values (+ 5 fusion-target-map-result.232)
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg1.152 +arg1.259))
                 (+ +arg1.152 fusion-target-map-result.232))
                (body-matcher map-result.151) (map-result (map-result.151))
                (consumer (values))))))))
          (body-matcher (reduce-arg.136 reduce-arg.154)) (map-result ())
          (consumer
           (reduce
            (fused-reduce-arg1.224 fused-reduce-arg2.225
             (reduce-arg.136 reduce-arg.154))
            (let
             ((+arg1.247 (#1 fused-reduce-arg1.224))
              (+arg2.249 (#1 fused-reduce-arg2.225)))
             (values (+ (#0 fused-reduce-arg1.224) (#0 fused-reduce-arg2.225))
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.161 +arg1.247) (+arg2.162 +arg2.249))
                  (+ +arg1.161 +arg2.162))
                 (body-matcher map-result.160) (map-result (map-result.160))
                 (consumer (values)))))))))))))
      (let ((+arg2.239 (#1 consumer-result.267)))
       (#0
        (#0
         (loop-block (frame-shape 3)
          (map ((+arg2.168 +arg2.239)) (+ (#0 consumer-result.267) +arg2.168))
          (body-matcher map-result.167) (map-result (map-result.167))
          (consumer (values)))))))) |}]
;;

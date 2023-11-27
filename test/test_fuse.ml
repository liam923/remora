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
     ((+arg1.104 (frame 4 5 6)) (+arg2.106 (frame 7 8 9))
      (+arg1.108 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.73 +arg1.104) (+arg2.74 +arg2.106) (+arg1.79 +arg1.108))
         (+ +arg1.79 (+ +arg1.73 +arg2.74)))
        (body-matcher map-result.78) (map-result (map-result.78))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let
     ((+arg2.163 (frame 1 2 3)) (+arg2.165 (frame 7 8 9))
      (+arg2.167 (frame 4 5 6)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg2.90 +arg2.163) (+arg2.109 +arg2.165) (+arg2.100 +arg2.167))
         (let ((fusion-target-map-result.152 (+ 1 +arg2.90)))
          (+ (+ fusion-target-map-result.152 +arg2.100)
           (+ fusion-target-map-result.152 +arg2.109))))
        (body-matcher map-result.113) (map-result (map-result.113))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.95 (frame 1 2 3)))
     (#1
      (loop-block (frame-shape 3) (map ((+arg2.70 +arg2.95)) (+ 1 +arg2.70))
       (body-matcher reduce-arg.72) (map-result ())
       (consumer
        (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.72)
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
    (let ((+arg2.191 (frame 1 2 3)))
     (let
      ((consumer-result.192
        (#1
         (loop-block (frame-shape 3)
          (map ((+arg2.102 +arg2.191))
           (let ((fusion-target-map-result.175 (+ 4 +arg2.102)))
            (values (+ 5 fusion-target-map-result.175)
             (+ 6 fusion-target-map-result.175))))
          (body-matcher (reduce-arg.114 reduce-arg.127)) (map-result ())
          (consumer
           (reduce (shape)
            (fused-reduce-arg1.167 fused-reduce-arg2.168
             (reduce-arg.114 reduce-arg.127))
            (values
             (+ (#0 (unzip fused-reduce-arg1.167))
              (#0 (unzip fused-reduce-arg2.168)))
             (+ (#1 (unzip fused-reduce-arg1.167))
              (#1 (unzip fused-reduce-arg2.168))))))))))
      (+ (#0 consumer-result.192) (#1 consumer-result.192)))) |}];
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
     ((+arg2.222 (frame 1 2 3))
      (+arg1.224 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
     (let
      ((loop-block-result.225
        (loop-block (frame-shape 3)
         (map ((+arg2.108 +arg2.222) (+arg1.133 +arg1.224))
          (let
           ((fusion-target-map-result.194 (+ 4 +arg2.108)) (+arg1.215 +arg1.133))
           (values
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.136 +arg1.215))
                (+ +arg1.136 fusion-target-map-result.194))
               (body-matcher map-result.135) (map-result (map-result.135))
               (consumer (values)))))
            (+ 5 fusion-target-map-result.194))))
         (body-matcher (map-result.131 reduce-arg.120))
         (map-result (map-result.131))
         (consumer
          (reduce (shape) (reduce-arg1.85 reduce-arg2.86 reduce-arg.120)
           (+ reduce-arg1.85 reduce-arg2.86))))))
      (let ((reduce-arg.207 (#0 (#0 loop-block-result.225))))
       (let
        ((+arg2.201
          (#1
           (loop-block (frame-shape 3)
            (map ((reduce-arg.140 reduce-arg.207)) reduce-arg.140)
            (body-matcher reduce-arg.138) (map-result ())
            (consumer
             (reduce (shape 3) (reduce-arg1.99 reduce-arg2.100 reduce-arg.138)
              (+ reduce-arg1.99 reduce-arg2.100)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.144 +arg2.201)) (+ (#1 loop-block-result.225) +arg2.144))
           (body-matcher map-result.143) (map-result (map-result.143))
           (consumer (values))))))))) |}]
;;

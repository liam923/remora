open! Base
open Remora

let%expect_test "check fusing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module Fuse.Stage (Source.UnitBuilder))
      @> (module SimplifyNested.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let fuseAndPrint = TestPipeline.runAndPrint pipeline in
  fuseAndPrint "(+ [1 2 3] (+ [4 5 6] [7 8 9]))";
  [%expect
    {|
    (#0
     (#0
      (let
       ((+arg1.80 (frame 4 5 6)) (+arg2.82 (frame 7 8 9))
        (+arg1.84 (frame 1 2 3)))
       (loop-block (frame-shape 3)
        (map ((+arg1.65 +arg1.80) (+arg2.66 +arg2.82) (+arg1.69 +arg1.84))
         (+ +arg1.69 (+ +arg1.65 +arg2.66)))
        (body-matcher map-result.68) (map-result (map-result.68))
        (consumer (values)))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (#0
     (#0
      (let
       ((+arg2.120 (frame 1 2 3)) (+arg2.122 (frame 7 8 9))
        (+arg2.124 (frame 4 5 6)))
       (loop-block (frame-shape 3)
        (map ((+arg2.83 +arg2.120) (+arg2.95 +arg2.122) (+arg2.89 +arg2.124))
         (let ((fusion-target-map-result.109 (+ 1 +arg2.83)))
          (+ (+ fusion-target-map-result.109 +arg2.89)
           (+ fusion-target-map-result.109 +arg2.95))))
        (body-matcher map-result.97) (map-result (map-result.97))
        (consumer (values)))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (#1
     (let ((+arg2.75 (frame 1 2 3)))
      (loop-block (frame-shape 3) (map ((+arg2.62 +arg2.75)) (+ 1 +arg2.62))
       (body-matcher reduce-arg.59) (map-result ())
       (consumer
        (reduce (shape) (reduce-arg1.57 reduce-arg2.58 reduce-arg.59)
         (+ reduce-arg1.57 reduce-arg2.58)))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (let
     ((fused-block-result.127
       (#1
        (let ((+arg2.145 (frame 1 2 3)))
         (loop-block (frame-shape 3)
          (map ((+arg2.95 +arg2.145))
           (let
            ((fusion-target-map-result.125
              (let ((fusion-target-map-result.120 (+ 4 +arg2.95)))
               (values fusion-target-map-result.120
                (+ 5 fusion-target-map-result.120)))))
            (values (#1 fusion-target-map-result.125)
             (+ 6 (#0 fusion-target-map-result.125)))))
          (body-matcher (reduce-arg.97 reduce-arg.103)) (map-result ())
          (consumer
           (reduce (shape)
            (fused-reduce-arg1.129 fused-reduce-arg2.130
             (reduce-arg.97 reduce-arg.103))
            (values
             (+ (#0 (unzip fused-reduce-arg1.129))
              (#0 (unzip fused-reduce-arg2.130)))
             (+ (#1 (unzip fused-reduce-arg1.129))
              (#1 (unzip fused-reduce-arg2.130)))))))))))
     (+ (#0 fused-block-result.127) (#1 fused-block-result.127))) |}];
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
     ((fused-block-result.138
       (let
        ((loop-block-result.168
          (let ((+arg2.167 (frame 1 2 3)))
           (loop-block (frame-shape 3)
            (map ((+arg2.102 +arg2.167))
             (let ((fusion-target-map-result.136 (+ 4 +arg2.102)))
              (values fusion-target-map-result.136
               (+ 5 fusion-target-map-result.136))))
            (body-matcher (map-result.101 reduce-arg.104))
            (map-result (map-result.101))
            (consumer
             (reduce (shape) (reduce-arg1.81 reduce-arg2.82 reduce-arg.104)
              (+ reduce-arg1.81 reduce-arg2.82)))))))
        (values (#0 (#0 loop-block-result.168)) (#1 loop-block-result.168)))))
     (#0
      (#0
       (let
        ((+arg2.143
          (#1
           (let
            ((+arg2.155 (#0 fused-block-result.138))
             (+arg1.157 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
            (loop-block (frame-shape 3)
             (map ((+arg2.115 +arg2.155) (+arg1.116 +arg1.157))
              (#0
               (#0
                (let ((+arg1.151 +arg1.116))
                 (loop-block (frame-shape 3)
                  (map ((+arg1.119 +arg1.151)) (+ +arg1.119 +arg2.115))
                  (body-matcher map-result.118) (map-result (map-result.118))
                  (consumer (values)))))))
             (body-matcher reduce-arg.111) (map-result ())
             (consumer
              (reduce (shape 3) (reduce-arg1.95 reduce-arg2.96 reduce-arg.111)
               (+ reduce-arg1.95 reduce-arg2.96))))))))
        (loop-block (frame-shape 3)
         (map ((+arg2.124 +arg2.143)) (+ (#1 fused-block-result.138) +arg2.124))
         (body-matcher map-result.123) (map-result (map-result.123))
         (consumer (values))))))) |}]
;;

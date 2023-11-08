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
    (let ((+arg1.61 (frame 1 2 3)))
     (let ((+arg1.62 (frame 4 5 6)) (+arg2.63 (frame 7 8 9)))
      (#1
       (#0
        (consumer-block (frame-shape 3)
         (map ((+arg1.65 +arg1.62) (+arg2.66 +arg2.63) (+arg1.69 +arg1.61))
          (let ((fusion-target-map-result.72 (+ +arg1.65 +arg2.66)))
           (values fusion-target-map-result.72
            (+ +arg1.69 fusion-target-map-result.72))))
         (body-matcher (map-result.64 map-result.68))
         (map-result (map-result.64 map-result.68)) (consumer (values))))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (let ((+arg2.81 (frame 1 2 3)))
     (let ((+arg2.92 (frame 7 8 9)))
      (let ((+arg2.86 (frame 4 5 6)))
       (#3
        (#0
         (consumer-block (frame-shape 3)
          (map ((+arg2.83 +arg2.81) (+arg2.95 +arg2.92) (+arg2.89 +arg2.86))
           (let ((fusion-target-map-result.109 (+ 1 +arg2.83)))
            (values fusion-target-map-result.109
             (let
              ((fusion-target-map-result.105
                (+ fusion-target-map-result.109 +arg2.95)))
              (values fusion-target-map-result.105
               (let
                ((fusion-target-map-result.101
                  (+ fusion-target-map-result.109 +arg2.89)))
                (values fusion-target-map-result.101
                 (+ fusion-target-map-result.101 fusion-target-map-result.105))))))))
          (body-matcher
           (map-result.82 (map-result.93 (map-result.87 map-result.97))))
          (map-result (map-result.82 map-result.93 map-result.87 map-result.97))
          (consumer (values)))))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (let ((+arg2.60 (frame 1 2 3)))
     (#1
      (consumer-block (frame-shape 3)
       (map ((+arg2.62 +arg2.60))
        (let ((fusion-target-map-result.66 (+ 1 +arg2.62)))
         (values fusion-target-map-result.66
          (values fusion-target-map-result.66))))
       (body-matcher (map-result.61 (reduce-arg.59)))
       (map-result (map-result.61))
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
    (let ((+arg2.93 (frame 1 2 3)))
     (let
      ((fused-block-result.127
        (consumer-block (frame-shape 3)
         (map ((+arg2.95 +arg2.93))
          (let
           ((fusion-target-map-result.125
             (let ((fusion-target-map-result.120 (+ 4 +arg2.95)))
              (values fusion-target-map-result.120
               (let
                ((fusion-target-map-result.110
                  (+ 5 fusion-target-map-result.120)))
                (values fusion-target-map-result.110
                 (values fusion-target-map-result.110)))))))
           (values fusion-target-map-result.125
            (let
             ((fusion-target-map-result.115
               (+ 6 (#0 fusion-target-map-result.125))))
             (values fusion-target-map-result.115
              (values fusion-target-map-result.115))))))
         (body-matcher
          ((map-result.94 (map-result.99 (reduce-arg.97)))
           (map-result.105 (reduce-arg.103))))
         (map-result (map-result.94 map-result.99 map-result.105))
         (consumer
          (reduce (shape)
           (fused-reduce-arg1.129 fused-reduce-arg2.130
            (reduce-arg.97 reduce-arg.103))
           (values (+ (#0 fused-reduce-arg1.129) (#0 fused-reduce-arg2.130))
            (+ (#1 fused-reduce-arg1.129) (#1 fused-reduce-arg2.130))))))))
      (+ (#0 (#1 fused-block-result.127)) (#1 (#1 fused-block-result.127))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [3] []} + (+ [[6 7 8] [6 7 8] [6 7 8]] x)))
    |};
  [%expect
    {|
    (let ((+arg2.100 (frame 1 2 3)))
     (let
      ((fused-block-result.138
        (consumer-block (frame-shape 3)
         (map ((+arg2.102 +arg2.100))
          (let ((fusion-target-map-result.136 (+ 4 +arg2.102)))
           (values fusion-target-map-result.136
            (let
             ((fusion-target-map-result.126 (+ 5 fusion-target-map-result.136)))
             (values fusion-target-map-result.126
              (values fusion-target-map-result.126))))))
         (body-matcher (map-result.101 (map-result.106 (reduce-arg.104))))
         (map-result (map-result.101 map-result.106))
         (consumer
          (reduce (shape) (reduce-arg1.81 reduce-arg2.82 reduce-arg.104)
           (+ reduce-arg1.81 reduce-arg2.82))))))
      (let
       ((fusion-target-map-result.139 (values (#0 (#0 fused-block-result.138))))
        (fusion-archer-consumer-result.137 (#1 fused-block-result.138)))
       (let ((x.103 (#0 fusion-target-map-result.139)))
        (let ((x.77 x.103))
         (let ((hoistedExp.110 fusion-archer-consumer-result.137))
          (let ((hoistedExp.99 hoistedExp.110))
           (let
            ((+arg2.122
              (let
               ((+arg2.112 x.77)
                (+arg1.113 (frame (frame 6 7 8) (frame 6 7 8) (frame 6 7 8))))
               (#1
                (consumer-block (frame-shape 3)
                 (map ((+arg2.115 +arg2.112) (+arg1.116 +arg1.113))
                  (let
                   ((fusion-target-map-result.131
                     (let ((+arg2.93 +arg2.115) (+arg1.90 +arg1.116))
                      (let ((+arg1.117 +arg1.90))
                       (#0
                        (#0
                         (consumer-block (frame-shape 3)
                          (map ((+arg1.119 +arg1.117)) (+ +arg1.119 +arg2.93))
                          (body-matcher map-result.118)
                          (map-result (map-result.118)) (consumer (values)))))))))
                   (values fusion-target-map-result.131
                    (values fusion-target-map-result.131))))
                 (body-matcher (map-result.114 (reduce-arg.111)))
                 (map-result (map-result.114))
                 (consumer
                  (reduce (shape 3)
                   (reduce-arg1.95 reduce-arg2.96 reduce-arg.111)
                   (+ reduce-arg1.95 reduce-arg2.96))))))))
            (#0
             (#0
              (consumer-block (frame-shape 3)
               (map ((+arg2.124 +arg2.122)) (+ hoistedExp.99 +arg2.124))
               (body-matcher map-result.123) (map-result (map-result.123))
               (consumer (values))))))))))))) |}]
;;

open! Base
open Remora

let%expect_test "check kernelizing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module FuseAndSimplify.Stage (Source.UnitBuilder))
      @> (module Kernelize.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Corn) (Source.UnitBuilder))
      @> empty)
  in
  let kernelizeAndPrint = TestPipeline.runAndPrint pipeline in
  kernelizeAndPrint "(+ [1 2 3] (+ [4 5 6] [7 8 9]))";
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
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    (#0
     (map-kernel (frame-shape 1000000) () (iota iota.70)
      (body-matcher map-result.75) (map-result (map-result.75))
      (#1
       (loop-block (frame-shape 10) (map () (iota (iota.72 : iota.70)) iota.72)
        (body-matcher reduce-arg.80) (map-result ())
        (consumer
         (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
          (+ reduce-arg1.65 reduce-arg2.66))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 10)
       (map () (iota iota.70)
        (#1
         (loop-kernel (frame-shape 1000000)
          (map () (iota (iota.72 : iota.70)) iota.72)
          (body-matcher reduce-arg.80) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
            (+ reduce-arg1.65 reduce-arg2.66))))))
       (body-matcher map-result.75) (map-result (map-result.75))
       (consumer (values))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [1000000 1000000]})
    |};
  [%expect
    {|
    (#0
     (map-kernel (frame-shape 1000000) () (iota iota.70)
      (body-matcher map-result.75) (map-result (map-result.75))
      (#1
       (loop-block (frame-shape 1000000)
        (map () (iota (iota.72 : iota.70)) iota.72) (body-matcher reduce-arg.80)
        (map-result ())
        (consumer
         (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
          (+ reduce-arg1.65 reduce-arg2.66))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (#0
     (map-kernel (frame-shape 5000) () (iota iota.70)
      (body-matcher map-result.75) (map-result (map-result.75))
      (#1
       (loop-block (frame-shape 5000) (map () (iota (iota.72 : iota.70)) iota.72)
        (body-matcher reduce-arg.80) (map-result ())
        (consumer
         (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
          (+ reduce-arg1.65 reduce-arg2.66))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 5000)
       (map () (iota iota.70)
        (#1
         (loop-kernel (frame-shape 6000)
          (map () (iota (iota.72 : iota.70)) iota.72)
          (body-matcher reduce-arg.80) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
            (+ reduce-arg1.65 reduce-arg2.66))))))
       (body-matcher map-result.75) (map-result (map-result.75))
       (consumer (values))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [6000 5000]})
    |};
  [%expect
    {|
    (#0
     (map-kernel (frame-shape 6000) () (iota iota.70)
      (body-matcher map-result.75) (map-result (map-result.75))
      (#1
       (loop-block (frame-shape 5000) (map () (iota (iota.72 : iota.70)) iota.72)
        (body-matcher reduce-arg.80) (map-result ())
        (consumer
         (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
          (+ reduce-arg1.65 reduce-arg2.66))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (map-kernel (frame-shape 10) () (iota iota.57) (body-matcher map-result.64)
      (map-result (map-result.64))
      (#0
       (map-kernel (frame-shape 20) () (iota (iota.59 : iota.57))
        (body-matcher map-result.66) (map-result (map-result.66))
        (#0
         (map-kernel (frame-shape 30) () (iota (iota.61 : iota.59))
          (body-matcher map-result.68) (map-result (map-result.68))
          (+ 1 iota.61))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce-non-assoc{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 10)
       (map () (iota iota.70)
        (#1
         (loop-block (frame-shape 1000000)
          (map () (iota (iota.72 : iota.70)) iota.72)
          (body-matcher reduce-arg.80) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
            (+ reduce-arg1.65 reduce-arg2.66))))))
       (body-matcher map-result.75) (map-result (map-result.75))
       (consumer (values))))) |}]
;;

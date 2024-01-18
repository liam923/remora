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
     ((+arg1.121 (frame 4 5 6)) (+arg2.123 (frame 7 8 9))
      (+arg1.125 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.90 +arg1.121) (+arg2.91 +arg2.123) (+arg1.96 +arg1.125))
         (+ +arg1.96 (+ +arg1.90 +arg2.91)))
        (body-matcher map-result.95) (map-result (map-result.95))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    (#0
     (kernel (blocks 320) (threads 32)
      (map-kernel (frame-shape 1000000) () (iota iota.87)
       (body-matcher map-result.92) (map-result (map-result.92))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.89 : iota.87)) iota.89)
         (body-matcher reduce-arg.97) (map-result ())
         (consumer
          (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
           (+ reduce-arg1.82 reduce-arg2.83)))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 10)
       (map () (iota iota.87)
        (#1
         (kernel (blocks 320) (threads 32)
          (loop-kernel (frame-shape 1000000)
           (map () (iota (iota.89 : iota.87)) iota.89)
           (body-matcher reduce-arg.97) (map-result ())
           (consumer
            ((reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
              (+ reduce-arg1.82 reduce-arg2.83))
             (outer-body (+ reduce-arg1.82 reduce-arg2.83))))))))
       (body-matcher map-result.92) (map-result (map-result.92))
       (consumer (values))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [1000000 1000000]})
    |};
  [%expect
    {|
    (#0
     (kernel (blocks 320) (threads 32)
      (map-kernel (frame-shape 1000000) () (iota iota.87)
       (body-matcher map-result.92) (map-result (map-result.92))
       (#1
        (loop-block (frame-shape 1000000)
         (map () (iota (iota.89 : iota.87)) iota.89) (body-matcher reduce-arg.97)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
           (+ reduce-arg1.82 reduce-arg2.83)))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (#0
     (kernel (blocks 157) (threads 32)
      (map-kernel (frame-shape 5000) () (iota iota.87)
       (body-matcher map-result.92) (map-result (map-result.92))
       (#1
        (loop-block (frame-shape 5000)
         (map () (iota (iota.89 : iota.87)) iota.89) (body-matcher reduce-arg.97)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
           (+ reduce-arg1.82 reduce-arg2.83)))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 5000)
       (map () (iota iota.87)
        (#1
         (kernel (blocks 188) (threads 32)
          (loop-kernel (frame-shape 6000)
           (map () (iota (iota.89 : iota.87)) iota.89)
           (body-matcher reduce-arg.97) (map-result ())
           (consumer
            ((reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
              (+ reduce-arg1.82 reduce-arg2.83))
             (outer-body (+ reduce-arg1.82 reduce-arg2.83))))))))
       (body-matcher map-result.92) (map-result (map-result.92))
       (consumer (values))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 4999} iota{ | [6000 5000]})
    |};
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 6000) () (iota iota.87)
       (body-matcher map-result.92) (map-result (map-result.92))
       (#1
        (loop-block (frame-shape 5000)
         (map () (iota (iota.89 : iota.87)) iota.89) (body-matcher reduce-arg.97)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
           (+ reduce-arg1.82 reduce-arg2.83)))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.74) (body-matcher map-result.81)
       (map-result (map-result.81))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.76 : iota.74))
         (body-matcher map-result.83) (map-result (map-result.83))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.78 : iota.76))
           (body-matcher map-result.85) (map-result (map-result.85))
           (+ 1 iota.78)))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce-non-assoc{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 10)
       (map () (iota iota.87)
        (#1
         (loop-block (frame-shape 1000000)
          (map () (iota (iota.89 : iota.87)) iota.89)
          (body-matcher reduce-arg.97) (map-result ())
          (consumer
           (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
            (+ reduce-arg1.82 reduce-arg2.83))))))
       (body-matcher map-result.92) (map-result (map-result.92))
       (consumer (values))))) |}]
;;

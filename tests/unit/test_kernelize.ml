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
     ((+arg1.103 (frame 4 5 6)) (+arg2.105 (frame 7 8 9))
      (+arg1.107 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.72 +arg1.103) (+arg2.73 +arg2.105) (+arg1.78 +arg1.107))
         (+ +arg1.78 (+ +arg1.72 +arg2.73)))
        (body-matcher map-result.77) (map-result (map-result.77))
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
      (map-kernel (frame-shape 1000000) () (iota iota.69)
       (body-matcher map-result.74) (map-result (map-result.74))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.71 : iota.69)) iota.71)
         (body-matcher reduce-arg.79) (map-result ())
         (consumer
          (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
           (+ reduce-arg1.64 reduce-arg2.65)))))))) |}];
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
       (map () (iota iota.69)
        (#1
         (kernel (blocks 320) (threads 32)
          (loop-kernel (frame-shape 1000000)
           (map () (iota (iota.71 : iota.69)) iota.71)
           (body-matcher reduce-arg.79) (map-result ())
           (consumer
            ((reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
              (+ reduce-arg1.64 reduce-arg2.65))
             (outer-body (+ reduce-arg1.64 reduce-arg2.65))))))))
       (body-matcher map-result.74) (map-result (map-result.74))
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
      (map-kernel (frame-shape 1000000) () (iota iota.69)
       (body-matcher map-result.74) (map-result (map-result.74))
       (#1
        (loop-block (frame-shape 1000000)
         (map () (iota (iota.71 : iota.69)) iota.71) (body-matcher reduce-arg.79)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
           (+ reduce-arg1.64 reduce-arg2.65)))))))) |}];
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
      (map-kernel (frame-shape 5000) () (iota iota.69)
       (body-matcher map-result.74) (map-result (map-result.74))
       (#1
        (loop-block (frame-shape 5000)
         (map () (iota (iota.71 : iota.69)) iota.71) (body-matcher reduce-arg.79)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
           (+ reduce-arg1.64 reduce-arg2.65)))))))) |}];
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
       (map () (iota iota.69)
        (#1
         (kernel (blocks 188) (threads 32)
          (loop-kernel (frame-shape 6000)
           (map () (iota (iota.71 : iota.69)) iota.71)
           (body-matcher reduce-arg.79) (map-result ())
           (consumer
            ((reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
              (+ reduce-arg1.64 reduce-arg2.65))
             (outer-body (+ reduce-arg1.64 reduce-arg2.65))))))))
       (body-matcher map-result.74) (map-result (map-result.74))
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
      (map-kernel (frame-shape 6000) () (iota iota.69)
       (body-matcher map-result.74) (map-result (map-result.74))
       (#1
        (loop-block (frame-shape 5000)
         (map () (iota (iota.71 : iota.69)) iota.71) (body-matcher reduce-arg.79)
         (map-result ())
         (consumer
          (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
           (+ reduce-arg1.64 reduce-arg2.65)))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.56) (body-matcher map-result.63)
       (map-result (map-result.63))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.58 : iota.56))
         (body-matcher map-result.65) (map-result (map-result.65))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.60 : iota.58))
           (body-matcher map-result.67) (map-result (map-result.67))
           (+ 1 iota.60)))))))) |}];
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
       (map () (iota iota.69)
        (#1
         (loop-block (frame-shape 1000000)
          (map () (iota (iota.71 : iota.69)) iota.71)
          (body-matcher reduce-arg.79) (map-result ())
          (consumer
           (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
            (+ reduce-arg1.64 reduce-arg2.65))))))
       (body-matcher map-result.74) (map-result (map-result.74))
       (consumer (values))))) |}]
;;

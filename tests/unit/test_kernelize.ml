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
     ((+arg1.158 (frame 4 5 6)) (+arg2.160 (frame 7 8 9))
      (+arg1.162 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.127 +arg1.158) (+arg2.128 +arg2.160) (+arg1.133 +arg1.162))
         (+ +arg1.133 (+ +arg1.127 +arg2.128)))
        (body-matcher map-result.132) (map-result (map-result.132))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.334)
        (body-matcher map-result.339) (map-result (map-result.339))
        (let
         ((map-result.408
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.336 : iota.334)) iota.336)
             (body-matcher map-result.335) (map-result (map-result.335))
             (consumer (values))))))
         (let
          ((reduce-arg.403
            (contiguous-subarray (#0 map-result.408)
             contiguous-subarray-index.356 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
            (body-matcher reduce-arg.369) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.408)
               contiguous-subarray-index.350 (shape 10) (shape))
              (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
              (+ reduce-arg1.295 reduce-arg2.298))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.334)
         (let
          ((map-result.408
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.336 : iota.334))
              (body-matcher map-result.335) (map-result (map-result.335))
              iota.336))))
          (let
           ((reduce-arg.403
             (contiguous-subarray (#0 map-result.408)
              contiguous-subarray-index.356 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
              (body-matcher reduce-arg.369) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.408)
                  contiguous-subarray-index.350 (shape 1000000) (shape))
                 (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
                 (+ reduce-arg1.295 reduce-arg2.298))
                (outer-body (+ reduce-arg1.295 reduce-arg2.298))))))))))
        (body-matcher map-result.339) (map-result (map-result.339))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [1000000 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.334)
        (body-matcher map-result.339) (map-result (map-result.339))
        (let
         ((map-result.408
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.336 : iota.334)) iota.336)
             (body-matcher map-result.335) (map-result (map-result.335))
             (consumer (values))))))
         (let
          ((reduce-arg.403
            (contiguous-subarray (#0 map-result.408)
             contiguous-subarray-index.356 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
            (body-matcher reduce-arg.369) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.408)
               contiguous-subarray-index.350 (shape 1000000) (shape))
              (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
              (+ reduce-arg1.295 reduce-arg2.298))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (kernel (blocks 157) (threads 32)
       (map-kernel (frame-shape 5000) () (iota iota.334)
        (body-matcher map-result.339) (map-result (map-result.339))
        (let
         ((map-result.408
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.336 : iota.334)) iota.336)
             (body-matcher map-result.335) (map-result (map-result.335))
             (consumer (values))))))
         (let
          ((reduce-arg.403
            (contiguous-subarray (#0 map-result.408)
             contiguous-subarray-index.356 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
            (body-matcher reduce-arg.369) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.408)
               contiguous-subarray-index.350 (shape 5000) (shape))
              (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
              (+ reduce-arg1.295 reduce-arg2.298))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.334)
         (let
          ((map-result.408
            (kernel (blocks 188) (threads 32)
             (map-kernel (frame-shape 6000) () (iota (iota.336 : iota.334))
              (body-matcher map-result.335) (map-result (map-result.335))
              iota.336))))
          (let
           ((reduce-arg.403
             (contiguous-subarray (#0 map-result.408)
              contiguous-subarray-index.356 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
              (body-matcher reduce-arg.369) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.408)
                  contiguous-subarray-index.350 (shape 6000) (shape))
                 (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
                 (+ reduce-arg1.295 reduce-arg2.298))
                (outer-body (+ reduce-arg1.295 reduce-arg2.298))))))))))
        (body-matcher map-result.339) (map-result (map-result.339))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [6000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (kernel (blocks 188) (threads 32)
       (map-kernel (frame-shape 6000) () (iota iota.334)
        (body-matcher map-result.339) (map-result (map-result.339))
        (let
         ((map-result.408
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.336 : iota.334)) iota.336)
             (body-matcher map-result.335) (map-result (map-result.335))
             (consumer (values))))))
         (let
          ((reduce-arg.403
            (contiguous-subarray (#0 map-result.408)
             contiguous-subarray-index.356 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
            (body-matcher reduce-arg.369) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.408)
               contiguous-subarray-index.350 (shape 5000) (shape))
              (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
              (+ reduce-arg1.295 reduce-arg2.298))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.111)
       (body-matcher map-result.118) (map-result (map-result.118))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.113 : iota.111))
         (body-matcher map-result.120) (map-result (map-result.120))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.115 : iota.113))
           (body-matcher map-result.122) (map-result (map-result.122))
           (+ 1 iota.115)))))))) |}]
;;

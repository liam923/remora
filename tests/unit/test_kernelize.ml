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
     ((+arg1.182 (frame 4 5 6)) (+arg2.184 (frame 7 8 9))
      (+arg1.186 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.151 +arg1.182) (+arg2.152 +arg2.184) (+arg1.157 +arg1.186))
         (+ +arg1.157 (+ +arg1.151 +arg2.152)))
        (body-matcher map-result.156) (map-result (map-result.156))
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
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.358)
        (body-matcher map-result.363) (map-result (map-result.363))
        (let
         ((map-result.432
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.360 : iota.358)) iota.360)
             (body-matcher map-result.359) (map-result (map-result.359))
             (consumer (values))))))
         (let
          ((reduce-arg.427
            (contiguous-subarray (#0 map-result.432)
             contiguous-subarray-index.380 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
            (body-matcher reduce-arg.393) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.432)
               contiguous-subarray-index.374 (shape 10) (shape))
              (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
              (+ reduce-arg1.319 reduce-arg2.322))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.358)
         (let
          ((map-result.432
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.360 : iota.358))
              (body-matcher map-result.359) (map-result (map-result.359))
              iota.360))))
          (let
           ((reduce-arg.427
             (contiguous-subarray (#0 map-result.432)
              contiguous-subarray-index.380 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
              (body-matcher reduce-arg.393) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.432)
                  contiguous-subarray-index.374 (shape 1000000) (shape))
                 (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
                 (+ reduce-arg1.319 reduce-arg2.322))
                (outer-body (+ reduce-arg1.319 reduce-arg2.322))))))))))
        (body-matcher map-result.363) (map-result (map-result.363))
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
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.358)
        (body-matcher map-result.363) (map-result (map-result.363))
        (let
         ((map-result.432
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.360 : iota.358)) iota.360)
             (body-matcher map-result.359) (map-result (map-result.359))
             (consumer (values))))))
         (let
          ((reduce-arg.427
            (contiguous-subarray (#0 map-result.432)
             contiguous-subarray-index.380 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
            (body-matcher reduce-arg.393) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.432)
               contiguous-subarray-index.374 (shape 1000000) (shape))
              (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
              (+ reduce-arg1.319 reduce-arg2.322))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (kernel (blocks 157) (threads 32)
       (map-kernel (frame-shape 5000) () (iota iota.358)
        (body-matcher map-result.363) (map-result (map-result.363))
        (let
         ((map-result.432
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.360 : iota.358)) iota.360)
             (body-matcher map-result.359) (map-result (map-result.359))
             (consumer (values))))))
         (let
          ((reduce-arg.427
            (contiguous-subarray (#0 map-result.432)
             contiguous-subarray-index.380 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
            (body-matcher reduce-arg.393) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.432)
               contiguous-subarray-index.374 (shape 5000) (shape))
              (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
              (+ reduce-arg1.319 reduce-arg2.322))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.358)
         (let
          ((map-result.432
            (kernel (blocks 188) (threads 32)
             (map-kernel (frame-shape 6000) () (iota (iota.360 : iota.358))
              (body-matcher map-result.359) (map-result (map-result.359))
              iota.360))))
          (let
           ((reduce-arg.427
             (contiguous-subarray (#0 map-result.432)
              contiguous-subarray-index.380 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
              (body-matcher reduce-arg.393) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.432)
                  contiguous-subarray-index.374 (shape 6000) (shape))
                 (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
                 (+ reduce-arg1.319 reduce-arg2.322))
                (outer-body (+ reduce-arg1.319 reduce-arg2.322))))))))))
        (body-matcher map-result.363) (map-result (map-result.363))
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
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (kernel (blocks 188) (threads 32)
       (map-kernel (frame-shape 6000) () (iota iota.358)
        (body-matcher map-result.363) (map-result (map-result.363))
        (let
         ((map-result.432
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.360 : iota.358)) iota.360)
             (body-matcher map-result.359) (map-result (map-result.359))
             (consumer (values))))))
         (let
          ((reduce-arg.427
            (contiguous-subarray (#0 map-result.432)
             contiguous-subarray-index.380 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
            (body-matcher reduce-arg.393) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.432)
               contiguous-subarray-index.374 (shape 5000) (shape))
              (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
              (+ reduce-arg1.319 reduce-arg2.322))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.135)
       (body-matcher map-result.142) (map-result (map-result.142))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.137 : iota.135))
         (body-matcher map-result.144) (map-result (map-result.144))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.139 : iota.137))
           (body-matcher map-result.146) (map-result (map-result.146))
           (+ 1 iota.139)))))))) |}]
;;

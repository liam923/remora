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
     ((+arg1.207 (frame 4 5 6)) (+arg2.209 (frame 7 8 9))
      (+arg1.211 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.176 +arg1.207) (+arg2.177 +arg2.209) (+arg1.182 +arg1.211))
         (+ +arg1.182 (+ +arg1.176 +arg2.177)))
        (body-matcher map-result.181) (map-result (map-result.181))
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
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (kernel (blocks 20) (threads 512)
       (map-kernel (frame-shape 1000000) () (iota iota.383)
        (body-matcher map-result.388) (map-result (map-result.388))
        (let
         ((map-result.457
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.385 : iota.383)) iota.385)
             (body-matcher map-result.384) (map-result (map-result.384))
             (consumer (values))))))
         (let
          ((reduce-arg.452
            (contiguous-subarray (#0 map-result.457)
             contiguous-subarray-index.405 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
            (body-matcher reduce-arg.418) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.457)
               contiguous-subarray-index.399 (shape 10) (shape))
              (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
              (+ reduce-arg1.344 reduce-arg2.347))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.383)
         (let
          ((map-result.457
            (kernel (blocks 20) (threads 512)
             (map-kernel (frame-shape 1000000) () (iota (iota.385 : iota.383))
              (body-matcher map-result.384) (map-result (map-result.384))
              iota.385))))
          (let
           ((reduce-arg.452
             (contiguous-subarray (#0 map-result.457)
              contiguous-subarray-index.405 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
              (body-matcher reduce-arg.418) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.457)
                  contiguous-subarray-index.399 (shape 1000000) (shape))
                 (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
                 (+ reduce-arg1.344 reduce-arg2.347))
                (outer-body (+ reduce-arg1.344 reduce-arg2.347))))))))))
        (body-matcher map-result.388) (map-result (map-result.388))
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
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (kernel (blocks 20) (threads 512)
       (map-kernel (frame-shape 1000000) () (iota iota.383)
        (body-matcher map-result.388) (map-result (map-result.388))
        (let
         ((map-result.457
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.385 : iota.383)) iota.385)
             (body-matcher map-result.384) (map-result (map-result.384))
             (consumer (values))))))
         (let
          ((reduce-arg.452
            (contiguous-subarray (#0 map-result.457)
             contiguous-subarray-index.405 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
            (body-matcher reduce-arg.418) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.457)
               contiguous-subarray-index.399 (shape 1000000) (shape))
              (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
              (+ reduce-arg1.344 reduce-arg2.347))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (kernel (blocks 10) (threads 512)
       (map-kernel (frame-shape 5000) () (iota iota.383)
        (body-matcher map-result.388) (map-result (map-result.388))
        (let
         ((map-result.457
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.385 : iota.383)) iota.385)
             (body-matcher map-result.384) (map-result (map-result.384))
             (consumer (values))))))
         (let
          ((reduce-arg.452
            (contiguous-subarray (#0 map-result.457)
             contiguous-subarray-index.405 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
            (body-matcher reduce-arg.418) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.457)
               contiguous-subarray-index.399 (shape 5000) (shape))
              (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
              (+ reduce-arg1.344 reduce-arg2.347))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.383)
         (let
          ((map-result.457
            (kernel (blocks 12) (threads 512)
             (map-kernel (frame-shape 6000) () (iota (iota.385 : iota.383))
              (body-matcher map-result.384) (map-result (map-result.384))
              iota.385))))
          (let
           ((reduce-arg.452
             (contiguous-subarray (#0 map-result.457)
              contiguous-subarray-index.405 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
              (body-matcher reduce-arg.418) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.457)
                  contiguous-subarray-index.399 (shape 6000) (shape))
                 (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
                 (+ reduce-arg1.344 reduce-arg2.347))
                (outer-body (+ reduce-arg1.344 reduce-arg2.347))))))))))
        (body-matcher map-result.388) (map-result (map-result.388))
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
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (kernel (blocks 12) (threads 512)
       (map-kernel (frame-shape 6000) () (iota iota.383)
        (body-matcher map-result.388) (map-result (map-result.388))
        (let
         ((map-result.457
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.385 : iota.383)) iota.385)
             (body-matcher map-result.384) (map-result (map-result.384))
             (consumer (values))))))
         (let
          ((reduce-arg.452
            (contiguous-subarray (#0 map-result.457)
             contiguous-subarray-index.405 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
            (body-matcher reduce-arg.418) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.457)
               contiguous-subarray-index.399 (shape 5000) (shape))
              (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
              (+ reduce-arg1.344 reduce-arg2.347))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 12) (threads 512)
      (map-kernel (frame-shape 10) () (iota iota.160)
       (body-matcher map-result.167) (map-result (map-result.167))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.162 : iota.160))
         (body-matcher map-result.169) (map-result (map-result.169))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.164 : iota.162))
           (body-matcher map-result.171) (map-result (map-result.171))
           (+ 1 iota.164)))))))) |}]
;;

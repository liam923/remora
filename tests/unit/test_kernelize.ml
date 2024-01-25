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
     ((+arg1.191 (frame 4 5 6)) (+arg2.193 (frame 7 8 9))
      (+arg1.195 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.160 +arg1.191) (+arg2.161 +arg2.193) (+arg1.166 +arg1.195))
         (+ +arg1.166 (+ +arg1.160 +arg2.161)))
        (body-matcher map-result.165) (map-result (map-result.165))
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
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.367)
        (body-matcher map-result.372) (map-result (map-result.372))
        (let
         ((map-result.441
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.369 : iota.367)) iota.369)
             (body-matcher map-result.368) (map-result (map-result.368))
             (consumer (values))))))
         (let
          ((reduce-arg.436
            (contiguous-subarray (#0 map-result.441)
             contiguous-subarray-index.389 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
            (body-matcher reduce-arg.402) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.441)
               contiguous-subarray-index.383 (shape 10) (shape))
              (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
              (+ reduce-arg1.328 reduce-arg2.331))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.367)
         (let
          ((map-result.441
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.369 : iota.367))
              (body-matcher map-result.368) (map-result (map-result.368))
              iota.369))))
          (let
           ((reduce-arg.436
             (contiguous-subarray (#0 map-result.441)
              contiguous-subarray-index.389 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
              (body-matcher reduce-arg.402) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.441)
                  contiguous-subarray-index.383 (shape 1000000) (shape))
                 (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
                 (+ reduce-arg1.328 reduce-arg2.331))
                (outer-body (+ reduce-arg1.328 reduce-arg2.331))))))))))
        (body-matcher map-result.372) (map-result (map-result.372))
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
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.367)
        (body-matcher map-result.372) (map-result (map-result.372))
        (let
         ((map-result.441
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.369 : iota.367)) iota.369)
             (body-matcher map-result.368) (map-result (map-result.368))
             (consumer (values))))))
         (let
          ((reduce-arg.436
            (contiguous-subarray (#0 map-result.441)
             contiguous-subarray-index.389 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
            (body-matcher reduce-arg.402) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.441)
               contiguous-subarray-index.383 (shape 1000000) (shape))
              (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
              (+ reduce-arg1.328 reduce-arg2.331))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (kernel (blocks 157) (threads 32)
       (map-kernel (frame-shape 5000) () (iota iota.367)
        (body-matcher map-result.372) (map-result (map-result.372))
        (let
         ((map-result.441
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.369 : iota.367)) iota.369)
             (body-matcher map-result.368) (map-result (map-result.368))
             (consumer (values))))))
         (let
          ((reduce-arg.436
            (contiguous-subarray (#0 map-result.441)
             contiguous-subarray-index.389 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
            (body-matcher reduce-arg.402) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.441)
               contiguous-subarray-index.383 (shape 5000) (shape))
              (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
              (+ reduce-arg1.328 reduce-arg2.331))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.367)
         (let
          ((map-result.441
            (kernel (blocks 188) (threads 32)
             (map-kernel (frame-shape 6000) () (iota (iota.369 : iota.367))
              (body-matcher map-result.368) (map-result (map-result.368))
              iota.369))))
          (let
           ((reduce-arg.436
             (contiguous-subarray (#0 map-result.441)
              contiguous-subarray-index.389 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
              (body-matcher reduce-arg.402) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.441)
                  contiguous-subarray-index.383 (shape 6000) (shape))
                 (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
                 (+ reduce-arg1.328 reduce-arg2.331))
                (outer-body (+ reduce-arg1.328 reduce-arg2.331))))))))))
        (body-matcher map-result.372) (map-result (map-result.372))
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
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (kernel (blocks 188) (threads 32)
       (map-kernel (frame-shape 6000) () (iota iota.367)
        (body-matcher map-result.372) (map-result (map-result.372))
        (let
         ((map-result.441
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.369 : iota.367)) iota.369)
             (body-matcher map-result.368) (map-result (map-result.368))
             (consumer (values))))))
         (let
          ((reduce-arg.436
            (contiguous-subarray (#0 map-result.441)
             contiguous-subarray-index.389 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
            (body-matcher reduce-arg.402) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.441)
               contiguous-subarray-index.383 (shape 5000) (shape))
              (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
              (+ reduce-arg1.328 reduce-arg2.331))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.144)
       (body-matcher map-result.151) (map-result (map-result.151))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.146 : iota.144))
         (body-matcher map-result.153) (map-result (map-result.153))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.148 : iota.146))
           (body-matcher map-result.155) (map-result (map-result.155))
           (+ 1 iota.148)))))))) |}]
;;

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
     ((+arg1.151 (frame 4 5 6)) (+arg2.153 (frame 7 8 9))
      (+arg1.155 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.120 +arg1.151) (+arg2.121 +arg2.153) (+arg1.126 +arg1.155))
         (+ +arg1.126 (+ +arg1.120 +arg2.121)))
        (body-matcher map-result.125) (map-result (map-result.125))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.148)
        (body-matcher map-result.153) (map-result (map-result.153))
        (let
         ((map-result.206
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.150 : iota.148)) iota.150)
             (body-matcher map-result.149) (map-result (map-result.149))
             (consumer (values))))))
         (let
          ((reduce-arg.201
            (contiguous-subarray (#0 map-result.206)
             contiguous-subarray-index.170 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
            (body-matcher reduce-arg.172) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.206)
               contiguous-subarray-index.164 (shape 10) (shape))
              (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
              (+ reduce-arg1.136 reduce-arg2.137))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.148)
         (let
          ((map-result.206
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.150 : iota.148))
              (body-matcher map-result.149) (map-result (map-result.149))
              iota.150))))
          (let
           ((reduce-arg.201
             (contiguous-subarray (#0 map-result.206)
              contiguous-subarray-index.170 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
              (body-matcher reduce-arg.172) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.206)
                  contiguous-subarray-index.164 (shape 1000000) (shape))
                 (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
                 (+ reduce-arg1.136 reduce-arg2.137))
                (outer-body (+ reduce-arg1.136 reduce-arg2.137))))))))))
        (body-matcher map-result.153) (map-result (map-result.153))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [1000000 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.148)
        (body-matcher map-result.153) (map-result (map-result.153))
        (let
         ((map-result.206
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.150 : iota.148)) iota.150)
             (body-matcher map-result.149) (map-result (map-result.149))
             (consumer (values))))))
         (let
          ((reduce-arg.201
            (contiguous-subarray (#0 map-result.206)
             contiguous-subarray-index.170 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
            (body-matcher reduce-arg.172) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.206)
               contiguous-subarray-index.164 (shape 1000000) (shape))
              (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
              (+ reduce-arg1.136 reduce-arg2.137))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (kernel (blocks 157) (threads 32)
       (map-kernel (frame-shape 5000) () (iota iota.148)
        (body-matcher map-result.153) (map-result (map-result.153))
        (let
         ((map-result.206
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.150 : iota.148)) iota.150)
             (body-matcher map-result.149) (map-result (map-result.149))
             (consumer (values))))))
         (let
          ((reduce-arg.201
            (contiguous-subarray (#0 map-result.206)
             contiguous-subarray-index.170 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
            (body-matcher reduce-arg.172) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.206)
               contiguous-subarray-index.164 (shape 5000) (shape))
              (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
              (+ reduce-arg1.136 reduce-arg2.137))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.148)
         (let
          ((map-result.206
            (kernel (blocks 188) (threads 32)
             (map-kernel (frame-shape 6000) () (iota (iota.150 : iota.148))
              (body-matcher map-result.149) (map-result (map-result.149))
              iota.150))))
          (let
           ((reduce-arg.201
             (contiguous-subarray (#0 map-result.206)
              contiguous-subarray-index.170 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
              (body-matcher reduce-arg.172) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.206)
                  contiguous-subarray-index.164 (shape 6000) (shape))
                 (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
                 (+ reduce-arg1.136 reduce-arg2.137))
                (outer-body (+ reduce-arg1.136 reduce-arg2.137))))))))))
        (body-matcher map-result.153) (map-result (map-result.153))
        (consumer (values)))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 4999} iota{ | [6000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (kernel (blocks 188) (threads 32)
       (map-kernel (frame-shape 6000) () (iota iota.148)
        (body-matcher map-result.153) (map-result (map-result.153))
        (let
         ((map-result.206
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.150 : iota.148)) iota.150)
             (body-matcher map-result.149) (map-result (map-result.149))
             (consumer (values))))))
         (let
          ((reduce-arg.201
            (contiguous-subarray (#0 map-result.206)
             contiguous-subarray-index.170 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
            (body-matcher reduce-arg.172) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.206)
               contiguous-subarray-index.164 (shape 5000) (shape))
              (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
              (+ reduce-arg1.136 reduce-arg2.137))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.104)
       (body-matcher map-result.111) (map-result (map-result.111))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.106 : iota.104))
         (body-matcher map-result.113) (map-result (map-result.113))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.108 : iota.106))
           (body-matcher map-result.115) (map-result (map-result.115))
           (+ 1 iota.108)))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce-non-assoc{int | d-1 []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.164 (frame 0))
      (contiguous-subarray-index.170 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.148)
         (let
          ((map-result.206
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.150 : iota.148))
              (body-matcher map-result.149) (map-result (map-result.149))
              iota.150))))
          (let
           ((reduce-arg.201
             (contiguous-subarray (#0 map-result.206)
              contiguous-subarray-index.170 (shape 1000000) (shape 999999))))
           (#1
            (loop-block (frame-shape 999999)
             (map ((reduce-arg.174 reduce-arg.201)) reduce-arg.174)
             (body-matcher reduce-arg.172) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.206)
                contiguous-subarray-index.164 (shape 1000000) (shape))
               (reduce-arg1.136 reduce-arg2.137 reduce-arg.172)
               (+ reduce-arg1.136 reduce-arg2.137))))))))
        (body-matcher map-result.153) (map-result (map-result.153))
        (consumer (values)))))) |}]
;;

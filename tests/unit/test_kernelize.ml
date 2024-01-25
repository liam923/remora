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
     ((+arg1.170 (frame 4 5 6)) (+arg2.172 (frame 7 8 9))
      (+arg1.174 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((+arg1.139 +arg1.170) (+arg2.140 +arg2.172) (+arg1.145 +arg1.174))
         (+ +arg1.145 (+ +arg1.139 +arg2.140)))
        (body-matcher map-result.144) (map-result (map-result.144))
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
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.346)
        (body-matcher map-result.351) (map-result (map-result.351))
        (let
         ((map-result.420
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.348 : iota.346)) iota.348)
             (body-matcher map-result.347) (map-result (map-result.347))
             (consumer (values))))))
         (let
          ((reduce-arg.415
            (contiguous-subarray (#0 map-result.420)
             contiguous-subarray-index.368 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
            (body-matcher reduce-arg.381) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.420)
               contiguous-subarray-index.362 (shape 10) (shape))
              (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
              (+ reduce-arg1.307 reduce-arg2.310))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 999999} iota{ | [10 1000000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 10)
        (map () (iota iota.346)
         (let
          ((map-result.420
            (kernel (blocks 320) (threads 32)
             (map-kernel (frame-shape 1000000) () (iota (iota.348 : iota.346))
              (body-matcher map-result.347) (map-result (map-result.347))
              iota.348))))
          (let
           ((reduce-arg.415
             (contiguous-subarray (#0 map-result.420)
              contiguous-subarray-index.368 (shape 1000000) (shape 999999))))
           (#1
            (kernel (blocks 320) (threads 32)
             (loop-kernel (frame-shape 999999)
              (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
              (body-matcher reduce-arg.381) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.420)
                  contiguous-subarray-index.362 (shape 1000000) (shape))
                 (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
                 (+ reduce-arg1.307 reduce-arg2.310))
                (outer-body (+ reduce-arg1.307 reduce-arg2.310))))))))))
        (body-matcher map-result.351) (map-result (map-result.351))
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
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.346)
        (body-matcher map-result.351) (map-result (map-result.351))
        (let
         ((map-result.420
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota (iota.348 : iota.346)) iota.348)
             (body-matcher map-result.347) (map-result (map-result.347))
             (consumer (values))))))
         (let
          ((reduce-arg.415
            (contiguous-subarray (#0 map-result.420)
             contiguous-subarray-index.368 (shape 1000000) (shape 999999))))
          (#1
           (loop-block (frame-shape 999999)
            (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
            (body-matcher reduce-arg.381) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.420)
               contiguous-subarray-index.362 (shape 1000000) (shape))
              (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
              (+ reduce-arg1.307 reduce-arg2.310))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 4999} iota{ | [5000 5000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (kernel (blocks 157) (threads 32)
       (map-kernel (frame-shape 5000) () (iota iota.346)
        (body-matcher map-result.351) (map-result (map-result.351))
        (let
         ((map-result.420
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.348 : iota.346)) iota.348)
             (body-matcher map-result.347) (map-result (map-result.347))
             (consumer (values))))))
         (let
          ((reduce-arg.415
            (contiguous-subarray (#0 map-result.420)
             contiguous-subarray-index.368 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
            (body-matcher reduce-arg.381) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.420)
               contiguous-subarray-index.362 (shape 5000) (shape))
              (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
              (+ reduce-arg1.307 reduce-arg2.310))))))))))) |}];
  kernelizeAndPrint
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 5999} iota{ | [5000 6000]})
    |};
  [%expect
    {|
    (let
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 5000)
        (map () (iota iota.346)
         (let
          ((map-result.420
            (kernel (blocks 188) (threads 32)
             (map-kernel (frame-shape 6000) () (iota (iota.348 : iota.346))
              (body-matcher map-result.347) (map-result (map-result.347))
              iota.348))))
          (let
           ((reduce-arg.415
             (contiguous-subarray (#0 map-result.420)
              contiguous-subarray-index.368 (shape 6000) (shape 5999))))
           (#1
            (kernel (blocks 188) (threads 32)
             (loop-kernel (frame-shape 5999)
              (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
              (body-matcher reduce-arg.381) (map-result ())
              (consumer
               ((reduce-zero
                 (contiguous-subarray (#0 map-result.420)
                  contiguous-subarray-index.362 (shape 6000) (shape))
                 (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
                 (+ reduce-arg1.307 reduce-arg2.310))
                (outer-body (+ reduce-arg1.307 reduce-arg2.310))))))))))
        (body-matcher map-result.351) (map-result (map-result.351))
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
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (kernel (blocks 188) (threads 32)
       (map-kernel (frame-shape 6000) () (iota iota.346)
        (body-matcher map-result.351) (map-result (map-result.351))
        (let
         ((map-result.420
           (#0
            (loop-block (frame-shape 5000)
             (map () (iota (iota.348 : iota.346)) iota.348)
             (body-matcher map-result.347) (map-result (map-result.347))
             (consumer (values))))))
         (let
          ((reduce-arg.415
            (contiguous-subarray (#0 map-result.420)
             contiguous-subarray-index.368 (shape 5000) (shape 4999))))
          (#1
           (loop-block (frame-shape 4999)
            (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
            (body-matcher reduce-arg.381) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.420)
               contiguous-subarray-index.362 (shape 5000) (shape))
              (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
              (+ reduce-arg1.307 reduce-arg2.310))))))))))) |}];
  kernelizeAndPrint "(+ 1 iota{ | [10 20 30]})";
  [%expect
    {|
    (#0
     (kernel (blocks 188) (threads 32)
      (map-kernel (frame-shape 10) () (iota iota.123)
       (body-matcher map-result.130) (map-result (map-result.130))
       (#0
        (map-kernel (frame-shape 20) () (iota (iota.125 : iota.123))
         (body-matcher map-result.132) (map-result (map-result.132))
         (#0
          (map-kernel (frame-shape 30) () (iota (iota.127 : iota.125))
           (body-matcher map-result.134) (map-result (map-result.134))
           (+ 1 iota.127)))))))) |}]
;;

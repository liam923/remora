open! Base
open Remora

let%expect_test "check simplifying" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
  checkAndPrint {| 5 |};
  [%expect {|
    5 |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect {|
    3 |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (let ((+arg1.159 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.161 +arg1.159)) (+ +arg1.161 4))
        (body-matcher map-result.160) (map-result (map-result.160))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (t-fn (@t) "hello"){int| }
  |};
  [%expect {|
    (frame 'h' 'e' 'l' 'l' 'o') |}];
  checkAndPrint {|
    (define (id{@t| } [x @t]) x)
    (id{int| } 5)
  |};
  [%expect {|
    5 |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      (id{(Forall (@t) (-> (@t) @t))| } id)
    |};
  [%expect {|
    (values) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app (id{(Forall (@t) (-> (@t) @t))| } id) int) 5)
    |};
  [%expect {|
    5 |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect {|
    10 |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect {|
    5 |}];
  checkAndPrint {| 
    (reduce{int | 4 [] []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (let ((arr.322 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.354
        (contiguous-subarray arr.322 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.355 reduce-arg.354)) (values reduce-arg.355))
        (body-matcher (reduce-arg.348)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.322 (frame 0) (shape 5) (shape))
          (reduce-arg1.284 reduce-arg2.287 reduce-arg.348)
          (+ reduce-arg1.284 reduce-arg2.287))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.164)
       (map-result (map-result.164)) (consumer (values))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      (+
        (length{int | 5 []} [1 2 3 4 5])
        (length{char | 2 [2]} ["hi" "ih"]))
    |};
  [%expect {|
    7 |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.204 (frame 5 6 7)) (+arg1.210 (frame 1 2 3)))
     (let
      ((y.213
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.212 +arg1.210)) (+ +arg1.212 4))
           (body-matcher map-result.211) (map-result (map-result.211))
           (consumer (values)))))))
      (let ((+arg1.219 y.213) (+arg2.220 y.213))
       (let
        ((+arg2.225
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.222 +arg1.219) (+arg2.223 +arg2.220))
              (+ +arg1.222 +arg2.223))
             (body-matcher map-result.221) (map-result (map-result.221))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.206 x.204))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.227 +arg2.225)) (+ x.206 +arg2.227))
               (body-matcher map-result.226) (map-result (map-result.226))
               (consumer (values))))))
           (body-matcher map-result.205) (map-result (map-result.205))
           (consumer (values))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x y))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.189 (frame 5 6 7)) (+arg1.195 (frame 1 2 3)))
     (let
      ((+arg2.202
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.197 +arg1.195)) (+ +arg1.197 4))
           (body-matcher map-result.196) (map-result (map-result.196))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.191 x.189))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.204 +arg2.202)) (+ x.191 +arg2.204))
             (body-matcher map-result.203) (map-result (map-result.203))
             (consumer (values))))))
         (body-matcher map-result.190) (map-result (map-result.190))
         (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo 5)
    |};
  [%expect
    {|
    (let ((+arg1.205 (frame 1 2 3)))
     (let
      ((y.208
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.207 +arg1.205)) (+ +arg1.207 4))
           (body-matcher map-result.206) (map-result (map-result.206))
           (consumer (values)))))))
      (let ((+arg1.214 y.208) (+arg2.215 y.208))
       (let
        ((+arg2.220
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.217 +arg1.214) (+arg2.218 +arg2.215))
              (+ +arg1.217 +arg2.218))
             (body-matcher map-result.216) (map-result (map-result.216))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.222 +arg2.220)) (+ 5 +arg2.222))
           (body-matcher map-result.221) (map-result (map-result.221))
           (consumer (values))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] x))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.204 (frame 5 6 7)) (+arg1.210 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.206 x.204))
         (let
          ((y.213
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.212 +arg1.210)) (+ +arg1.212 x.206))
               (body-matcher map-result.211) (map-result (map-result.211))
               (consumer (values)))))))
          (let ((+arg1.219 y.213) (+arg2.220 y.213))
           (let
            ((+arg2.225
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.222 +arg1.219) (+arg2.223 +arg2.220))
                  (+ +arg1.222 +arg2.223))
                 (body-matcher map-result.221) (map-result (map-result.221))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.227 +arg2.225)) (+ x.206 +arg2.227))
               (body-matcher map-result.226) (map-result (map-result.226))
               (consumer (values)))))))))
        (body-matcher map-result.205) (map-result (map-result.205))
        (consumer (values)))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ 3 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (let ((x.192 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.194 x.192)) (+ x.194 14))
        (body-matcher map-result.193) (map-result (map-result.193))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.161 (frame 3 4)) (+arg2.162 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.164 +arg1.161) (+arg2.165 +arg2.162))
          (+ +arg1.164 +arg2.165))
         (body-matcher map-result.163) (map-result (map-result.163))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.181 (frame 1 2)) (+arg2.182 (frame 3 4)) (+arg1.189 (frame 1 2))
      (+arg2.190 (frame 3 4)) (+arg1.197 (frame 1 2)) (+arg2.198 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.184 +arg1.181) (+arg2.185 +arg2.182))
           (+ +arg1.184 +arg2.185))
          (body-matcher map-result.183) (map-result (map-result.183))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.192 +arg1.189) (+arg2.193 +arg2.190))
           (+ +arg1.192 +arg2.193))
          (body-matcher map-result.191) (map-result (map-result.191))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.200 +arg1.197) (+arg2.201 +arg2.198))
           (+ +arg1.200 +arg2.201))
          (body-matcher map-result.199) (map-result (map-result.199))
          (consumer (values))))))
      (frame (frame 4 5) (frame 6 7) (frame 8 9)))) |}];
  checkAndPrint {| [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]] |};
  [%expect
    {|
    (frame (frame (frame 1 2) (frame 3 4) (frame 5 6))
     (frame (frame 7 8) (frame 9 10) (frame 11 12))) |}];
  checkAndPrint "(append{int | 3 2 []} [1 2 3] [4 5])";
  [%expect {| (frame 1 2 3 4 5) |}];
  checkAndPrint "(append{int | 3 2 [1]} [[1] [2] [3]] [[4] [5]])";
  [%expect {| (frame (frame 1) (frame 2) (frame 3) (frame 4) (frame 5)) |}];
  checkAndPrint "[[1 1] [2 2] (+ [2 2] 1)]";
  [%expect
    {|
    (let ((+arg1.159 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.161 +arg1.159)) (+ +arg1.161 1))
         (body-matcher map-result.160) (map-result (map-result.160))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.166 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.168 +arg1.166)) (+ +arg1.168 1))
         (body-matcher map-result.167) (map-result (map-result.167))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.150)
        (let ((iota-offset.154 (* iota.150 2)))
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota iota.152)
             (let ((iota-offset.158 (* (+ iota-offset.154 iota.152) 3)))
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map () (iota iota.156) (+ iota-offset.158 iota.156))
                 (body-matcher map-result.155) (map-result (map-result.155))
                 (consumer (values)))))))
            (body-matcher map-result.151) (map-result (map-result.151))
            (consumer (values)))))))
       (body-matcher map-result.149) (map-result (map-result.149))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.169)
         (let ((iota-offset.173 (* iota.169 2)))
          (#0
           (#0
            (loop-block (frame-shape 2)
             (map () (iota iota.171)
              (let ((iota-offset.177 (* (+ iota-offset.173 iota.171) 3)))
               (#0
                (#0
                 (loop-block (frame-shape 3)
                  (map () (iota iota.175)
                   (let ((iota-offset.181 (* (+ iota-offset.177 iota.175) 4)))
                    (#0
                     (#0
                      (loop-block (frame-shape 4)
                       (map () (iota iota.179)
                        (let
                         ((iota-offset.185 (* (+ iota-offset.181 iota.179) 5)))
                         (#0
                          (#0
                           (loop-block (frame-shape 5)
                            (map () (iota iota.183) (+ iota-offset.185 iota.183))
                            (body-matcher map-result.182)
                            (map-result (map-result.182)) (consumer (values)))))))
                       (body-matcher map-result.178)
                       (map-result (map-result.178)) (consumer (values)))))))
                  (body-matcher map-result.174) (map-result (map-result.174))
                  (consumer (values)))))))
             (body-matcher map-result.170) (map-result (map-result.170))
             (consumer (values)))))))
        (body-matcher map-result.168) (map-result (map-result.168))
        (consumer (values)))))
     (frame 0 1 0) (shape 1 2 3) (shape)) |}];
  checkAndPrint
    {|
    (define (foo [x int])
      (+ [1 2 3] 4))
    (foo (array [0] int))
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 0)
       (map ()
        (let ((+arg1.175 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.177 +arg1.175)) (+ +arg1.177 4))
            (body-matcher map-result.176) (map-result (map-result.176))
            (consumer (values)))))))
       (body-matcher map-result.171) (map-result (map-result.171))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.174 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.176 index-value.174))
         (index-let ((i.148 runtime-value index-value.176))
          (box (i.148)
           (#0
            (#0
             (loop-block (frame-shape i.148) (map () 5)
              (body-matcher map-result.182) (map-result (map-result.182))
              (consumer (values))))))))
        (body-matcher map-result.175) (map-result (map-result.175))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.148 runtime-value (frame 1 2 3)))
     (box ((shape @i.148))
      (#0
       (#0
        (loop-block (frame-shape @i.148) (map () 5) (body-matcher map-result.180)
         (map-result (map-result.180)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.333 (frame 1 2 3))
      (+arg2.373
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.371) iota.371)
          (body-matcher map-result.370) (map-result (map-result.370))
          (consumer (values)))))))
     (let
      ((reduce-arg.365
        (contiguous-subarray arr.333 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.367
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.366 reduce-arg.365)) (values reduce-arg.366))
           (body-matcher (reduce-arg.359)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.333 (frame 0) (shape 3) (shape))
             (reduce-arg1.290 reduce-arg2.293 reduce-arg.359)
             (+ reduce-arg1.290 reduce-arg2.293)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.375 +arg2.373)) (+ x.367 +arg2.375))
          (body-matcher map-result.374) (map-result (map-result.374))
          (consumer (values)))))))) |}]
;;

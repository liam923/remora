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
    (let ((+arg1.122 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.124 +arg1.122)) (+ +arg1.124 4))
        (body-matcher map-result.123) (map-result (map-result.123))
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
    (let ((arr.285 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.317
        (contiguous-subarray arr.285 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.318 reduce-arg.317)) (values reduce-arg.318))
        (body-matcher (reduce-arg.311)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.285 (frame 0) (shape 5) (shape))
          (reduce-arg1.247 reduce-arg2.250 reduce-arg.311)
          (+ reduce-arg1.247 reduce-arg2.250))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.127)
       (map-result (map-result.127)) (consumer (values))))) |}];
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
    (let ((x.167 (frame 5 6 7)) (+arg1.173 (frame 1 2 3)))
     (let
      ((y.176
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.175 +arg1.173)) (+ +arg1.175 4))
           (body-matcher map-result.174) (map-result (map-result.174))
           (consumer (values)))))))
      (let ((+arg1.182 y.176) (+arg2.183 y.176))
       (let
        ((+arg2.188
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.185 +arg1.182) (+arg2.186 +arg2.183))
              (+ +arg1.185 +arg2.186))
             (body-matcher map-result.184) (map-result (map-result.184))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.169 x.167))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.190 +arg2.188)) (+ x.169 +arg2.190))
               (body-matcher map-result.189) (map-result (map-result.189))
               (consumer (values))))))
           (body-matcher map-result.168) (map-result (map-result.168))
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
    (let ((x.152 (frame 5 6 7)) (+arg1.158 (frame 1 2 3)))
     (let
      ((+arg2.165
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.160 +arg1.158)) (+ +arg1.160 4))
           (body-matcher map-result.159) (map-result (map-result.159))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.154 x.152))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.167 +arg2.165)) (+ x.154 +arg2.167))
             (body-matcher map-result.166) (map-result (map-result.166))
             (consumer (values))))))
         (body-matcher map-result.153) (map-result (map-result.153))
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
    (let ((+arg1.168 (frame 1 2 3)))
     (let
      ((y.171
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.170 +arg1.168)) (+ +arg1.170 4))
           (body-matcher map-result.169) (map-result (map-result.169))
           (consumer (values)))))))
      (let ((+arg1.177 y.171) (+arg2.178 y.171))
       (let
        ((+arg2.183
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.180 +arg1.177) (+arg2.181 +arg2.178))
              (+ +arg1.180 +arg2.181))
             (body-matcher map-result.179) (map-result (map-result.179))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.185 +arg2.183)) (+ 5 +arg2.185))
           (body-matcher map-result.184) (map-result (map-result.184))
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
    (let ((x.167 (frame 5 6 7)) (+arg1.173 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.169 x.167))
         (let
          ((y.176
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.175 +arg1.173)) (+ +arg1.175 x.169))
               (body-matcher map-result.174) (map-result (map-result.174))
               (consumer (values)))))))
          (let ((+arg1.182 y.176) (+arg2.183 y.176))
           (let
            ((+arg2.188
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.185 +arg1.182) (+arg2.186 +arg2.183))
                  (+ +arg1.185 +arg2.186))
                 (body-matcher map-result.184) (map-result (map-result.184))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.190 +arg2.188)) (+ x.169 +arg2.190))
               (body-matcher map-result.189) (map-result (map-result.189))
               (consumer (values)))))))))
        (body-matcher map-result.168) (map-result (map-result.168))
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
    (let ((x.155 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.157 x.155)) (+ x.157 14))
        (body-matcher map-result.156) (map-result (map-result.156))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.124 (frame 3 4)) (+arg2.125 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.127 +arg1.124) (+arg2.128 +arg2.125))
          (+ +arg1.127 +arg2.128))
         (body-matcher map-result.126) (map-result (map-result.126))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.144 (frame 1 2)) (+arg2.145 (frame 3 4)) (+arg1.152 (frame 1 2))
      (+arg2.153 (frame 3 4)) (+arg1.160 (frame 1 2)) (+arg2.161 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.147 +arg1.144) (+arg2.148 +arg2.145))
           (+ +arg1.147 +arg2.148))
          (body-matcher map-result.146) (map-result (map-result.146))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.155 +arg1.152) (+arg2.156 +arg2.153))
           (+ +arg1.155 +arg2.156))
          (body-matcher map-result.154) (map-result (map-result.154))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.163 +arg1.160) (+arg2.164 +arg2.161))
           (+ +arg1.163 +arg2.164))
          (body-matcher map-result.162) (map-result (map-result.162))
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
    (let ((+arg1.122 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.124 +arg1.122)) (+ +arg1.124 1))
         (body-matcher map-result.123) (map-result (map-result.123))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.129 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.131 +arg1.129)) (+ +arg1.131 1))
         (body-matcher map-result.130) (map-result (map-result.130))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.113)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.115 : iota.113))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.117 : iota.115)) iota.117)
               (body-matcher map-result.116) (map-result (map-result.116))
               (consumer (values))))))
           (body-matcher map-result.114) (map-result (map-result.114))
           (consumer (values))))))
       (body-matcher map-result.112) (map-result (map-result.112))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.132)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.134 : iota.132))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.136 : iota.134))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.138 : iota.136))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.140 : iota.138)) iota.140)
                        (body-matcher map-result.139)
                        (map-result (map-result.139)) (consumer (values))))))
                    (body-matcher map-result.137) (map-result (map-result.137))
                    (consumer (values))))))
                (body-matcher map-result.135) (map-result (map-result.135))
                (consumer (values))))))
            (body-matcher map-result.133) (map-result (map-result.133))
            (consumer (values))))))
        (body-matcher map-result.131) (map-result (map-result.131))
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
        (let ((+arg1.138 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.140 +arg1.138)) (+ +arg1.140 4))
            (body-matcher map-result.139) (map-result (map-result.139))
            (consumer (values)))))))
       (body-matcher map-result.134) (map-result (map-result.134))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.137 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.139 index-value.137))
         (index-let ((i.111 runtime-value index-value.139))
          (box (i.111)
           (#0
            (#0
             (loop-block (frame-shape i.111) (map () 5)
              (body-matcher map-result.145) (map-result (map-result.145))
              (consumer (values))))))))
        (body-matcher map-result.138) (map-result (map-result.138))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.111 runtime-value (frame 1 2 3)))
     (box ((shape @i.111))
      (#0
       (#0
        (loop-block (frame-shape @i.111) (map () 5) (body-matcher map-result.143)
         (map-result (map-result.143)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.296 (frame 1 2 3))
      (+arg2.336
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.334) iota.334)
          (body-matcher map-result.333) (map-result (map-result.333))
          (consumer (values)))))))
     (let
      ((reduce-arg.328
        (contiguous-subarray arr.296 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.330
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.329 reduce-arg.328)) (values reduce-arg.329))
           (body-matcher (reduce-arg.322)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.296 (frame 0) (shape 3) (shape))
             (reduce-arg1.253 reduce-arg2.256 reduce-arg.322)
             (+ reduce-arg1.253 reduce-arg2.256)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.338 +arg2.336)) (+ x.330 +arg2.338))
          (body-matcher map-result.337) (map-result (map-result.337))
          (consumer (values)))))))) |}]
;;

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
    (let ((+arg1.103 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.105 +arg1.103)) (+ +arg1.105 4))
        (body-matcher map-result.104) (map-result (map-result.104))
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
    (reduce{int | 4 []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (let ((arr.135 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.151
        (contiguous-subarray arr.135 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.152 reduce-arg.151)) (values reduce-arg.152))
        (body-matcher (reduce-arg.150)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.135 (frame 0) (shape 5) (shape))
          (reduce-arg1.124 reduce-arg2.125 reduce-arg.150)
          (+ reduce-arg1.124 reduce-arg2.125))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.107)
       (map-result (map-result.107)) (consumer (values))))) |}];
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
    (let ((x.133 (frame 5 6 7)) (+arg1.139 (frame 1 2 3)))
     (let
      ((y.142
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.141 +arg1.139)) (+ +arg1.141 4))
           (body-matcher map-result.140) (map-result (map-result.140))
           (consumer (values)))))))
      (let ((+arg1.148 y.142) (+arg2.149 y.142))
       (let
        ((+arg2.154
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.151 +arg1.148) (+arg2.152 +arg2.149))
              (+ +arg1.151 +arg2.152))
             (body-matcher map-result.150) (map-result (map-result.150))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.135 x.133))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.156 +arg2.154)) (+ x.135 +arg2.156))
               (body-matcher map-result.155) (map-result (map-result.155))
               (consumer (values))))))
           (body-matcher map-result.134) (map-result (map-result.134))
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
    (let ((x.123 (frame 5 6 7)) (+arg1.129 (frame 1 2 3)))
     (let
      ((+arg2.136
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.131 +arg1.129)) (+ +arg1.131 4))
           (body-matcher map-result.130) (map-result (map-result.130))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.125 x.123))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.138 +arg2.136)) (+ x.125 +arg2.138))
             (body-matcher map-result.137) (map-result (map-result.137))
             (consumer (values))))))
         (body-matcher map-result.124) (map-result (map-result.124))
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
    (let ((+arg1.134 (frame 1 2 3)))
     (let
      ((y.137
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.136 +arg1.134)) (+ +arg1.136 4))
           (body-matcher map-result.135) (map-result (map-result.135))
           (consumer (values)))))))
      (let ((+arg1.143 y.137) (+arg2.144 y.137))
       (let
        ((+arg2.149
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.146 +arg1.143) (+arg2.147 +arg2.144))
              (+ +arg1.146 +arg2.147))
             (body-matcher map-result.145) (map-result (map-result.145))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.151 +arg2.149)) (+ 5 +arg2.151))
           (body-matcher map-result.150) (map-result (map-result.150))
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
    (let ((x.133 (frame 5 6 7)) (+arg1.139 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.135 x.133))
         (let
          ((y.142
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.141 +arg1.139)) (+ +arg1.141 x.135))
               (body-matcher map-result.140) (map-result (map-result.140))
               (consumer (values)))))))
          (let ((+arg1.148 y.142) (+arg2.149 y.142))
           (let
            ((+arg2.154
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.151 +arg1.148) (+arg2.152 +arg2.149))
                  (+ +arg1.151 +arg2.152))
                 (body-matcher map-result.150) (map-result (map-result.150))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.156 +arg2.154)) (+ x.135 +arg2.156))
               (body-matcher map-result.155) (map-result (map-result.155))
               (consumer (values)))))))))
        (body-matcher map-result.134) (map-result (map-result.134))
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
    (let ((x.125 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.127 x.125)) (+ x.127 14))
        (body-matcher map-result.126) (map-result (map-result.126))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.105 (frame 3 4)) (+arg2.106 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.108 +arg1.105) (+arg2.109 +arg2.106))
          (+ +arg1.108 +arg2.109))
         (body-matcher map-result.107) (map-result (map-result.107))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.125 (frame 1 2)) (+arg2.126 (frame 3 4)) (+arg1.133 (frame 1 2))
      (+arg2.134 (frame 3 4)) (+arg1.141 (frame 1 2)) (+arg2.142 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.128 +arg1.125) (+arg2.129 +arg2.126))
           (+ +arg1.128 +arg2.129))
          (body-matcher map-result.127) (map-result (map-result.127))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.136 +arg1.133) (+arg2.137 +arg2.134))
           (+ +arg1.136 +arg2.137))
          (body-matcher map-result.135) (map-result (map-result.135))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.144 +arg1.141) (+arg2.145 +arg2.142))
           (+ +arg1.144 +arg2.145))
          (body-matcher map-result.143) (map-result (map-result.143))
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
    (let ((+arg1.103 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.105 +arg1.103)) (+ +arg1.105 1))
         (body-matcher map-result.104) (map-result (map-result.104))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.110 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.112 +arg1.110)) (+ +arg1.112 1))
         (body-matcher map-result.111) (map-result (map-result.111))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.94)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.96 : iota.94))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.98 : iota.96)) iota.98)
               (body-matcher map-result.97) (map-result (map-result.97))
               (consumer (values))))))
           (body-matcher map-result.95) (map-result (map-result.95))
           (consumer (values))))))
       (body-matcher map-result.93) (map-result (map-result.93))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.108)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.110 : iota.108))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.112 : iota.110))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.114 : iota.112))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.116 : iota.114)) iota.116)
                        (body-matcher map-result.115)
                        (map-result (map-result.115)) (consumer (values))))))
                    (body-matcher map-result.113) (map-result (map-result.113))
                    (consumer (values))))))
                (body-matcher map-result.111) (map-result (map-result.111))
                (consumer (values))))))
            (body-matcher map-result.109) (map-result (map-result.109))
            (consumer (values))))))
        (body-matcher map-result.107) (map-result (map-result.107))
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
        (let ((+arg1.114 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.116 +arg1.114)) (+ +arg1.116 4))
            (body-matcher map-result.115) (map-result (map-result.115))
            (consumer (values)))))))
       (body-matcher map-result.110) (map-result (map-result.110))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.100 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.102 index-value.100))
         (index-let ((i.92 runtime-value index-value.102))
          (box (i.92)
           (#0
            (#0
             (loop-block (frame-shape i.92) (map () 5)
              (body-matcher map-result.106) (map-result (map-result.106))
              (consumer (values))))))))
        (body-matcher map-result.101) (map-result (map-result.101))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.92 runtime-value (frame 1 2 3)))
     (box ((shape @i.92))
      (#0
       (#0
        (loop-block (frame-shape @i.92) (map () 5) (body-matcher map-result.104)
         (map-result (map-result.104)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.146 (frame 1 2 3))
      (+arg2.170
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.168) iota.168)
          (body-matcher map-result.167) (map-result (map-result.167))
          (consumer (values)))))))
     (let
      ((reduce-arg.162
        (contiguous-subarray arr.146 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.164
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.163 reduce-arg.162)) (values reduce-arg.163))
           (body-matcher (reduce-arg.161)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.146 (frame 0) (shape 3) (shape))
             (reduce-arg1.130 reduce-arg2.131 reduce-arg.161)
             (+ reduce-arg1.130 reduce-arg2.131)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.172 +arg2.170)) (+ x.164 +arg2.172))
          (body-matcher map-result.171) (map-result (map-result.171))
          (consumer (values)))))))) |}]
;;

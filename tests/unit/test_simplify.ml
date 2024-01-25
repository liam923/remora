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
    (let ((+arg1.143 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((+arg1.145 +arg1.143)) (+ +arg1.145 4))
        (body-matcher map-result.144) (map-result (map-result.144))
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
    (let ((arr.306 (frame 1 2 3 4 5)))
     (let
      ((reduce-arg.338
        (contiguous-subarray arr.306 (frame 1) (shape 5) (shape 4))))
      (#1
       (loop-block (frame-shape 4)
        (map ((reduce-arg.339 reduce-arg.338)) (values reduce-arg.339))
        (body-matcher (reduce-arg.332)) (map-result ())
        (consumer
         (reduce-zero (contiguous-subarray arr.306 (frame 0) (shape 5) (shape))
          (reduce-arg1.268 reduce-arg2.271 reduce-arg.332)
          (+ reduce-arg1.268 reduce-arg2.271))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 3) (map () 5) (body-matcher map-result.148)
       (map-result (map-result.148)) (consumer (values))))) |}];
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
    (let ((x.188 (frame 5 6 7)) (+arg1.194 (frame 1 2 3)))
     (let
      ((y.197
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.196 +arg1.194)) (+ +arg1.196 4))
           (body-matcher map-result.195) (map-result (map-result.195))
           (consumer (values)))))))
      (let ((+arg1.203 y.197) (+arg2.204 y.197))
       (let
        ((+arg2.209
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.206 +arg1.203) (+arg2.207 +arg2.204))
              (+ +arg1.206 +arg2.207))
             (body-matcher map-result.205) (map-result (map-result.205))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((x.190 x.188))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.211 +arg2.209)) (+ x.190 +arg2.211))
               (body-matcher map-result.210) (map-result (map-result.210))
               (consumer (values))))))
           (body-matcher map-result.189) (map-result (map-result.189))
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
    (let ((x.173 (frame 5 6 7)) (+arg1.179 (frame 1 2 3)))
     (let
      ((+arg2.186
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.181 +arg1.179)) (+ +arg1.181 4))
           (body-matcher map-result.180) (map-result (map-result.180))
           (consumer (values)))))))
      (#0
       (#0
        (loop-block (frame-shape 3)
         (map ((x.175 x.173))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.188 +arg2.186)) (+ x.175 +arg2.188))
             (body-matcher map-result.187) (map-result (map-result.187))
             (consumer (values))))))
         (body-matcher map-result.174) (map-result (map-result.174))
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
    (let ((+arg1.189 (frame 1 2 3)))
     (let
      ((y.192
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg1.191 +arg1.189)) (+ +arg1.191 4))
           (body-matcher map-result.190) (map-result (map-result.190))
           (consumer (values)))))))
      (let ((+arg1.198 y.192) (+arg2.199 y.192))
       (let
        ((+arg2.204
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg1.201 +arg1.198) (+arg2.202 +arg2.199))
              (+ +arg1.201 +arg2.202))
             (body-matcher map-result.200) (map-result (map-result.200))
             (consumer (values)))))))
        (#0
         (#0
          (loop-block (frame-shape 3)
           (map ((+arg2.206 +arg2.204)) (+ 5 +arg2.206))
           (body-matcher map-result.205) (map-result (map-result.205))
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
    (let ((x.188 (frame 5 6 7)) (+arg1.194 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((x.190 x.188))
         (let
          ((y.197
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg1.196 +arg1.194)) (+ +arg1.196 x.190))
               (body-matcher map-result.195) (map-result (map-result.195))
               (consumer (values)))))))
          (let ((+arg1.203 y.197) (+arg2.204 y.197))
           (let
            ((+arg2.209
              (#0
               (#0
                (loop-block (frame-shape 3)
                 (map ((+arg1.206 +arg1.203) (+arg2.207 +arg2.204))
                  (+ +arg1.206 +arg2.207))
                 (body-matcher map-result.205) (map-result (map-result.205))
                 (consumer (values)))))))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map ((+arg2.211 +arg2.209)) (+ x.190 +arg2.211))
               (body-matcher map-result.210) (map-result (map-result.210))
               (consumer (values)))))))))
        (body-matcher map-result.189) (map-result (map-result.189))
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
    (let ((x.176 (frame 5 6 7)))
     (#0
      (#0
       (loop-block (frame-shape 3) (map ((x.178 x.176)) (+ x.178 14))
        (body-matcher map-result.177) (map-result (map-result.177))
        (consumer (values)))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect {|
    (frame (frame 1 2) (frame 3 4) (frame 5 6)) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect {| (frame) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (let ((+arg1.145 (frame 3 4)) (+arg2.146 (frame 5 6)))
     (frame (frame 1 2)
      (#0
       (#0
        (loop-block (frame-shape 2)
         (map ((+arg1.148 +arg1.145) (+arg2.149 +arg2.146))
          (+ +arg1.148 +arg2.149))
         (body-matcher map-result.147) (map-result (map-result.147))
         (consumer (values))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect {| (frame (frame) (frame)) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
    (let
     ((+arg1.165 (frame 1 2)) (+arg2.166 (frame 3 4)) (+arg1.173 (frame 1 2))
      (+arg2.174 (frame 3 4)) (+arg1.181 (frame 1 2)) (+arg2.182 (frame 3 4)))
     (frame
      (frame
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.168 +arg1.165) (+arg2.169 +arg2.166))
           (+ +arg1.168 +arg2.169))
          (body-matcher map-result.167) (map-result (map-result.167))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.176 +arg1.173) (+arg2.177 +arg2.174))
           (+ +arg1.176 +arg2.177))
          (body-matcher map-result.175) (map-result (map-result.175))
          (consumer (values)))))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.184 +arg1.181) (+arg2.185 +arg2.182))
           (+ +arg1.184 +arg2.185))
          (body-matcher map-result.183) (map-result (map-result.183))
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
    (let ((+arg1.143 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.145 +arg1.143)) (+ +arg1.145 1))
         (body-matcher map-result.144) (map-result (map-result.144))
         (consumer (values))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (let ((+arg1.150 (frame 2 2)))
     (frame (frame 1 1) (frame 2 2)
      (#0
       (#0
        (loop-block (frame-shape 2) (map ((+arg1.152 +arg1.150)) (+ +arg1.152 1))
         (body-matcher map-result.151) (map-result (map-result.151))
         (consumer (values)))))
      (frame 4 4) (frame 5 5))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (#0
     (#0
      (loop-block (frame-shape 1)
       (map () (iota iota.134)
        (#0
         (#0
          (loop-block (frame-shape 2)
           (map () (iota (iota.136 : iota.134))
            (#0
             (#0
              (loop-block (frame-shape 3)
               (map () (iota (iota.138 : iota.136)) iota.138)
               (body-matcher map-result.137) (map-result (map-result.137))
               (consumer (values))))))
           (body-matcher map-result.135) (map-result (map-result.135))
           (consumer (values))))))
       (body-matcher map-result.133) (map-result (map-result.133))
       (consumer (values))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (contiguous-subarray
     (#0
      (#0
       (loop-block (frame-shape 1)
        (map () (iota iota.153)
         (#0
          (#0
           (loop-block (frame-shape 2)
            (map () (iota (iota.155 : iota.153))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map () (iota (iota.157 : iota.155))
                 (#0
                  (#0
                   (loop-block (frame-shape 4)
                    (map () (iota (iota.159 : iota.157))
                     (#0
                      (#0
                       (loop-block (frame-shape 5)
                        (map () (iota (iota.161 : iota.159)) iota.161)
                        (body-matcher map-result.160)
                        (map-result (map-result.160)) (consumer (values))))))
                    (body-matcher map-result.158) (map-result (map-result.158))
                    (consumer (values))))))
                (body-matcher map-result.156) (map-result (map-result.156))
                (consumer (values))))))
            (body-matcher map-result.154) (map-result (map-result.154))
            (consumer (values))))))
        (body-matcher map-result.152) (map-result (map-result.152))
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
        (let ((+arg1.159 (frame 1 2 3)))
         (#0
          (#0
           (loop-block (frame-shape 3)
            (map ((+arg1.161 +arg1.159)) (+ +arg1.161 4))
            (body-matcher map-result.160) (map-result (map-result.160))
            (consumer (values)))))))
       (body-matcher map-result.155) (map-result (map-result.155))
       (consumer (values))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (let ((index-value.158 (frame 1 2 3)))
     (#0
      (#0
       (loop-block (frame-shape 3)
        (map ((index-value.160 index-value.158))
         (index-let ((i.132 runtime-value index-value.160))
          (box (i.132)
           (#0
            (#0
             (loop-block (frame-shape i.132) (map () 5)
              (body-matcher map-result.166) (map-result (map-result.166))
              (consumer (values))))))))
        (body-matcher map-result.159) (map-result (map-result.159))
        (consumer (values)))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (index-let ((@i.132 runtime-value (frame 1 2 3)))
     (box ((shape @i.132))
      (#0
       (#0
        (loop-block (frame-shape @i.132) (map () 5) (body-matcher map-result.164)
         (map-result (map-result.164)) (consumer (values))))))) |}];
  checkAndPrint
    {|
      (define x (reduce{int | 2 [] []} + [1 2 3]))
      (+ x iota{ | [1001]})
    |};
  [%expect
    {|
    (let
     ((arr.317 (frame 1 2 3))
      (+arg2.357
       (#0
        (#0
         (loop-block (frame-shape 1001) (map () (iota iota.355) iota.355)
          (body-matcher map-result.354) (map-result (map-result.354))
          (consumer (values)))))))
     (let
      ((reduce-arg.349
        (contiguous-subarray arr.317 (frame 1) (shape 3) (shape 2))))
      (let
       ((x.351
         (#1
          (loop-block (frame-shape 2)
           (map ((reduce-arg.350 reduce-arg.349)) (values reduce-arg.350))
           (body-matcher (reduce-arg.343)) (map-result ())
           (consumer
            (reduce-zero
             (contiguous-subarray arr.317 (frame 0) (shape 3) (shape))
             (reduce-arg1.274 reduce-arg2.277 reduce-arg.343)
             (+ reduce-arg1.274 reduce-arg2.277)))))))
       (#0
        (#0
         (loop-block (frame-shape 1001)
          (map ((+arg2.359 +arg2.357)) (+ x.351 +arg2.359))
          (body-matcher map-result.358) (map-result (map-result.358))
          (consumer (values)))))))) |}]
;;

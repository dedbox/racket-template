;; Copyright 2020 Eric Griffis <dedbox@gmail.com>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang racket/base

(require template)

(module+ test
  (require rackunit rackunit/text-ui)

  (provide (rename-out [literals the-tests]))

  (define (run-all-tests)
    (run-tests literals)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-test-suite literals
    (test-suite "boolean"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #t) #t)
          (check equal? (with-template () #f) #f)
          (check equal? (with-template ([$b #t]) #f) #f)
          (check equal? (with-template ([$b #t]) $b) #t))
        (test-case "quasisyntax"
          (check equal? (syntax-e (with-template () #`#t)) #t)
          (check equal? (syntax-e (with-template () #`#f)) #f)
          (check equal? (syntax-e (with-template ([$b #t]) #`#f)) #f)
          (check equal? (syntax-e (with-template ([$b #t]) #`$b)) #t))
        (test-case "syntax"
          (check equal? (syntax-e (with-template () #'#t)) #t)
          (check equal? (syntax-e (with-template () #'#f)) #f)
          (check equal? (syntax-e (with-template ([$b #t]) #'#f)) #f)
          (check equal? (syntax-e (with-template ([$b #t]) #'$b)) #t)))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #t) #t)
          (check equal? (quote-template () #f) #f)
          (check equal? (quote-template ([$b #t]) #f) #f)
          (check equal? (quote-template ([$b #t]) $b) #t))
        (test-case "quasisyntax"
          (check equal? (syntax-e (quote-template () #`#t)) #t)
          (check equal? (syntax-e (quote-template () #`#f)) #f)
          (check equal? (syntax-e (quote-template ([$b #t]) #`#f)) #f)
          (check equal? (syntax-e (quote-template ([$b #t]) #`$b)) #t))
        (test-case "syntax"
          (check equal? (syntax-e (quote-template () #'#t)) #t)
          (check equal? (syntax-e (quote-template () #'#f)) #f)
          (check equal? (syntax-e (quote-template ([$b #t]) #'#f)) #f)
          (check equal? (syntax-e (quote-template ([$b #t]) #'$b)) #t))))

    (test-suite "number"
      (test-suite "with-template"
        (test-case "datum"
          (check = (with-template () 0) 0)
          (check = (with-template ([$n 1]) 2) 2)
          (check = (with-template ([$n 1]) $n) 1)
          (check = (with-template ([$n 1] [$m 2]) $n$m3) 123))
        (test-case "quasisyntax"
          (check = (syntax-e (with-template () #`0)) 0)
          (check = (syntax-e (with-template ([$n 1]) #`2)) 2)
          (check = (syntax-e (with-template ([$n 1]) #`$n)) 1)
          (check = (syntax-e (with-template ([$n 1] [$m 2]) #`$n$m3)) 123))
        (test-case "syntax"
          (check = (syntax-e (with-template () #'0)) 0)
          (check = (syntax-e (with-template ([$n 1]) #'2)) 2)
          (check = (syntax-e (with-template ([$n 1]) #'$n)) 1)
          (check = (syntax-e (with-template ([$n 1] [$m 2]) #'$n$m3)) 123)))
      (test-suite "quote-template"
        (test-case "datum"
          (check = (quote-template () 0) 0)
          (check = (quote-template ([$n 1]) 2) 2)
          (check = (quote-template ([$n 1]) $n) 1)
          (check = (quote-template ([$n 1] [$m 2]) $n$m3) 123)
          (check = (quote-template ([$n 1] [$m 2]) (add1 $n$m$m)) 123))
        (test-case "quasisyntax"
          (check = (syntax-e (quote-template () #`0)) 0)
          (check = (syntax-e (quote-template ([$n 1]) #`2)) 2)
          (check = (syntax-e (quote-template ([$n 1]) #`$n)) 1)
          (check = (syntax-e (quote-template ([$n 1] [$m 2]) #`$n$m3)) 123))
        (test-case "syntax"
          (check = (syntax-e (quote-template () #'0)) 0)
          (check = (syntax-e (quote-template ([$n 1]) #'2)) 2)
          (check = (syntax-e (quote-template ([$n 1]) #'$n)) 1)
          (check = (syntax-e (quote-template ([$n 1] [$m 2]) #'$n$m3)) 123))))

    (test-suite "string"
      (test-suite "with-template"
        (test-case "datum"
          (check string=? (with-template () "") "")
          (check string=? (with-template ([$n 1]) "2") "2")
          (check string=? (with-template ([$n 1]) "$n") "1")
          (check string=? (with-template ([$n 1] [$m 2]) "$n$m3") "123"))
        (test-case "quasisyntax"
          (check string=? (syntax-e (with-template () #`"0")) "0")
          (check string=? (syntax-e (with-template ([$n 1]) #`"2")) "2")
          (check string=? (syntax-e (with-template ([$n 1]) #`"$n")) "1")
          (check string=? (syntax-e (with-template ([$n 1] [$m 2]) #`"$n$m3")) "123"))
        (test-case "syntax"
          (check string=? (syntax-e (with-template () #'"0")) "0")
          (check string=? (syntax-e (with-template ([$n 1]) #'"2")) "2")
          (check string=? (syntax-e (with-template ([$n 1]) #'"$n")) "1")
          (check string=? (syntax-e (with-template ([$n 1] [$m 2]) #'"$n$m3")) "123")))
      (test-suite "quote-template"
        (test-case "datum"
          (check string=? (quote-template () "") "")
          (check string=? (quote-template ([$n 1]) "2") "2")
          (check string=? (quote-template ([$n 1]) "$n") "1")
          (check string=? (quote-template ([$n 1] [$m 2]) "$n$m3") "123"))
        (test-case "quasisyntax"
          (check string=? (syntax-e (quote-template () #`"0")) "0")
          (check string=? (syntax-e (quote-template ([$n 1]) #`"2")) "2")
          (check string=? (syntax-e (quote-template ([$n 1]) #`"$n")) "1")
          (check string=? (syntax-e (quote-template ([$n 1] [$m 2]) #`"$n$m3")) "123"))
        (test-case "syntax"
          (check string=? (syntax-e (quote-template () #'"0")) "0")
          (check string=? (syntax-e (quote-template ([$n 1]) #'"2")) "2")
          (check string=? (syntax-e (quote-template ([$n 1]) #'"$n")) "1")
          (check string=? (syntax-e (quote-template ([$n 1] [$m 2]) #'"$n$m3")) "123"))))

    (test-suite "byte string"
      (test-suite "with-template"
        (test-case "datum"
          (check bytes=? (with-template () #"") #"")
          (check bytes=? (with-template ([$n 1]) #"2") #"2")
          (check bytes=? (with-template ([$n 1]) #"$n") #"1")
          (check bytes=? (with-template ([$n 1] [$m 2]) #"$n$m3") #"123"))
        (test-case "quasisyntax"
          (check bytes=? (syntax-e (with-template () #`#"0")) #"0")
          (check bytes=? (syntax-e (with-template ([$n 1]) #`#"2")) #"2")
          (check bytes=? (syntax-e (with-template ([$n 1]) #`#"$n")) #"1")
          (check bytes=? (syntax-e (with-template ([$n 1] [$m 2]) #`#"$n$m3")) #"123"))
        (test-case "syntax"
          (check bytes=? (syntax-e (with-template () #'#"0")) #"0")
          (check bytes=? (syntax-e (with-template ([$n 1]) #'#"2")) #"2")
          (check bytes=? (syntax-e (with-template ([$n 1]) #'#"$n")) #"1")
          (check bytes=? (syntax-e (with-template ([$n 1] [$m 2]) #'#"$n$m3")) #"123")))
      (test-suite "quote-template"
        (test-case "datum"
          (check bytes=? (quote-template () #"") #"")
          (check bytes=? (quote-template ([$n 1]) #"2") #"2")
          (check bytes=? (quote-template ([$n 1]) #"$n") #"1")
          (check bytes=? (quote-template ([$n 1] [$m 2]) #"$n$m3") #"123"))
        (test-case "quasisyntax"
          (check bytes=? (syntax-e (quote-template () #`#"0")) #"0")
          (check bytes=? (syntax-e (quote-template ([$n 1]) #`#"2")) #"2")
          (check bytes=? (syntax-e (quote-template ([$n 1]) #`#"$n")) #"1")
          (check bytes=? (syntax-e (quote-template ([$n 1] [$m 2]) #`#"$n$m3"))
                 #"123"))
        (test-case "syntax"
          (check bytes=? (syntax-e (quote-template () #'#"0")) #"0")
          (check bytes=? (syntax-e (quote-template ([$n 1]) #'#"2")) #"2")
          (check bytes=? (syntax-e (quote-template ([$n 1]) #'#"$n")) #"1")
          (check bytes=? (syntax-e (quote-template ([$n 1] [$m 2]) #'#"$n$m3"))
                 #"123"))))

    (test-suite "symbol"
      (test-suite "with-template"
        (test-case "datum"
          (check eq? (with-template () 'X) 'X)
          (check eq? (with-template ([$x a]) 'b) 'b)
          (check eq? (with-template ([$x a]) '$x) 'a)
          (check eq? (with-template ([$x a] [$y b]) '$x$yc) 'abc))
        (test-case "quasisyntax"
          (check eq? (syntax-e (with-template () #`X)) 'X)
          (check eq? (syntax-e (with-template ([$x a]) #`b)) 'b)
          (check eq? (syntax-e (with-template ([$x a]) #`a)) 'a)
          (check eq? (syntax-e (with-template ([$x a] [$y b]) #`$x$yc)) 'abc))
        (test-case "syntax"
          (check eq? (syntax-e (with-template () #'X)) 'X)
          (check eq? (syntax-e (with-template ([$x a]) #'b)) 'b)
          (check eq? (syntax-e (with-template ([$x a]) #'$x)) 'a)
          (check eq? (syntax-e (with-template ([$x a] [$y b]) #'$x$yc)) 'abc)))
      (test-suite "quote-template"
        (test-case "datum"
          (check eq? (quote-template () 'X) 'X)
          (check eq? (quote-template ([$x a]) 'b) 'b)
          (check eq? (quote-template ([$x a]) '$x) 'a)
          (check eq? (quote-template ([$x a] [$y b]) '$x$yc) 'abc))
        (test-case "quasisyntax"
          (check eq? (syntax-e (quote-template () #`X)) 'X)
          (check eq? (syntax-e (quote-template ([$x a]) #`b)) 'b)
          (check eq? (syntax-e (quote-template ([$x a]) #`$x)) 'a)
          (check eq? (syntax-e (quote-template ([$x a] [$y b]) #`$x$yc)) 'abc))
        (test-case "syntax"
          (check eq? (syntax-e (quote-template () #'X)) 'X)
          (check eq? (syntax-e (quote-template ([$x a]) #'b)) 'b)
          (check eq? (syntax-e (quote-template ([$x a]) #'$x)) 'a)
          (check eq? (syntax-e (quote-template ([$x a] [$y b]) #'$x$yc)) 'abc))))

    (test-suite "regexp"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #rx"X") #rx"X")
          (check equal? (with-template ([$x A]) #rx"B") #rx"B")
          (check equal? (with-template ([$x A]) #rx"$x") #rx"A")
          (check equal? (with-template ([$x A] [$y B]) #rx"$x$yC") #rx"ABC"))
        (test-case "quasisyntax"
          (check equal? (syntax-e (with-template () #`#rx"X")) #rx"X")
          (check equal? (syntax-e (with-template ([$x A]) #`#rx"B")) #rx"B")
          (check equal? (syntax-e (with-template ([$x A]) #`#rx"$x")) #rx"A")
          (check equal? (syntax-e (with-template ([$x A] [$y B]) #`#rx"$x$yC"))
                 #rx"ABC"))
        (test-case "syntax"
          (check equal? (syntax-e (with-template () #'#rx"X")) #rx"X")
          (check equal? (syntax-e (with-template ([$x A]) #'#rx"B")) #rx"B")
          (check equal? (syntax-e (with-template ([$x A]) #'#rx"$x")) #rx"A")
          (check equal? (syntax-e (with-template ([$x A] [$y B]) #'#rx"$x$yC"))
                 #rx"ABC")))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #rx"X") #rx"X")
          (check equal? (quote-template ([$x A]) #rx"B") #rx"B")
          (check equal? (quote-template ([$x A]) #rx"$x") #rx"A")
          (check equal? (quote-template ([$x A] [$y B]) #rx"$x$yC") #rx"ABC"))
        (test-case "quasisyntax"
          (check equal? (syntax-e (quote-template () #`#rx"X")) #rx"X")
          (check equal? (syntax-e (quote-template ([$x A]) #`#rx"B")) #rx"B")
          (check equal? (syntax-e (quote-template ([$x A]) #`#rx"$x")) #rx"A")
          (check equal? (syntax-e (quote-template ([$x A] [$y B]) #`#rx"$x$yC"))
                 #rx"ABC"))
        (test-case "syntax"
          (check equal? (syntax-e (quote-template () #'#rx"X")) #rx"X")
          (check equal? (syntax-e (quote-template ([$x A]) #'#rx"B")) #rx"B")
          (check equal? (syntax-e (quote-template ([$x A]) #'#rx"$x")) #rx"A")
          (check equal? (syntax-e (quote-template ([$x A] [$y B]) #'#rx"$x$yC"))
                 #rx"ABC"))))

    (test-suite "pregexp"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #px"X") #px"X")
          (check equal? (with-template ([$x A]) #px"B") #px"B")
          (check equal? (with-template ([$x A]) #px"$x") #px"A")
          (check equal? (with-template ([$x A] [$y B]) #px"$x$yC") #px"ABC"))
        (test-case "quasisyntax"
          (check equal? (syntax-e (with-template () #`#px"X")) #px"X")
          (check equal? (syntax-e (with-template ([$x A]) #`#px"B")) #px"B")
          (check equal? (syntax-e (with-template ([$x A]) #`#px"$x")) #px"A")
          (check equal? (syntax-e (with-template ([$x A] [$y B]) #`#px"$x$yC"))
                 #px"ABC"))
        (test-case "syntax"
          (check equal? (syntax-e (with-template () #'#px"X")) #px"X")
          (check equal? (syntax-e (with-template ([$x A]) #'#px"B")) #px"B")
          (check equal? (syntax-e (with-template ([$x A]) #'#px"$x")) #px"A")
          (check equal? (syntax-e (with-template ([$x A] [$y B]) #'#px"$x$yC"))
                 #px"ABC")))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #px"X") #px"X")
          (check equal? (quote-template ([$x A]) #px"B") #px"B")
          (check equal? (quote-template ([$x A]) #px"$x") #px"A")
          (check equal? (quote-template ([$x A] [$y B]) #px"$x$yC") #px"ABC"))
        (test-case "quasisyntax"
          (check equal? (syntax-e (quote-template () #`#px"X")) #px"X")
          (check equal? (syntax-e (quote-template ([$x A]) #`#px"B")) #px"B")
          (check equal? (syntax-e (quote-template ([$x A]) #`#px"$x")) #px"A")
          (check equal? (syntax-e (quote-template ([$x A] [$y B]) #`#px"$x$yC"))
                 #px"ABC"))
        (test-case "syntax"
          (check equal? (syntax-e (quote-template () #'#px"X")) #px"X")
          (check equal? (syntax-e (quote-template ([$x A]) #'#px"B")) #px"B")
          (check equal? (syntax-e (quote-template ([$x A]) #'#px"$x")) #px"A")
          (check equal? (syntax-e (quote-template ([$x A] [$y B]) #'#px"$x$yC"))
                 #px"ABC"))))

    (test-suite "keyword"
      (test-suite "with-template"
        (test-case "datum"
          (check eq? (with-template () '#:X) '#:X)
          (check eq? (with-template ([$x I]) 'J) 'J)
          (check eq? (with-template ([$x I]) '$x) 'I)
          (check eq? (with-template ([$x I] [$y J]) '$x$yK) 'IJK))
        (test-case "quasisyntax"
          (check eq? (syntax-e (with-template () #`X)) 'X)
          (check eq? (syntax-e (with-template ([$x I]) #`J)) 'J)
          (check eq? (syntax-e (with-template ([$x I]) #`I)) 'I)
          (check eq? (syntax-e (with-template ([$x I] [$y J]) #`$x$yK)) 'IJK))
        (test-case "syntax"
          (check eq? (syntax-e (with-template () #'X)) 'X)
          (check eq? (syntax-e (with-template ([$x I]) #'J)) 'J)
          (check eq? (syntax-e (with-template ([$x I]) #'$x)) 'I)
          (check eq? (syntax-e (with-template ([$x I] [$y J]) #'$x$yK)) 'IJK)))
      (test-suite "quote-template"
        (test-case "datum"
          (check eq? (quote-template () 'X) 'X)
          (check eq? (quote-template ([$x I]) 'J) 'J)
          (check eq? (quote-template ([$x I]) '$x) 'I)
          (check eq? (quote-template ([$x I] [$y J]) '$x$yK) 'IJK))
        (test-case "quasisyntax"
          (check eq? (syntax-e (quote-template () #`X)) 'X)
          (check eq? (syntax-e (quote-template ([$x I]) #`J)) 'J)
          (check eq? (syntax-e (quote-template ([$x I]) #`$x)) 'I)
          (check eq? (syntax-e (quote-template ([$x I] [$y J]) #`$x$yK)) 'IJK))
        (test-case "syntax"
          (check eq? (syntax-e (quote-template () #'X)) 'X)
          (check eq? (syntax-e (quote-template ([$x I]) #'J)) 'J)
          (check eq? (syntax-e (quote-template ([$x I]) #'$x)) 'I)
          (check eq? (syntax-e (quote-template ([$x I] [$y J]) #'$x$yK)) 'IJK))))

    (test-suite "vector"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #(X)) #(X))
          (check equal? (with-template ([$v R]) #(S)) #(S))
          (check equal? (with-template ([$v R]) #($v)) #(R))
          (check equal? (with-template ([$v R] [$u S]) #($v $u T)) #(R S T)))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (with-template () #`#(X))) #(X))
          (check equal? (syntax->datum (with-template ([$v R]) #`#(S))) #(S))
          (check equal? (syntax->datum (with-template ([$v R]) #`#(R))) #(R))
          (check equal? (syntax->datum (with-template ([$v R] [$u S]) #`#($v $u T)))
                 #(R S T)))
        (test-case "syntax"
          (check equal? (syntax->datum (with-template () #'#(X))) #(X))
          (check equal? (syntax->datum (with-template ([$v R]) #'#(S))) #(S))
          (check equal? (syntax->datum (with-template ([$v R]) #'#($v))) #(R))
          (check equal? (syntax->datum (with-template ([$v R] [$u S]) #'#($v $u T)))
                 #(R S T))))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #(X)) #(X))
          (check equal? (quote-template ([$v R]) #(S)) #(S))
          (check equal? (quote-template ([$v R]) #($v)) #(R))
          (check equal? (quote-template ([$v R] [$u S]) #($v $u T)) #(R S T)))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (quote-template () #`#(X))) #(X))
          (check equal? (syntax->datum (quote-template ([$v R]) #`#(S))) #(S))
          (check equal? (syntax->datum (quote-template ([$v R]) #`#(R))) #(R))
          (check equal? (syntax->datum (quote-template ([$v R] [$u S]) #`#($v $u T)))
                 #(R S T)))
        (test-case "syntax"
          (check equal? (syntax->datum (quote-template () #'#(X))) #(X))
          (check equal? (syntax->datum (quote-template ([$v R]) #'#(S))) #(S))
          (check equal? (syntax->datum (quote-template ([$v R]) #'#($v))) #(R))
          (check equal? (syntax->datum (quote-template ([$v R] [$u S]) #'#($v $u T)))
                 #(R S T)))))

    (test-suite "box"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #&X) #&X)
          (check equal? (with-template ([$v R]) #&S) #&S)
          (check equal? (with-template ([$v R]) #&$v) #&R)
          (check equal? (with-template ([$v R] [$u S]) #&$v$uT) #&RST))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (with-template () #`#&X)) #&X)
          (check equal? (syntax->datum (with-template ([$v R]) #`#&S)) #&S)
          (check equal? (syntax->datum (with-template ([$v R]) #`#&R)) #&R)
          (check equal? (syntax->datum (with-template ([$v R] [$u S]) #`#&$v$uT))
                 #&RST))
        (test-case "syntax"
          (check equal? (syntax->datum (with-template () #'#&X)) #&X)
          (check equal? (syntax->datum (with-template ([$v R]) #'#&S)) #&S)
          (check equal? (syntax->datum (with-template ([$v R]) #'#&$v)) #&R)
          (check equal? (syntax->datum (with-template ([$v R] [$u S]) #'#&$v$uT))
                 #&RST)))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #&X) #&X)
          (check equal? (quote-template ([$v R]) #&S) #&S)
          (check equal? (quote-template ([$v R]) #&$v) #&R)
          (check equal? (quote-template ([$v R] [$u S]) #&$v$uT) #&RST))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (quote-template () #`#&X)) #&X)
          (check equal? (syntax->datum (quote-template ([$v R]) #`#&S)) #&S)
          (check equal? (syntax->datum (quote-template ([$v R]) #`#&R)) #&R)
          (check equal? (syntax->datum (quote-template ([$v R] [$u S]) #`#&$v$uT))
                 #&RST))
        (test-case "syntax"
          (check equal? (syntax->datum (quote-template () #'#&X)) #&X)
          (check equal? (syntax->datum (quote-template ([$v R]) #'#&S)) #&S)
          (check equal? (syntax->datum (quote-template ([$v R]) #'#&$v)) #&R)
          (check equal? (syntax->datum (quote-template ([$v R] [$u S]) #'#&$v$uT))
                 #&RST))))

    (test-suite "hash"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #hash([X . 0])) #hash([X . 0]))
          (check equal? (with-template ([$k J]) #hash([K . 1])) #hash([K . 1]))
          (check equal? (with-template ([$k J]) #hash([$k . 1])) #hash([J . 1]))
          (check equal? (with-template ([$k J] [$v 2]) #hash([$k . $v]))
                 #hash([J . 2])))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (with-template () #`#hash([X . 0])))
                 #hash([X . 0]))
          (check equal? (syntax->datum (with-template ([$k J]) #`#hash([K . 1])))
                 #hash([K . 1]))
          (check equal? (syntax->datum (with-template ([$k J]) #`#hash([$k . 1])))
                 #hash([J . 1]))
          (check equal? (syntax->datum (with-template ([$k J] [$v 2]) #`#hash([$k . $v])))
                 #hash([J . 2])))
        (test-case "syntax"
          (check equal? (syntax->datum (with-template () #'#hash([X . 0])))
                 #hash([X . 0]))
          (check equal? (syntax->datum (with-template ([$k J]) #'#hash([K . 1])))
                 #hash([K . 1]))
          (check equal? (syntax->datum (with-template ([$k J]) #'#hash([$k . 1])))
                 #hash([J . 1]))
          (check equal? (syntax->datum (with-template ([$k J] [$v 2]) #'#hash([$k . $v])))
                 #hash([J . 2]))))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #hash([X . 0])) #hash([X . 0]))
          (check equal? (quote-template ([$k J]) #hash([K . 1])) #hash([K . 1]))
          (check equal? (quote-template ([$k J]) #hash([$k . 1])) #hash([J . 1]))
          (check equal? (quote-template ([$k J] [$v 2]) #hash([$k . $v]))
                 #hash([J . 2])))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (quote-template () #`#hash([X . 0])))
                 #hash([X . 0]))
          (check equal? (syntax->datum (quote-template ([$k J]) #`#hash([K . 1])))
                 #hash([K . 1]))
          (check equal? (syntax->datum (quote-template ([$k J]) #`#hash([$k . 1])))
                 #hash([J . 1]))
          (check equal? (syntax->datum (quote-template ([$k J] [$v 2]) #`#hash([$k . $v])))
                 #hash([J . 2])))
        (test-case "syntax"
          (check equal? (syntax->datum (quote-template () #'#hash([X . 0])))
                 #hash([X . 0]))
          (check equal? (syntax->datum (quote-template ([$k J]) #'#hash([K . 1])))
                 #hash([K . 1]))
          (check equal? (syntax->datum (quote-template ([$k J]) #'#hash([$k . 1])))
                 #hash([J . 1]))
          (check equal? (syntax->datum (quote-template ([$k J] [$v 2]) #'#hash([$k . $v])))
                 #hash([J . 2])))))

    (test-suite "prefab struct"
      (test-suite "with-template"
        (test-case "datum"
          (check equal? (with-template () #s(X)) #s(X))
          (check equal? (with-template ([$x A]) #s(B 1)) #s(B 1))
          (check equal? (with-template ([$x A]) #s($x 1)) #s(A 1))
          (check equal? (with-template ([$x A] [$y 2]) #s($x$y3 $y)) #s(A23 2)))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (with-template () #`#s(X 0))) #s(X 0))
          (check equal? (syntax->datum (with-template ([$x A]) #`#s(B 1))) #s(B 1))
          (check equal? (syntax->datum (with-template ([$x A]) #`#s($x 1))) #s(A 1))
          (check equal? (syntax->datum (with-template ([$x A] [$y 2]) #`#s($x$y3 $y)))
                 #s(A23 2)))
        (test-case "syntax"
          (check equal? (syntax->datum (with-template () #'#s(X 0))) #s(X 0))
          (check equal? (syntax->datum (with-template ([$x A]) #'#s(B 1))) #s(B 1))
          (check equal? (syntax->datum (with-template ([$x A]) #'#s($x 1))) #s(A 1))
          (check equal? (syntax->datum (with-template ([$x A] [$y 2]) #'#s($x$y3 $y)))
                 #s(A23 2))))
      (test-suite "quote-template"
        (test-case "datum"
          (check equal? (quote-template () #s(X)) #s(X))
          (check equal? (quote-template ([$x A]) #s(B 1)) #s(B 1))
          (check equal? (quote-template ([$x A]) #s($x 1)) #s(A 1))
          (check equal? (quote-template ([$x A] [$y 2]) #s($x$y3 $y)) #s(A23 2)))
        (test-case "quasisyntax"
          (check equal? (syntax->datum (quote-template () #`#s(X 0))) #s(X 0))
          (check equal? (syntax->datum (quote-template ([$x A]) #`#s(B 1))) #s(B 1))
          (check equal? (syntax->datum (quote-template ([$x A]) #`#s($x 1))) #s(A 1))
          (check equal? (syntax->datum (quote-template ([$x A] [$y 2]) #`#s($x$y3 $y)))
                 #s(A23 2)))
        (test-case "syntax"
          (check equal? (syntax->datum (quote-template () #'#s(X 0))) #s(X 0))
          (check equal? (syntax->datum (quote-template ([$x A]) #'#s(B 1))) #s(B 1))
          (check equal? (syntax->datum (quote-template ([$x A]) #'#s($x 1))) #s(A 1))
          (check equal? (syntax->datum (quote-template ([$x A] [$y 2]) #'#s($x$y3 $y)))
                 #s(A23 2))))))

  (run-all-tests))

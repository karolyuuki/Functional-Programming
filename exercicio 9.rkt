#lang racket

(require rackunit rackunit/text-ui)
(require racket/trace)

;; Atividade 9 - Exercism
;; Aluna Karoline Neves Bernardo - 11311868


;; Exercicio 1 - Leap

(define (leap year)
  (if (integer? (/ year 4))
      (if (integer? (/ year 100))
          (if (integer? (/ year 400))
              #t
              #f)
          #t)
      #f))

(define-test-suite test-leap
  (test-true "ano bissexto" (leap 1996))
  (test-true "bissexto divisivel por 100" (leap 2000))
  (test-false "ano não bissexto" (leap 1997))
  (test-false "não bissexto" (leap 1900))
  )


;;Exercicio 2 - Two Fer

(define (twofer str)
  (if (equal? str "")
      (printf "One for you, one for me\n" )
      (printf "One for ~a, one for me\n" str)))


(twofer "")
(twofer "Bran")

;;  Exercicio 3 - Hamming distance

(define (hamming dna1 dna2 [dist 0])
  (if (empty? dna1)
      dist
      (if (equal? (first dna1) (first dna2))
          (hamming (rest dna1) (rest dna2) dist)
          (hamming (rest dna1) (rest dna2) (add1 dist)))))


(define-test-suite test-hamm
  (test-equal? "cadeias iguais" (hamming '("G""A""G""C""C""T""A""C""T""A""A""C""G""G""G""A""T")
                                       '("G""A""G""C""C""T""A""C""T""A""A""C""G""G""G""A""T")) 0)
  (test-equal? "cadeias diferentes" (hamming '("G""A""G""C""C""T""A""C""T""A""A""C""G""G""G""A""T")
                                       '("C""A""T""C""G""T""A""A""T""G""A""C""G""G""C""C""T")) 7))
  

;; Exercicio 4 - The Collatz Conjecture

(define (collatz n [valor 0])
  (if (= n 1)
      valor
      (if (even? n)
          (collatz (/ n 2) (add1 valor))
          (collatz (+ 1(* n 3)) (add1 valor)))))



(define-test-suite test-coll
  (test-equal? "n = 12 v = 9" (collatz 12) 9))


;; Exercicio 5 - RNA Transcription

(define transcriptable (hash
                        "g" "c"
                        "c" "g"
                        "t" "a"
                        "a" "u"))

(define (dna-to-rna dna)
  (if (empty? dna)
      '()
      (cons (hash-ref transcriptable (first dna))
            (dna-to-rna (rest dna)))))

(define-test-suite test-rna-transcript
  (test-equal? "n = 12 v = 9" (dna-to-rna (list "g" "t" "a")) '("c" "a" "u")))

(trace dna-to-rna)

  

(run-tests
 (test-suite "todos os testes"
             test-leap
             test-hamm
             test-coll
             test-rna-transcript))
      
      
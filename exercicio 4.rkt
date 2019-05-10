#lang racket


;; Aluna: karoline Neves Bernardo
;; Ciencia da Computacao - 11311868

;;Esta tarefa consiste em adaptar as funções recursivas que foram criadas
;;em exercícios anteriores para versões com recursão em cauda (tail recursion)
(require rackunit rackunit/text-ui)
(require racket/trace)

;;------Lista 4---------------

;; --- Exercício 1 ---------------------

;; Crie uma função (mult m n) que multiplica os dois números
;; naturais m e n, usando apenas a operação de soma.

;; aqui está uma definição incorreta da função, para que os testes funcionem.
;; para resolver o exercício deve-se alterar o corpo da função para uma versão
;; que faça os testes passarem
(define (mult m n [acc 0])
  (cond [(= m 0) acc]
        [(= n 0) acc]
        [(> m n) (mult (- m 1) n (+ acc n))]
        [else (mult m (- n 1) (+ acc m))]))
  

(define-test-suite testes-mult
  (test-equal? "3 * 4"  (mult 3 4)    12)
  (test-equal? "5 * 0"  (mult 5 0)    0)
  (test-equal? "0 * 5"  (mult 0 5)    0)
  (test-equal? "13 * 1" (mult 13 1)   13)
  (test-equal? "1 * 13" (mult 1 13)   13))

;; --- Exercício 2 ---------------------

;; Crie uma função (sub m n) que calcula a subtração de m por n,
;; usando apenas as funções add1 e sub1. Pode ser assumido que
;; m >= n, mas não é difícil escrever uma função que funcione mesmo
;; quando m < n.

(define (sub m n)
  (cond [(= m 0) n]
        [(= n 0) m]
        [else(sub (sub1 m) (sub1 n))])
)

(define-test-suite testes-sub
  (test-equal? "42 - 0"  (sub 42 0)   42)
  (test-equal? "32 - 16" (sub 32 16)  16)
  (test-equal? "42 - 42" (sub 42 42)  0)
  (test-equal? "11 - 10" (sub 11 10)  1))

;; --- Exercício 3 ---------------------

;; Crie uma função (par n) que retorna #t se n é par e #f se n é ímpar. Em seguida,
;; crie uma função (impar n) que retorna #t se n é ímpar e #f se n é par. Pense em
;; como definir uma usando a outra (ver observações nas notas de aula).

;; suas versoes passadas já eram tail recursive
(define (par n)
  (cond
    [(= n 0)#t]
    [(= n 1)#f]
    [else (par (remainder n 2))])
 )

(define (impar n)
  (if (par n)
      #f
      #t))

(define-test-suite testes-par-impar
  (test-true "2 é par"         (par 2))
  (test-true "0 é par"         (par 0))
  (test-true "42 é par"        (par 42))
  (test-false "3 não é par"    (par 3))
  (test-false "111 não é par"  (par 111))
  (test-false "12 não é ímpar" (impar 12))
  (test-false "0 não é ímpar"  (impar 0))
  (test-true "7 é ímpar"       (impar 7))
  (test-true "353 é ímpar"     (impar 353)))

;; --- Exercício 4 ---------------------

;; Altere a definição de lista-ex4, abaixo, para que ela contenha os números
;; de 1 a 5, em ordem crescente, usando apenas cons e a lista vazia
(define lista-ex4 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))))
  
 ;; Não são necessarias alterações nesta questão 

;; para não entregar a resposta no teste, vamos construir a resposta de outra forma...
(define-test-suite teste-ex4
  (test-equal? "numeros de 1 a 5" lista-ex4 (range 1 6)))

;; --- Exercício 5 ---------------------

;; Altere a definição de lista-ex5, abaixo, para que ela contenha os números
;; de 1 a 5, em ordem crescente, usando a notação com apóstrofo ou a função list
(define lista-ex5 (list 1 2 3 4 5))

;; Não são necessarias alterações nesta questão

(define-test-suite teste-ex5
  (test-equal? "numeros de 1 a 5" lista-ex5 (range 1 6)))


;; --- Exercício 6 ---------------------

;; Considere a lista6, a seguir
(define lista6 (list 11 22 33 44 55 66))

;; Altere a definição da variável elem3-lista6, abaixo, para que ele tenha
;; o valor do terceiro elemento de lista6, usando apenas as funções first e rest
(define elem3-lista6 
   (first(rest(rest lista6))))
  ;;;; Não são necessarias alterações nesta questão    

(define-test-suite teste-ex6
  (test-equal? "elem3-lista6 deve ser 33" elem3-lista6 33))

;; --- Exercício 7 ---------------------

;; Escreva a função terceiro-elemento, abaixo, que retorna sempre o terceiro
;; elemento da lista l. Suponha que l sempre tenha 3 elementos ou mais.
(define (terceiro-elemento l)
  (first(rest(rest l))))

;; A função não é recursiva, logo, não são necessarias alterações nesta questão

(define-test-suite testes-terceiro-elemento
  (test-equal? "3o de '(1 2 3)" (terceiro-elemento (list 1 2 3))   3)
  (test-equal? "3o de '(4 8 15 16 23 42)"
               (terceiro-elemento (list 4 8 15 16 23 42))
               15))


;; --- Exercício 8 ---------------------

;; Crie uma função recursiva soma-lista (abaixo) que, dada uma lista de números,
;; calcula a soma dos números contidos
(define (soma-lista l [acc 0])
  (if (empty? l)
      acc
      (soma-lista (rest l) (+ (first l) acc)))
  )

(define-test-suite testes-soma-lista
  (test-equal? "soma da lista vazia"                (soma-lista '())                  0)
  (test-equal? "soma de um número apenas"           (soma-lista '(13))                13)
  (test-equal? "soma de vários números"             (soma-lista (list 5 4 3 2 1))     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista (list 1 2 3 4 5))     15)
  (test-equal? "soma de lista com zero"             (soma-lista (list 1 0 2 0 13 0))  16))

;; --- Exercício 9 ---------------------

;; Crie uma função recursiva mult-lista (abaixo) que, dada uma lista de números,
;; calcula o produto dos números contidos (a lista vazia deve ter produto igual a 1)
(define (mult-lista l [acc 1])
  (if (empty? l)
      acc
      (mult-lista (rest l)(* (first l) acc)))
  )

(define-test-suite testes-mult-lista
  (test-equal? "produto da lista vazia"            (mult-lista '())                  1)
  (test-equal? "produto de lista com zero"         (mult-lista (list 1 0 2 0 13 0))  0)
  (test-equal? "produto de um número"              (mult-lista '(55))                55)
  (test-equal? "produto de vários números"         (mult-lista (list 1 2 3 4 5))     120)
  (test-equal? "produto de números em outra ordem" (mult-lista (list 2 5 1 4 3))     120))

;; --- Exercício 10 --------------------

;; Crie uma função recursiva max-lista (abaixo) que, dada uma lista de números naturais,
;; calcula o maior número entre os presentes na lista. Use (max-lista '()) = 0.
(define (max-lista l [max 0])
  (if (empty? l )
      max
      (if (> (first l) max)
          (max-lista (rest l) (first l))
          (max-lista(rest l) max))
  ))
;;(trace max-lista)

(define-test-suite testes-max-lista
  (test-equal? "maximo da lista vazia"       (max-lista '())                     0)
  (test-equal? "maximo de lista unitaria"    (max-lista '(22))                   22)
  (test-equal? "maximo de lista com numeros" (max-lista (list 8 55 13 24 45))    55)
  (test-equal? "maximo não muda com ordem"   (max-lista (list 45 13 8 55 24))    55)
  (test-equal? "maximo de lista com zeros"   (max-lista (list 1 0 13 0 356 0))   356))

;; --- Exercício 11 --------------------

;; Crie uma funcao elemento-n (abaixo) que, dada uma lista (que pode conter
;; números ou outros tipos de elementos) e um número n, retorna o n-ésimo
;; elemento da lista, contando a partir de zero. Se n é maior ou igual ao
;; tamanho da lista, a função deve retornar #f (veja os testes para exemplos
(define (elemento-n lista n)
  (if (empty? lista)
      #f
      (if (= n 0)
          (first lista)
          (elemento-n (rest lista) (sub1 n)))))

;; A função já é tail recursive
  
;;(trace elemento-n)

(define-test-suite testes-elemento-n
  (test-equal? "elemento de lista vazia" (elemento-n '() 0)                #f)
  (test-equal? "elemento 0"              (elemento-n (list 1 2 3 4 5) 0)    1)
  (test-equal? "elemento 3"              (elemento-n (list 1 2 3 4 5) 3)    4)
  (test-equal? "ultimo elemento"         (elemento-n (list 1 2 3 4 5) 4)    5)
  (test-equal? "indice fora da lista"    (elemento-n (list 1 2 3 4 5) 7)    #f))

;; --- Exercício 12 --------------------

;; Muitas vezes precisamos transformar os elementos de uma lista da mesma
;; maneira. Escreva a função quadrado-lista (abaixo) que, dada uma lista de
;; números, obtém uma lista contendo o quadrado de cada número da lista
;; original (nas mesmas posições)
(define (quadrado-lista l[ listi '()])
  (if (empty? l)
      (reverse listi)
      (quadrado-lista (rest l)(cons (sqr (first l)) listi ))))

;;(trace quadrado-lista)

(define-test-suite testes-quadrado-lista
  (test-equal? "quadrado da lista vazia"  (quadrado-lista '())        '())
  (test-equal? "quadrado de um número"    (quadrado-lista '(5))       (list 25))
  (test-equal? "quadrado de números"
               (quadrado-lista (list 2 5 12 25))
               (list 4 25 144 625)))

;; --- Exercício 13 --------------------

;; Agora vamos selecionar itens em uma lista. Crie uma função filtra-par (abaixo)
;; que, dado uma lista de números naturais, retorna uma outra lista contendo apenas
;; os números pares da lista original. Use a função par definida no exercício 3
(define (filtra-par l [listi'()])
  (if (empty? l )
      (reverse listi)
      (if (par (first l))
          (filtra-par (rest l)(cons (first l)listi ))
          (filtra-par (rest l) listi))
      )
  )
;;(trace filtra-par)

(define-test-suite testes-filtra-par
  (test-equal? "filtragem da lista vazia"     (filtra-par '())                  '())
  (test-equal? "filtragem de lista sem pares" (filtra-par (list 1 3 5 7 9))     '())
  (test-equal? "filtragem de lista com pares" (filtra-par (list 1 2 3 4 5))     (list 2 4))
  (test-equal? "filtragem com todos os itens pares"
               (filtra-par (list 2 4 22 144))
               (list 2 4 22 144)))

;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             testes-mult
             testes-sub
             testes-par-impar
             teste-ex4
             teste-ex5
             teste-ex6
             testes-terceiro-elemento
             testes-soma-lista
             testes-mult-lista
             testes-max-lista
             testes-elemento-n
             testes-quadrado-lista
             testes-filtra-par))


;; Tarefa 3
;;
;; Nesta tarefa faremos um pouco de revisão mas também vamos explorar
;; algumas técnicas novas.
;;

;; Vimos antes como escrever funções recursivas para lidar com listas, baseando
;; a estrutura do código na estrutura recursiva das listas.

;; Lembrando: uma lista satisfaz um dos seguintes casos:
;; 1. é a lista vazia, '()
;; 2. é o cons de um elemento na frente de uma outra lista, (cons x lst)
;;    onde x é o elemento e lst a lista

;; Nas primeiras duas questões podemos continuar com essa estrutura:

;; --- Questão 1 ----------------------------

;; Escreva uma função remove-primeiro tal que
;; (remove-primeiro x lst) remove a primeira ocorrência do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
;; Veja os testes para exemplos.
(define (remove-primeiro x lst [listi '()])
  (cond [(empty? lst) (reverse listi)]
        [(equal? x listi) (remove-primeiro listi (rest lst) (cons (first lst) listi))]
        [(equal? x (first lst)) (remove-primeiro listi (rest lst) listi)]
        [else (remove-primeiro x (rest lst) (cons (first lst) listi))]
  )
)
 ;;(trace remove-primeiro)

(define-test-suite test-remove-primeiro
  (test-equal? "lista vazia"
               (remove-primeiro 5 '())              '())
  
  (test-equal? "uma ocorrência"
               (remove-primeiro 5 '(1 3 5 7))       '(1 3 7))
  
  (test-equal? "múltiplas ocorrências"
               (remove-primeiro 5 '(1 3 5 7 5 9))   '(1 3 7 5 9))
  
  (test-equal? "nenhuma ocorrência"
               (remove-primeiro 3 '(11 7 23 55 42)) '(11 7 23 55 42)))


;; --- Questão 2 ----------------------------

;; Escreva uma função remove-todos tal que
;; (remove-todos x lst) remove todas as ocorrencias do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
(define (remove-todos x lst [listi '()])
  (if (empty? lst)
      (reverse listi)
      (if (= (first lst) x)
          (remove-todos x (rest lst) listi)
          (remove-todos x (rest lst)(cons (first lst) listi)))))

;;(trace remove-todos)

(define-test-suite test-remove-todos
  (test-equal? "lista vazia"           (remove-todos 5 '())              '())
  (test-equal? "uma ocorrência"        (remove-todos 5 '(1 3 5 7))       '(1 3 7))
  (test-equal? "múltiplas ocorrências" (remove-todos 5 '(1 3 5 7 5 9))   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos 3 '(11 7 23 55 42)) '(11 7 23 55 42)))


;; --- Questão 3 ----------------------------

;; As funções remove-primeiro e remove-todos, acima, funcionam apenas para
;; listas de números, ou também funcionam para listas de outros tipos de
;; elementos, como strings? Funciona com listas heterogêneas (com elementos
;; de tipos diferentes na mesma lista)? Faça alguns testes que demonstram se
;; funcionam ou não.

(define-test-suite test-remove-tipos
  (test-equal? "lista com strings primeiro"        (remove-primeiro "a" '())              '())
  (test-equal? "lista com strings todos"           (remove-todos "a" '())              '())
  (test-equal? "uma ocorrência"                 (remove-todos "a" (list "b" "a" "c"))       (list "b" "c"))
  (test-equal? "uma ocorrência primeiro"        (remove-primeiro "a" (list "b" "a" "c"))       (list "b" "c"))
  (test-equal? "lista heterogenea" (remove-todos 5 '(1 3 5 #t 5 9))   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos "a" '(11 7 23 55 42)) '(11 7 23 55 42)))

;; Não são necessarias alteraçoes nesta questão
;;RESULTADO: Esta função não aceita strings ou listas heterogeneas

;; --- Questão 4 ----------------------------

;; Listas podem ser usadas como base para a criação de várias outras estruturas
;; de dados. Embora raramente uma implementação baseada em listas seja a mais
;; rápida, pode ser utilizada para conjuntos de dados pequenos e é fácil de criar
;; em uma linguagem funcional.

;; Uma estrutura de dados que pode ser construída em cima das listas são conjuntos.
;; Conjuntos são similares às listas, mas podem ter apenas uma ocorrência de cada
;; elemento. Algumas operações normalmente usadas com conjuntos são a união,
;; intersecção, diferença de conjuntos e teste de pertencimento.

;; Para o teste de pertencimento podemos continuar usando a receita recursiva baseada
;; na estrutura das listas:

;; Escreva uma função pertence? tal que
;; (pertence? x lst) retorna #t se o elemento x aparece na lista (conjunto) lst
(define (pertence? x lst)
  (if (empty? lst)
      #f
      (if (= (first lst) x)
          #t
          (pertence? x(rest lst)))))
 ;;(trace pertence?)

;; A função já é tail recursive, não são necessarias alterações

(define-test-suite test-pertence?
  (test-false "lista vazia"    (pertence? 5 '()))
  (test-true  "3 pertence"     (pertence? 3 '(1 2 3 4 5)))
  (test-false "9 não pertence" (pertence? 9 '(1 2 3 4 5)))
  (test-true  "5 pertence"     (pertence? 5 '(1 2 3 4 5))))


;; --- Questão 5 ----------------------------

;; Infelizmente nem sempre podemos usar a mesma receita para recursividade. Um caso
;; comum são funções que devem combinar duas listas de alguma forma (como é o caso
;; das operações de união, intersecção e diferença de conjuntos).

;; Para praticar a ideia primeiro, escreva uma função combine tal que
;; (combine l1 l2) retorna uma lista de pares (listas de dois elementos) onde o primeiro
;; elemento de cada par vem de l1 e o segundo de l2. O número de pares deve ser igual ao
;; tamanho da menor lista. Veja os testes para exemplos.
(define (combine L1 L2 [listi '()])
  (if (empty? L1)
      (reverse listi)
      (if (empty? L2)
          (reverse listi)
          (combine (rest L1) (rest L2)
                   (cons (list(first L1)(first L2)) listi)
                ))))
;;(trace combine)

(define-test-suite test-combine
  (test-equal? "listas de mesmo tamanho"
               (combine '(1 2 3) '(10 20 30))  '((1 10) (2 20) (3 30)))
  
  (test-equal? "listas de tamanho diferente"
               (combine '(1)     '(55 33 11))  '((1 55)))
  
  (test-equal? "primeira lista vazia"
               (combine '()      '(1 2 3))     '())
  
  (test-equal? "segunda lista vazia"
               (combine '(1 2 3) '())          '())
  
  (test-equal? "segunda lista menor"
               (combine '(4 5 6) '(22 33))     '((4 22) (5 33))))


;; --- Questão 6 ----------------------------

;; Antes de trabalhar com conjuntos, é interessante ter algumas funções de apoio.

;; Além da falta de itens duplicados, outra característica dos conjuntos é a
;; ausência de ordem. As listas (1 2 3), (3 1 2), (2 3 1) etc todas representam
;; o mesmo conjunto. Por isso, não podemos usar equal? para testar igualdade de
;; conjuntos.

;; Mesmo nos testes, podemos ter diferentes implementações das operações de conjuntos,
;; ambas corretas, mas que retornam os elementos em uma ordem diferente (por
;; exemplo (uniao '(1 2 5) (2 5 3)) pode retornar (1 2 3 5) ou (3 2 1 5), ambos
;; os resultados corretos).

;; Escreva uma função conjunto=? tal que
;; (conjunto=? c1 c2) retorna #t se c1 e c2 contêm os mesmos elementos, não
;; necessariamente na mesma ordem, e #f caso exista algum elemento que pertence
;; a um mas não a outro.
(define (conjunto=? c1 c2)
  (if (empty? c1)
      (if (empty? c2)
          #t
          #f)
      (if (pertence? (first c1) c2)
          (if (empty? (rest c1))
              #t
              (conjunto=? (rest c1)c2))
          #f
  )))
;;(trace conjunto=?)

;; A função definida anteriormente já era Tail Recursive

(define-test-suite test-conjunto=?
  (test-true  "conjuntos vazios"        (conjunto=? '() '()))
  (test-false "vazio e unitário"        (conjunto=? '() '(1)))
  (test-true  "conjs. unitários"        (conjunto=? '(1) '(1)))
  (test-true  "iguais, mesma ordem"     (conjunto=? '(1 2 3) '(1 2 3)))
  (test-true  "iguais, ordem diferente" (conjunto=? '(1 2 3) '(1 3 2)))
  (test-true  "ordem diferente"         (conjunto=? '(2 1 3) '(2 3 1)))
  (test-false "(1 2 3) e (1 2 5)"       (conjunto=? '(1 2 3) '(1 2 5)))
  (test-false "(3 2 1) e (1 3 7)"       (conjunto=? '(3 2 1) '(1 3 7))))


;; --- Questão 7 ----------------------------

;; Outra função de apoio que pode ser útil é uma que, dada uma
;; lista qualquer (podendo conter itens duplicados) retorna uma lista válida como
;; conjunto, sem itens duplicados. Podemos chamar essa função remove-duplicatas.

;; Escreva a função remove-duplicatas tal que
;; (remove-duplicatas lst) retorna uma lista com os mesmos elementos de lst mas
;; sem que nenhum item ocorra mais de uma vez.

(define (remove-duplicatas lst [listi '()])
  (if (empty? lst)
      (reverse listi)
      (remove-duplicatas (remove-todos (first lst)(rest lst)) (cons (first lst) listi))))



(trace remove-duplicatas)

;; Um outro nome para a mesma função poderia ser lista->conjunto, enfatizando a
;; sua aplicação na criação de conjuntos a partir de listas. Nesse caso podemos
;; definir um sinônimo para a mesma função acima
(define lista->conjunto remove-duplicatas)

;; Note que usamos conjunto=? nos testes, caso contrário funções que retornassem
;; elementos em ordens diferentes não passariam
(define-test-suite test-remove-duplicatas
  (test-true "sem duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 4 5)) '(1 2 3 4 5)))
  
  (test-true "lista vazia"
             (conjunto=? (remove-duplicatas '()) '()))
  
  (test-true "várias duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 2 3 5)) '(1 2 3 5)))
  
  (test-true "apenas um elemento"
             (conjunto=? (lista->conjunto   '(5 5 5 5 5 5)) '(5)))
  
  (test-true "mais repetições"
             (conjunto=? (lista->conjunto '(1 2 3 1 2 3 1 2 3 1 2 3)) '(1 2 3))))

  
;; --- Questão 7 ----------------------------

;; Agora vamos implementar as operações de conjuntos implementados com listas.

;; Escreva a função uniao tal que
;; (uniao c1 c2) retorna um conjunto contendo os elementos de c1 e c2, sem duplicações.
(define (uniao c1 c2 [listi '()])
  (cond  [(and (empty? c1) (empty? c2)) (remove-duplicatas(reverse listi))]
         [(empty? c1 ) c2]
         [(empty? c2) c1]
         [else (uniao (rest c1) (rest c2) (cons (first c1)(cons (first c2) listi )))]
   
))
;;(trace uniao)

;; Dica: com o que vimos até agora tem pelo menos duas maneiras de escrever essa função.
;; Uma forma é uma função recursiva que tem que eliminar itens duplicados a cada passo.
;; Outra forma seria combinar os dois conjuntos primeiro e remover as duplicatas só no
;; final. É interessante (mas opcional) tentar fazer das duas formas.

(define-test-suite test-uniao
  (test-true "Vazio é elemento neutro 1"
             (conjunto=? (uniao '() '(1 2 3))  '(1 2 3)))
  
  (test-true "Vazio é elemento neutro 2"
             (conjunto=? (uniao '(4 5 6) '())  '(4 5 6)))
  
  (test-true "União de vazios"
             (conjunto=? (uniao '() '())  '()))
  
  (test-true "Sem elementos em comum"
             (conjunto=? (uniao '(1 2 3) '(4 5 6))  '(1 2 3 4 5 6)))
  
  (test-true "Com elementos em comum"
             (conjunto=? (uniao '(1 4 5) '(4 5 6))  '(1 4 5 6))))


;; --- Questão 8 -----------------------

;; Escreva uma função interseccao tal que
;; (interseccao c1 c2) retorna um conjunto contendo os elementos que ocorrem
;; em ambos c1 e c2
(define (interseccao c1 c2 [listi '()])
  (cond [(empty? c1) (reverse listi)]
        [(empty? c2) '()]
        [(pertence? (first c1) c2) (interseccao (rest c1) c2 (cons (first c1) listi ))]
        [ else (interseccao (rest c1) c2 listi)]
   ))

;;(trace interseccao)

(define-test-suite test-interseccao
  (test-equal? "Conjuntos vazios"        (interseccao '()      '())      '())
  (test-equal? "Intersecção com vazio 1" (interseccao '(1 2 3) '())      '())
  (test-equal? "Intersecção com vazio 2" (interseccao '()      '(11 22)) '())
  (test-equal? "Sem elementos comuns"    (interseccao '(1 2 3) '(11 22)) '())

  (test-true "Um elemento em comum"
             (conjunto=? (interseccao '(1 2 3) '(11 1 121))  '(1)))

  (test-true "Vários elementos em comum"
             (conjunto=? (interseccao '(1 3 5 7 9 11)
                                      '(11 3 1 13 17))
                         '(1 3 11)))

  (test-true "Mesmo conjunto"
             (conjunto=? (interseccao '(1 2 3 4 5) '(5 4 3 2 1))
                         '(1 2 3 4 5))))


;; --- Questão 9 -----------------------

;; Escreva uma função diferenca tal que
;; (diferenca c1 c2) retorna um conjunto que tem todos os elementos de c1 que
;; não pertencem a c2. Por exemplo, (diferenca '(1 3 5 7) '(3 7)) deve retornar
;; '(1 5) (não necessariamente nesta ordem).
(define (diferenca c1 c2 [listi '()])
  (cond [(empty? c1) (reverse listi)]
        [(empty? c2) c1]
        [(pertence? (first c1) c2) (diferenca (rest c1) c2 listi)]
        [ else (diferenca (rest c1) c2 (cons (first c1) listi ))]
   ))
;;(trace diferenca)

(define-test-suite test-diferenca
  (test-equal? "Conjuntos vazios"        (diferenca '()      '())      '())
  (test-equal? "Intersecção com vazio 1" (diferenca '(1 2 3) '())      '(1 2 3))
  (test-equal? "Intersecção com vazio 2" (diferenca '()      '(11 22)) '())
  (test-equal? "Sem elementos comuns"    (diferenca '(1 2 3) '(11 22)) '(1 2 3))

  (test-true "Um elemento em comum"
             (conjunto=? (diferenca '(1 2 3) '(11 1 121))  '(2 3)))

  (test-true "Vários elementos em comum"
             (conjunto=? (diferenca '(1 3 5 7 9 11)
                                      '(11 3 1 13 17))
                         '(5 7 9)))

  (test-true "Mesmo conjunto"
             (conjunto=? (diferenca '(1 2 3 4 5) '(5 4 3 2 1))
                         '())))

;; Para esta função, escreva também um conjunto de testes, e adicione a suite de 
;; testes criados à execução de todos os testes, abaixo. Você pode escrever os
;; testes antes ou depois de implementar a função.


;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             test-remove-primeiro
             test-remove-todos
             ;;test-remove-tipos
             test-pertence?
             test-combine
             test-conjunto=?
             test-remove-duplicatas
             test-uniao
             test-interseccao
             test-diferenca
             ))


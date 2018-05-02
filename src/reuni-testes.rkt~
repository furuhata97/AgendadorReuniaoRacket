#lang racket

;; Não é necessário editar este arquivo.
;; Você deve editar o arquivo reuni.rkt
;;
;; Veja o arquivo reuni.rkt para a descrição do que cada função deve fazer.
;;
;; As funções com nomes que começam com check são responsáveis por fazer os
;; testes. Nestas funções o último parâmetro refere-se ao valor esperado que a
;; função que está sendo testada deve retornar.
;;
;; Você pode escolher quais testes serão executados alterando a definição de
;; test-suite-reuni.

(require rackunit/text-ui rackunit "reuni.rkt")

(define test-suite-reuni
  (test-suite
   "Reuni"
   test-suite-intersecao
   test-suite-encontrar-dispo-em-comum
   test-suite-encontrar-dispo-semana-em-comum))

(define test-suite-intersecao
  (test-suite
   "Função interseção"

   (test-case
    "Um intervalo está totalmente contido em outro"
    (check-intersecao (horario 07 30) (horario 12 45)              ; 7:30 -------------- 12:45
                      (horario 09 20) (horario 12 00)              ;       9:20 - 12:00
                      (intervalo (horario 09 20) (horario 12 00))));       9:20 - 12:00
   (test-case
    "Um intervalo está parcialmente contido em outro"
    (check-intersecao (horario 07 30) (horario 12 45)              ; 7:30 ------- 12:45
                      (horario 09 20) (horario 14 00)              ;       9:20 -------- 14:00
                      (intervalo (horario 09 20) (horario 12 45))));       9:20 - 12:45

   (test-case
    "Intervalos disjuntos"
    (check-intersecao (horario 07 30) (horario 12 45)              ; 7:30 - 12:45
                      (horario 13 30) (horario 14 00)              ;               13:00 - 14:00
                      intervalo-vazio))                            ;       vazio

   (test-case
    "Mesmo intervalo"
    (check-intersecao (horario 07 00) (horario 08 00)                ; 7:00 - 8:00
                      (horario 07 00) (horario 08 00)                ; 7:00 - 8:00
                      (intervalo (horario 07 00) (horario 08 00)))))); 7:00 - 8:00


;; Definições de algumas disponibilidades que são usadas no teste da função
;; encontrar-dispo-em-comum
;; a |7:30 ----------------------------------------------------------------------------- 18:00|
;; b        |8:32 ------ 9:45|  |10:20 - 11:15|  |13:30 ------------------------- 17:00|
;; c             |9:00 ---------------------------------- 14:21|  |15:29 - 16:12|
;; b ∩ c = d     |9:00 - 9:45|  |10:20 - 11:15|  |13:30 - 14:21|  |15:29 - 16:12|
;; e |7:30 - 8:32|      |9:46 -- 10:19| |11:16 -- 13:29| |14:22 -- 15:28| |16:13 ------- 18:00|
;; e ∩ d = vazio

(define dispo-a (list (intervalo (horario 07 30) (horario 18 00))))

(define dispo-b (list (intervalo (horario 08 32) (horario 09 45))
                      (intervalo (horario 10 20) (horario 11 15))
                      (intervalo (horario 13 30) (horario 18 00))))

(define dispo-c (list (intervalo (horario 09 00) (horario 14 21))
                      (intervalo (horario 15 29) (horario 16 12))))

(define dispo-d (list (intervalo (horario 09 00) (horario 09 45))
                      (intervalo (horario 10 20) (horario 11 15))
                      (intervalo (horario 13 30) (horario 14 21))
                      (intervalo (horario 15 29) (horario 16 12))))

(define dispo-e (list (intervalo (horario 07 30) (horario 08 32))
                      (intervalo (horario 09 46) (horario 10 19))
                      (intervalo (horario 11 16) (horario 13 29))
                      (intervalo (horario 14 22) (horario 15 28))
                      (intervalo (horario 16 13) (horario 18 00))))

(define test-suite-encontrar-dispo-em-comum
  (test-suite
   "Função encontrar-dispo-em-comum"

   (test-case
    "Interseção genérica"
    (check-encontrar-dispo-em-comum dispo-b dispo-c dispo-d)
    (check-encontrar-dispo-em-comum dispo-c dispo-b dispo-d))

   (test-case
    "Interseção de disponibilidades iguais"
    (check-encontrar-dispo-em-comum dispo-a dispo-a dispo-a)
    (check-encontrar-dispo-em-comum dispo-b dispo-b dispo-b)
    (check-encontrar-dispo-em-comum dispo-c dispo-c dispo-c)
    (check-encontrar-dispo-em-comum dispo-d dispo-d dispo-d)
    (check-encontrar-dispo-em-comum dispo-e dispo-e dispo-e))

   (test-case
    "Interseção sem nada em comum"
    (check-encontrar-dispo-em-comum dispo-e dispo-d empty))

   (test-case
    "Interseção de uma disponibilidade contida totalmente em outra"
    (check-encontrar-dispo-em-comum dispo-a dispo-b dispo-b)
    (check-encontrar-dispo-em-comum dispo-a dispo-c dispo-c)
    (check-encontrar-dispo-em-comum dispo-a dispo-d dispo-d)
    (check-encontrar-dispo-em-comum dispo-a dispo-e dispo-e))))


;; Definições de algumas disponibilidades semanais que são usadas no teste da
;; função encontrar-dispo-semana-em-comum
;;
;; Estas definições são equivalentes aos arquivos no diretório testes.
;;
;; Para facilitar a compreensão da resposta esperada, você pode fazer um
;; "desenho" e calcular as interseções manualmente.

(define dispo-semana-livre
  (list (list "dom" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "seg" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "ter" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "qua" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "qui" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "sex" (list (intervalo (horario 0 0) (horario 23 59))))
        (list "sab" (list (intervalo (horario 0 0) (horario 23 59))))))

(define dispo-semana-ocupado '())

(define dispo-semana-a
  (list (list "seg" (list (intervalo (horario 08 30) (horario 10 30))
                          (intervalo (horario 14 03) (horario 16 00))
                          (intervalo (horario 17 10) (horario 18 10))))
        (list "ter" (list (intervalo (horario 13 30) (horario 15 45))))
        (list "qua" (list (intervalo (horario 11 27) (horario 13 00))
                          (intervalo (horario 15 00) (horario 19 00))))
        (list "sex" (list (intervalo (horario 07 30) (horario 11 30))
                          (intervalo (horario 13 30) (horario 14 00))
                          (intervalo (horario 15 02) (horario 16 00))
                          (intervalo (horario 17 20) (horario 18 30))))))
(define dispo-semana-b
  (list (list "seg" (list (intervalo (horario 14 35) (horario 17 58))))
        (list "ter" (list (intervalo (horario 08 40) (horario 10 30))
                          (intervalo (horario 13 31) (horario 15 13))))
        (list "qui" (list (intervalo (horario 08 30) (horario 15 30))))
        (list "sex" (list (intervalo (horario 14 07) (horario 15 00))
                          (intervalo (horario 16 00) (horario 17 30))
                          (intervalo (horario 19 00) (horario 22 00))))))
(define dispo-semana-c
  (list (list "seg" (list (intervalo (horario 10 00) (horario 12 00))
                          (intervalo (horario 15 30) (horario 17 30))))
        (list "sex" (list (intervalo (horario 10 00) (horario 12 00))
                          (intervalo (horario 15 30) (horario 17 30))))))

(define dispo-semana-00:01-a-b
  (list (list "seg" (list (intervalo (horario 14 35) (horario 16 00))
                          (intervalo (horario 17 10) (horario 17 58))))
        (list "ter" (list (intervalo (horario 13 31) (horario 15 13))))
        (list "sex" (list (intervalo (horario 17 20) (horario 17 30))))))

(define dispo-semana-01:00-a-b
  (list (list "seg" (list (intervalo (horario 14 35) (horario 16 00))))
        (list "ter" (list (intervalo (horario 13 31) (horario 15 13))))))

(define dispo-semana-00:30-a-c
  (list (list "seg" (list (intervalo (horario 10 00) (horario 10 30))
                          (intervalo (horario 15 30) (horario 16 00))))
        (list "sex" (list (intervalo (horario 10 00) (horario 11 30))
                          (intervalo (horario 15 30) (horario 16 00))))))

(define dispo-semana-00:20-a-b-c
  (list (list "seg" (list (intervalo (horario 15 30) (horario 16 00))
                          (intervalo (horario 17 10) (horario 17 30))))))

(define test-suite-encontrar-dispo-semana-em-comum
  (test-suite
   "Função encontrar-dispo-semana-em-comum"

   (test-case
    "Uma disponibilidade semanal"
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-a)
                                           dispo-semana-a)
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-b)
                                           dispo-semana-b)
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-c)
                                           dispo-semana-c)
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-livre)
                                           dispo-semana-livre)
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-ocupado)
                                           dispo-semana-ocupado))

   (test-case
    "Sem disponibilidade em comum menor que o tempo"
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-a dispo-semana-b)
                                           dispo-semana-00:01-a-b))

   (test-case
    "Disponibilidade em comum com tempo menor"
    (check-encontrar-dispo-semana-em-comum (horario 01 00)
                                           (list dispo-semana-a dispo-semana-b)
                                           dispo-semana-01:00-a-b)
    (check-encontrar-dispo-semana-em-comum (horario 00 30)
                                           (list dispo-semana-a dispo-semana-c)
                                           dispo-semana-00:30-a-c)
    (check-encontrar-dispo-semana-em-comum (horario 00 20)
                                           (list dispo-semana-a dispo-semana-b dispo-semana-c)
                                           dispo-semana-00:20-a-b-c))
   (test-case
    "Não existe disponibilidade em comum se alguém está sempre ocupado"
    (check-encontrar-dispo-semana-em-comum (horario 00 01)
                                           (list dispo-semana-a dispo-semana-b dispo-semana-ocupado)
                                           empty))))

;; Funções auxiliares
(define (check-intersecao ai af bi bf esperado)
  (define a (intervalo ai af))
  (define b (intervalo bi bf))
  ; não importa a ordem dos parâmetros, a interseção é a mesma
  (check-equal? (intervalo-intersecao a b) esperado)
  (check-equal? (intervalo-intersecao b a) esperado))

(define (check-encontrar-dispo-em-comum dispo-a dispo-b esperado)
  ; não importa a ordem dos parâmetros, a interseção é a mesma
  (check-equal? (encontrar-dispo-em-comum dispo-a dispo-b) esperado)
  (check-equal? (encontrar-dispo-em-comum dispo-b dispo-a) esperado))

(define (check-encontrar-dispo-semana-em-comum tempo dispos esperado)
  (check-equal? (encontrar-dispo-semana-em-comum tempo dispos) esperado))

;; Executa os testes
(define (executar-testes)
  (run-tests test-suite-reuni)
  (void))

(executar-testes)

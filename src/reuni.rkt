#lang racket

;; Este programa encontra horários disponíveis que sejam comuns entre vários
;; horários especificados e que tenham um tamanho mínimo especificado.
;;
;; ** Conceitos **
;;  Horário
;;    Um momento no tempo, definido em termos da hora e minutos
;;  Intervalo (abreviado inter)
;;    Um intervalo no tempo, tem um horário de início e um horário de fim
;;  Disponibilidade do dia (abreviado dispo)
;;    Uma lista de intervalos que estão disponíveis em um determinado dia
;;  Disponibilidade semanal (abreviado dispo-semana)
;;    Uma lista com as disponibilidades de cada dia
;;  Lista de associações
;;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;;    elemento do par é chamado de chave e o segundo elemento é chamado de
;;    valor. Uma lista de associações é uma maneira simples de implementar uma
;;    tabela associativa (dicionário).  Ex: o dicionário
;;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado pela lista associativa
;;    (list (list 1 4) (list 20 12) (list 6 70)).
;;    A função assoc é utilizada para consultar uma lista associativa.
;;
;; ** Formatação de entrada e saída **
;; Toda operação de entrada e saída deve ser feita respeitando essas
;; formatações. A sua implementação não precisa validar as entradas. Para os
;; testes automatizados as entradas sempre serão válidas.
;;
;;  Horário (HH:MM) (sempre 5 dígitos)
;;  Exemplos
;;     08:30 =  8 horas e 30 minutos
;;     12:07 = 12 horas e  7 minutos
;;
;;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;;  Exemplos
;;     08:30-12:07 = o intervalo tem início às 8 horas e 30 minutos e tem
;;                   o fim às 12 horas e 7 minutos
;;
;;  Dias da semana
;;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;;
;;  Disponibilidade semanal
;;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;;    intervalos disponíveis naquele dia
;;  Exemplo
;;    ter 10:20-12:00 16:10-17:30
;;    sex 08:30-11:30
;;  Observe que nem todos os dias devem estar especificados. Os dias
;;  que não têm disponibilidades não devem ser especificados.


;; exporta as funções que podem ser utilizadas em outros arquivos
(provide horario
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)
         

(struct horario (h m) #:transparent)
;; Horário representa um momento no tempo, definido em termos da hora e minutos
;;    h : Número - horas
;;    m : Número - minutos

(struct intervalo (inicio fim) #:transparent)
;; Intervalo representa um intervalo no tempo, tem um horário de início e um
;; horário de fim
;;    inicio : Horário - horário de início
;;       fim : Horário - horário de fim

;; Constante que define um intervalo vazio
(define intervalo-vazio (void))

;; Intervalo -> bool
;; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (if (intervalo-vazio? b) intervalo-vazio
      (let ([inicio-a (+(* 3600 (horario-h (intervalo-inicio a)))(* 60 (horario-m (intervalo-inicio a))))]
        [fim-a (+(* 3600 (horario-h (intervalo-fim a)))(* 60 (horario-m (intervalo-fim a))))]
        [inicio-b (+(* 3600 (horario-h (intervalo-inicio b)))(* 60 (horario-m (intervalo-inicio b))))]
        [fim-b (+(* 3600 (horario-h (intervalo-fim b)))(* 60 (horario-m (intervalo-fim b))))]
        [hora-inicio-a (horario-h (intervalo-inicio a))]
        [hora-fim-a (horario-h (intervalo-fim a))]
        [minutos-inicio-a (horario-m (intervalo-inicio a))]
        [minutos-fim-a (horario-m (intervalo-fim a))]
        [hora-inicio-b (horario-h (intervalo-inicio b))]
        [hora-fim-b (horario-h (intervalo-fim b))]
        [minutos-inicio-b (horario-m (intervalo-inicio b))]
        [minutos-fim-b (horario-m (intervalo-fim b))])
    (cond
      [(and (< inicio-a inicio-b) (> inicio-b fim-a)) intervalo-vazio]
      [(and (< inicio-a inicio-b) (< inicio-b fim-a) (<= fim-a fim-b)) (intervalo (horario hora-inicio-b minutos-inicio-b) (horario hora-fim-a minutos-fim-a))]
      [(and (< inicio-a inicio-b) (< inicio-b fim-a) (> fim-a fim-b)) (intervalo (horario hora-inicio-b minutos-inicio-b) (horario hora-fim-b minutos-fim-b))]
      [(and (< inicio-b inicio-a) (> inicio-a fim-b)) intervalo-vazio]
      [(and (< inicio-b inicio-a) (< inicio-a fim-b) (<= fim-b fim-a)) (intervalo (horario hora-inicio-a minutos-inicio-a) (horario hora-fim-b minutos-fim-b))]
      [(and (< inicio-b inicio-a) (< inicio-a fim-b) (> fim-b fim-a)) (intervalo (horario hora-inicio-a minutos-inicio-a) (horario hora-fim-a minutos-fim-a))]
      [(and (= inicio-a inicio-b) (< fim-a fim-b)) (intervalo (horario hora-inicio-b minutos-inicio-b) (horario hora-fim-a minutos-fim-a))]
      [(and (= inicio-a inicio-b) (< fim-b fim-a)) (intervalo (horario hora-inicio-b minutos-inicio-b) (horario hora-fim-b minutos-fim-b))]
      [(and (= inicio-a inicio-b) (= inicio-b fim-a)) a]))))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (cond
    [(empty? dispo-b) dispo-a]
    [(empty? dispo-a) empty]
    [else (append (verifica-intervalo (first dispo-a) dispo-b)
                (encontrar-dispo-em-comum (rest dispo-a) dispo-b))]))

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
;;
;; dispo-semana é uma lista de associações entre um dia (string) e a
;; disponibilidade naquele dia. Veja a definição de lista de associações no
;; início deste arquivo.
;;
;; Por exemplo, a disponibilidade semanal (dispo-semana):
;; ter 10:20-12:00 16:10-17:30
;; sex 08:30-11:30
;; é representada da seguinte maneira:
;; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;;                         (intervalo (hora 16 10) (hora 17 30))))
;;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;;
;; Observe que esta função recebe como parâmetro uma lista de disponibilidades
;; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
;; Veja os testes de unidade para exemplos de entrada e saída desta função
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (cond
    [(empty? dispos) empty]
    [else (let ([dias-disponiveis (cons (encontra-dias-em-comum "dom" dispos)
                         (cons(encontra-dias-em-comum "seg" dispos)
                              (cons (encontra-dias-em-comum "ter" dispos)
                                    (cons (encontra-dias-em-comum "qua" dispos)
                                          (cons (encontra-dias-em-comum "qui" dispos)
                                                (cons (encontra-dias-em-comum "sex" dispos)
                                                      (encontra-dias-em-comum "sab" dispos)))))))])
            (calcular-tempo tempo (remover-void (encontra-interseccao dias-disponiveis))))]))

(define (calcular-tempo tempo lst)
  (cond
    [(empty? lst) empty]
    [(empty? (verificar-tempo-intervalo tempo (second (first lst)))) (calcular-tempo tempo (rest lst))]
    [else (cons (list (first (first lst))(verificar-tempo-intervalo tempo (second (first lst))))
            (calcular-tempo tempo (rest lst)))]))

(define (verificar-tempo-intervalo tempo lst)
  (cond
    [(empty? lst) empty]
    [else (if (intervalo-vazio? (verificar-duracao tempo (first lst)))
              (verificar-tempo-intervalo tempo (rest lst))
              (cons (verificar-duracao tempo (first lst))
                (verificar-tempo-intervalo tempo (rest lst))))]))

(define (verificar-duracao tempo int)
  (let ([duracao (+(* 3600 (horario-h tempo))(* 60 (horario-m tempo)))]
        [inicio-int (+(* 3600 (horario-h (intervalo-inicio int)))(* 60 (horario-m (intervalo-inicio int))))]
        [fim-int (+(* 3600 (horario-h (intervalo-fim int)))(* 60 (horario-m (intervalo-fim int))))])
    (cond
      [(< (- fim-int inicio-int) duracao) intervalo-vazio]
      [(>= (- fim-int inicio-int) duracao) int])))

(define (remover-void lst)
  (cond
    [(empty? lst) empty]
    [else (cons (list (first (first lst))(fezer-remocao (second (first lst))))
            (remover-void (rest lst)))]))

(define (fezer-remocao lst)
  (cond
    [(empty? lst) empty]
    [else (if (intervalo-vazio? (first lst)) (fezer-remocao (rest lst)) (cons (first lst) (fezer-remocao (rest lst))))]))

(define (encontra-interseccao lst)
  (cond
    [(empty? lst) empty]
    [(empty? (first lst)) (encontra-interseccao (rest lst))]
    [else (cons (list (first (first (first lst))) (verificar-interseccao (first lst))) (encontra-interseccao (rest lst)))]))

(define (verificar-interseccao lst)
  (cond
    [(empty? lst) empty]
    [else (encontrar-dispo-em-comum (second (first lst)) (verificar-interseccao (rest lst)))]))


(define (verifica-intervalo lst-inter lst-horarios)
  (cond
    [(empty? lst-horarios) empty]
    [else (cons (intervalo-intersecao lst-inter (first lst-horarios))
                (verifica-intervalo lst-inter (rest lst-horarios)))]))


(define (encontra-dias-em-comum dia lst)
  (cond
    [(empty? lst) empty]
    [else (if (< (length (verifica-dias-em-comum dia lst)) (length lst)) empty (begin (verifica-dias-em-comum dia lst)))]))


(define (verifica-dias-em-comum dia lst)
  (cond
    [(empty? lst) empty]
    [else (if (assoc dia (first lst))
              (begin (cons (assoc dia (first lst))
                           (verifica-dias-em-comum dia (rest lst))))
              empty)]))



;; String -> Horario
;;Recebe uma duração de tempo no formato HH:MM e devolve um horário
(define (ler-horario tempo)
  (horario (string->number(substring tempo 0 2))
           (string->number (substring tempo 3 5))))

;;String - > lista
;; Recebe o nome do arquivo a ser lido e devolve uma lista com a disponibbilidade do dia
(define (ler-arquivo arquivo)
  (separar-dia-dos-intervalos (port->lines (open-input-file arquivo))))

;;lista -> lista
;;Recebe uma lista com a disponibilidade do dia e cria uma lista associativa
(define (separar-dia-dos-intervalos lst)
  (cond
    [(empty? lst) lst]
    [else (let ([assoc-dia-dispo (string-split (first lst))]) (cons (list (first assoc-dia-dispo) (criar-intervalos (rest assoc-dia-dispo)))
            (separar-dia-dos-intervalos (rest lst))))]))

;;lista -> lista
;;Recebe uma lista com os intervalos do dia e devolve uma lista de Intervalo 
(define (criar-intervalos lst)
  (cond
    [(empty? lst) empty]
    [else (cons (intervalo (ler-horario (substring (first lst) 0 5))
                           (ler-horario (substring (first lst) 6 11)))
            (criar-intervalos(rest lst)))]))

;; lista -> lista
;;Recebe uma lista com os nomes dos arquivos e devolve uma lista com disponibilidades semanais
(define (ler-dispo lst)
  (cond
    [(empty? lst) lst]
    [else (cons (ler-arquivo (first lst))
            (ler-dispo (rest lst)))]))

(define (escrever-dispo dispo)
  (cond
    [(empty? dispo) ""]
    [else (string-append (first (first dispo)) " " (converte-lista-intervalos (second (first dispo))) (if(not (empty? (rest dispo))) "\n" "") (escrever-dispo (rest dispo)))]))

;;[else (string-append (list->string (map converte-number-caractere (first jogo))) (if(not (empty? (rest jogo))) "\n" "") (escrever-jogo (rest jogo)))]

(define (converte-lista-intervalos lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (converte-intervalo (first lst)) " " (converte-lista-intervalos (rest lst)))]))

(define (converte-intervalo int)
  (let ([hora-inicial (horario-h (intervalo-inicio int))]
        [minuto-inicial (horario-m (intervalo-inicio int))]
        [hora-final (horario-h (intervalo-fim int))]
        [minuto-final (horario-m (intervalo-fim int))])
    (string-append (verifica-hora hora-inicial) ":" (verifica-hora minuto-inicial) "-"
                   (verifica-hora hora-final) ":" (verifica-hora minuto-final))))

(define (verifica-hora hora)
  (if (< hora 10) (string-append "0" (number->string hora)) (number->string hora)))


;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
;; devem ter. O tempo mínimo é especificado usando a formatação de horário.
;;
;; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
;; contém uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
;; testes.
;;
;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.

(define (main args)
  (display (escrever-dispo (encontrar-dispo-semana-em-comum (ler-horario (first args)) (ler-dispo  (rest args))))))
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
;; Veja os testes de unidade para exemplos de entrada e saída desta função.

;; Nesta função, primeiramente é verificado se a lista de dispos tem tamanho 1, o que significa que a lista possui a disponibilidade de apenas 1 pessoa. Caso isso ocorra
;; é chamada a função para calcular o tempo nos intervalos desta pessoa. Caso a lista possua tamanho maior que 1, é verificado quais dias da semana as pessoas possuem em comum.
;; Caso todas as pessoas possuam disponibilidade no dia desejado, será retornado uma lista com esses dias em comum. Caso pelo menos 1 não tenha disponibilidade no dia,
;; é retornado uma lista vazia. Após encontrar os dias em comum é chamada a função para calcular o tempo de duração da reunião sobre a interseção dos dias em comum.
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (cond
    [(empty? dispos) empty]
    [(= (length dispos) 1) (calcular-tempo tempo (first dispos))]
    [else (let ([dias-disponiveis (cons (encontra-dias-em-comum "dom" dispos)
                         (cons(encontra-dias-em-comum "seg" dispos)
                              (cons (encontra-dias-em-comum "ter" dispos)
                                    (cons (encontra-dias-em-comum "qua" dispos)
                                          (cons (encontra-dias-em-comum "qui" dispos)
                                                (cons (encontra-dias-em-comum "sex" dispos)
                                                      (encontra-dias-em-comum "sab" dispos)))))))])
            (calcular-tempo tempo (encontra-interseccao dias-disponiveis)))]))

; ;Horario, list dispo-semana -> list dispo-semana
;; Recebe um horário de duração da reunião e uma lista com as disponibilidades de horários em cada dia da semana e devolve uma lista com as intersecções
;; de horários dentro tempo estipulado
(define (calcular-tempo tempo lst)
  (cond
    [(empty? lst) empty]
    [(empty? (verificar-tempo-intervalo tempo (second (first lst)))) (calcular-tempo tempo (rest lst))]
    [else (cons (list (first (first lst))(verificar-tempo-intervalo tempo (second (first lst))))
            (calcular-tempo tempo (rest lst)))]))

;;Horario, list dispo-dia -> list dispo-dia
;;Recebe um Horario e uma lista com as disponibilidades do dia
(define (verificar-tempo-intervalo tempo lst)
  (cond
    [(empty? lst) empty]
    [else (if (intervalo-vazio? (verificar-duracao tempo (first lst)))
              (verificar-tempo-intervalo tempo (rest lst))
              (cons (verificar-duracao tempo (first lst))
                (verificar-tempo-intervalo tempo (rest lst))))]))


;; Horario, Intervalo - > Intervalo
;; Recebe um Horario e um Intervalo e verifica se o intervalo tem duração maior ou igual ao Horario, em caso positivo, o Intervalo é retornado, caso contrário,
;; é retornado o intervalo-vazio
(define (verificar-duracao tempo int)
  (let ([duracao (+(* 3600 (horario-h tempo))(* 60 (horario-m tempo)))]
        [inicio-int (+(* 3600 (horario-h (intervalo-inicio int)))(* 60 (horario-m (intervalo-inicio int))))]
        [fim-int (+(* 3600 (horario-h (intervalo-fim int)))(* 60 (horario-m (intervalo-fim int))))])
    (cond
      [(< (- fim-int inicio-int) duracao) intervalo-vazio]
      [(>= (- fim-int inicio-int) duracao) int])))


;; list dispo -> list dispo
;; Recebe uma lista contendo as disponibilidades de cada pessoa nos dias em que elas possuem em comum e retorna as disponibilidades que possuem intersecção.
;; Cada elemento da lista recebida nesta função representa uma lista com as disponibilidades em comum de cada dia. Por exemplo, o primeiro elemento representa as
;; disponibilidades de domingo, o segundo representa as disponibilidades de segunda, etc. 
;; Primeiro é verificado se o primeiro elemento desta lista é vazio. Caso seja, significa que não foi encontrado disponibilidade para todas as pessoas neste dia, logo este
;; dia é descartado e é feita uma chamada recursiva a esta função passando o resto da lista. Quando o primeiro elemento não for vazio, dia-semana recebe a string representando
;; o dia da semana a ser verificado. Após isso é criada uma lista associativa com esse dia da semana e com o resultado da função verificar-interseccao passando o primeiro
;; elemento da lista. O resultado disso recebe um cons com a chamada recursiva de encontra-interseccao passando o resto da lista.
(define (encontra-interseccao lst)
  (cond
    [(empty? lst) empty]
    [(empty? (first lst)) (encontra-interseccao (rest lst))]
    [else (let ([dia-semana (first(first(first lst)))])
            (cons (list dia-semana (verificar-interseccao (first lst))) (encontra-interseccao (rest lst))))]))

;; list dias-comum -> list dias-comum-interseccao
;; Recebe uma lista contendo as disponibilidades de todas as pessoas em um determinado dia da semana
;; A função faz uma chamada a função encontrar-dispo-em-comum, passando o intervalo do primeiro elemento e a chamada recursiva de verificar-interseccao passando o resto da lista.
;; Como cada elemento da lista é uma lista associativa, o first de cada elemento representa a string do dia da semana e o second representa os intervalos deste dia.
;; Como sempre ocorre uma chamada recursiva, a lista vai sendo percorrida até chegar em seu último intervalo. Assim que isto ocorrer, a função encontrar-dispo-em-comum irá
;; encontrar a intersecção entre este intervalo e o anterior, e irá fazendo seus retornos até que a intersecção de todos os intervalos seja calculada.
(define (verificar-interseccao lst)
  (cond
    [(empty? lst) empty]
    [else (encontrar-dispo-em-comum (second (first lst)) (verificar-interseccao (rest lst)))]))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
;; Recebe uma lista de intervalos a e b. É então feita uma chamada para a função verifica-intervalo passando o primeiro intervalo de dispo-a e a lista de intervalos dispo-b.
;; O retorno de verifica-intervalo será a intersecção entre o primeiro intervalo de dispo-a e todos os intervalos de dispo-b. Em seguida é feita uma chamada recursiva
;; a encontrar-dispo-em-comum passando o resto de dispo-a e a lista dispo-b. O processo irá se repetir, encontrando a intersecção entre o primeiro elemento do resto de dispo-a
;; e a lista de intervalos de dispo-b. O resultado de tudo isso recebe um append. Quando não houver intersecção entre os intervalos, haverá um void. É aplicado um filter para
;; remover o void
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (cond
    [(empty? dispo-b) dispo-a]
    [(empty? dispo-a) empty]
    [else (filter intervalo? (append (verifica-intervalo (first dispo-a) dispo-b)
                (encontrar-dispo-em-comum (rest dispo-a) dispo-b)))]))

;; Intervalo, list Intervalo -> list Intervalo
;; Recebe um intervalo e uma lista de intervalos. É calculada a intersecção entre o intervalo e cada elemento da lista de intervalos
(define (verifica-intervalo inter lst-intervalo)
  (cond
    [(empty? lst-intervalo) empty]
    [else (cons (intervalo-intersecao inter (first lst-intervalo))
                (verifica-intervalo inter (rest lst-intervalo)))]))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
;; Transforma os dois intervalos em segundos, então é feita uma comparação entre esses dois intervalos. O resultado da intersecção depende do resultado das comparações.
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
      [(and (= inicio-a inicio-b) (= fim-b fim-a)) a]))))

;; String, list dispo -> list dias-comum
;; Recebe uma string representando o dia procurado e a lista com as disponibilidades de todas as pessoas.
;; Primeiro é feito um map que retorna os dias em comum entre as diferentes dispos. Quando não houver dia em comum é retornado #f. Depois é feito um filter para remover
;; todos os #f. O resultado é armazenado em dias-comum.
;; Após isso, é verificado se o tamanho de dias-comum é menor que o tamanho da lista original, caso seja, é retornado vazio, caso não seja, é retornado dias-comum.
;; Essa verificação de tamanho ocorre para ver se a quantidade de dias em comum encontrados é igual a quantidade de pessoas. Por exemplo:
;; caso deseje-se verificar a disponibilidade de 2 pessoas A e B em uma quarta, e apenas a pessoa A possua horários na quarta, dias-comum terá tamanho 1, pois só
;; encontrou quarta-feira na disponibilidade da pessoa A. Como a lista original terá tamanho 2, será retornado vazio, pois isso significa que não haverá
;; dispoibilidade em comum na quarta.
(define (encontra-dias-em-comum dia lst)
  (cond
    [(empty? lst) empty]
    [else (let ([dias-comum (filter list? (map(lambda(lst)(assoc dia lst))lst))])
            (if (< (length dias-comum) (length lst)) empty dias-comum))]))



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
    [else (let ([assoc-dia-dispo (string-split (first lst))])
            (cons (list (first assoc-dia-dispo) (criar-intervalos (rest assoc-dia-dispo)))
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

;; List dispo -> string
;; Recebe a lista com as disponibilidades e as imprime na tela
(define (escrever-dispo dispo)
  (cond
    [(empty? dispo) ""]
    [else (string-append (first (first dispo)) " " (converte-lista-intervalos (second (first dispo))) "\n" (escrever-dispo (rest dispo)))]))

;; List Intervalo -> string
;; Recebe uma lista com os intervalos disponíveis em um certo dia e os transforma em string
(define (converte-lista-intervalos lst)
  (cond
    [(empty? lst) ""]
    [(empty? (rest lst)) (converte-intervalo (first lst))]
    [else (string-append (converte-intervalo (first lst)) " " (converte-lista-intervalos (rest lst)))]))

;; Intervalo -> string
;; Recebe um intervalo e o transforma em string
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
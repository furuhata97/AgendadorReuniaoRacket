#lang racket

;; Não é necessário editar este arquivo.
;; Você deve editar o arquivo reuni.rkt

;; Este arquivo é executado pelo testador.

(require "reuni.rkt")

(main (vector->list (current-command-line-arguments)))
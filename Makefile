RACKET=racket
PYTHON=python2.7

.PHONY: teste-unitario teste-funcional teste zip limpar

teste: teste-unitario teste-funcional

teste-unitario:
	@echo Executando testes unitários
	$(RACKET) src/reuni-testes.rkt

teste-funcional:
	@echo Executando testes funcionais
	@$(PYTHON) testador.py testes/casos racket src/reuni-main.rkt

zip: src.zip

src.zip: $(shell find src/ -type f ! -wholename 'src/compiled*' ! -name '*~')
	@echo Criando arquivo src.zip.
	@zip --quiet src.zip -r $?
	@echo Arquivo src.zip criado.

limpar:
	@echo Removendo src.zip e src/compiled
	@rm -rf src.zip src/compiled/

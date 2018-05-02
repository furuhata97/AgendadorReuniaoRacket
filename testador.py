#!/usr/bin/env python2.7

def main(arquivo_casos, prog):
    testar(prog, ler_casos(arquivo_casos))

def testar(prog, casos):
    total = 0
    falhas = 0
    for caso in casos:
        params, esperado = caso
        nome = " ".join(prog + params)
        print nome,
        ok, obtido = executar_programa(prog, params)
        obtido = obtido.split('\n')
        if obtido:
            obtido = obtido[:-1]
        total += 1
        if not ok or obtido != esperado:
            falhas += 1
            print 'Falha'
            print 'Esperado:\n', "\n".join(esperado)
            print 'Obtido:\n', "\n".join(obtido)
            print '---'
        else:
            print 'OK'
    print 'Passou em', (total - falhas), 'teste(s) do total de', total

def ler_casos(arquivo):
    casos = []
    with open(arquivo) as f:
        ultimo = None
        params = None
        esperado = []
        for linha in f.readlines():
            linha = linha.strip()
            if linha and ultimo:
                esperado.append(linha)
            else:
                if params:
                    casos.append((params,  esperado))
                params = linha.split()
                esperado = []
            ultimo = linha
    return casos

def executar_programa(prog, params):
    import subprocess
    try:
        return True, subprocess.check_output(prog + params)
    except:
        r = "Erro ao executar o programa: %s\nExecute o programa no terminal para ver o erro.\n" % " ".join(prog + params)
        return False, r

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 3:
        print 'Modo de usar: %s casos nome-do-programa [parametros]'
        sys.exit(1)
    main(sys.argv[1], sys.argv[2:])

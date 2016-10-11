function [ x, M ] = fRoots( coeficientesDoPolinomio, restoLimiteEscolhido, numeroMaximoDeIteracoes, toleranciaMinima )

    passoDaIteracao         = 1;
    grauDoPolinomio         = length( coeficientesDoPolinomio ) - 1;

    # Salvamos o valor original do polinômio para o processo de purificação.
    polinomioOriginal       = coeficientesDoPolinomio;
    grauDoPolinomioOriginal = grauDoPolinomio;

    while grauDoPolinomio > 0

        xi( passoDaIteracao )                          = fLocaliza( grauDoPolinomio, coeficientesDoPolinomio );
        [ x( passoDaIteracao ), M( passoDaIteracao ) ] = fNPolinomios(
                                                                       grauDoPolinomio,
                                                                       coeficientesDoPolinomio,
                                                                       xi( passoDaIteracao ),
                                                                       restoLimiteEscolhido,
                                                                       numeroMaximoDeIteracoes,
                                                                       toleranciaMinima
                                                                     );

        # Redução de grau pela raiz x, M vezes.
        # Criar um função de redução de grau, que receba retorne o novo para continuar o processo.
        [ grauDoPolinomio, coeficientesDoPolinomio ] = reduzirGrauDoPolinomio(
                                                                               grauDoPolinomio,
                                                                               coeficientesDoPolinomio,
                                                                               x( passoDaIteracao ),
                                                                               M( passoDaIteracao )
                                                                             );
        
        passoDaIteracao = passoDaIteracao + 1;

    end
    
    x = x.';
    M = M.';
    
end

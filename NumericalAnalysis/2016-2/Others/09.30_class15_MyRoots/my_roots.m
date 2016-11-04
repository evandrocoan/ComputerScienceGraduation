#
function [ x, M ] = my_roots( coeficientes_do_polinomio, tolerancia )

    passo_atual_de_iteracao = 1;
    grau_do_polinomio       = length( coeficientes_do_polinomio ) - 1;

    # Salvamos o valor original do polinômio para o processo de purificação.
    polinomio_original         = coeficientes_do_polinomio;
    grau_do_polinomio_original = grau_do_polinomio;

    while grau_do_polinomio > 0

        xi( passo_atual_de_iteracao ) = fLocaliza( grau_do_polinomio, coeficientes_do_polinomio );

        [ a, b ] = fNewtonPolinomios(
                                      grau_do_polinomio,
                                      coeficientes_do_polinomio,
                                      xi( passo_atual_de_iteracao ),
                                      1e-4,
                                      tolerancia
                                    );

        x(passo_atual_de_iteracao) = a;
        M(passo_atual_de_iteracao) = b;

        # Redução de grau pela raiz x, M vezes.
        # Criar um função de redução de grau, que receba retorne o novo para continuar o processo.
        [ a, b ] = reduzirGrauDoPolinomio(
                                            grau_do_polinomio,
                                            coeficientes_do_polinomio,
                                            x( passo_atual_de_iteracao ),
                                            M( passo_atual_de_iteracao )
                                         );
        
        grau_do_polinomio         = a;
        coeficientes_do_polinomio = b;
        passo_atual_de_iteracao   = passo_atual_de_iteracao + 1;

    end

end

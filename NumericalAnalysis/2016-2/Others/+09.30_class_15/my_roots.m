#
function x = my_roots( coeficientes_do_polinomio )

    passo_atual_de_iteracao = 1
    grau_do_polinomio       = length( coeficientes_do_polinomio )  - 1

    # Salvamos o valor original do polinômio para o processo de purificação.
    polinomio_original         = coeficientes_do_polinomio
    grau_do_polinomio_original = grau_do_polinomio

    while grau_do_polinomio > 0

        xi( passo_atual_de_iteracao ) = fLocaliza( grau_do_polinomio, coeficientes_do_polinomio )

        [ x(passo_atual_de_iteracao), m(passo_atual_de_iteracao) ] = fNPolinomios( grau_do_polinomio, coeficientes_do_polinomio, )

        # Redução de grau pela raiz x, M vezes
        # Criar um função de redução de grau, que receba
        #     (grau_do_polinomio, coeficientes_do_polinomio, x(passo_atual_de_iteracao) ) e retorne o novo [grau_do_polinomio,coeficientes_do_polinomio] para
        # continuar o processo.
        [grau_do_polinomio, coeficientes_do_polinomio] = reducao_de_grau(
                                                                            grau_do_polinomio,
                                                                            coeficientes_do_polinomio,
                                                                            x( passo_atual_de_iteracao ),
                                                                            M( passo_atual_de_iteracao )
                                                                        )
        
    end

end

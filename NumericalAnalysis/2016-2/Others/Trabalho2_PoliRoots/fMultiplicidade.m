function M = fMultiplicidade(R, restoLimiteEscolhido)
    % M = número de restos próximos de zero < restoLimiteEscolhido
    M = 1;
    soma = abs(R(1)) + abs(R(2));
    while soma < restoLimiteEscolhido
        M = M+1;
        soma = soma + abs(R(M+1));
    end
end

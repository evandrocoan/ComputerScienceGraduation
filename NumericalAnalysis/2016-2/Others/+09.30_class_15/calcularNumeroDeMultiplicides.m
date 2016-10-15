function M = calcularNumeroDeMultiplicides(R)
    % M = número de restos próximos de zero < restoLimiteEscolhido
    M = 1;
    soma = abs(R(1)) + abs(R(2));
    while soma < 1e-4
        M = M+1;
        soma = soma + abs(R(M+1));
    end
end

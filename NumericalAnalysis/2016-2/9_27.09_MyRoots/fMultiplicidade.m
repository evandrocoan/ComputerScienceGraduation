function M = fMultiplicidade(R)
    % M = número de restos próximos de zero < Rlimite
    Rlimite = 0.1;
    M = 1;
    soma = abs(R(1)) + abs(R(2));
    while soma < Rlimite
        M = M+1;
        soma = soma + abs(R(M+1));
    end
end
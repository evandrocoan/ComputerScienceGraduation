/*
 * Modelo de uso de variáveis globais.
 * */
recursao(100).
recursao(X) :- 
    adicionar, 
    Y is X + 1, 
    recursao(Y).

adicionar :- 
    nb_getval(contador, C), 
    NovoC is C + 1, 
    nb_setval(contador, NovoC).

testarRecursao :-
    % Ajusta o contador para 10
    nb_setval(contador, 10),

    % Executa algumas somas
    recursao(50), !,

    % Imprime os resultados
    nb_getval(contador, ContadorValor),
    write('Recurcoes: '), 
    writeln(ContadorValor).

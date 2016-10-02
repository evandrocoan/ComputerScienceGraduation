programa :- 
    open('exemplos.pl', read, X),
    current_input(Stream),
    set_input(X),
    write(X),
    set_input(Stream),
    close(X).

estrelas(0) :- 
     !.

estrelas(N) :-
    write('*'), 
    N1 is N -1, 
    estrelas(N1).

guess :- 
    write('Escreva um predicado guess(N) que incita o usuario a\n '),
    write('adivinhar o numero N. O predicado repetidamente le um numero,\n '),
    write('compara-o com N, e imprime "Muito baixo!", "Acertou!", \n '),
    write('"Muito alto!", conforme o caso, orientando o usuario na direcao\n '),
    write('certa. '),
    repeat,
		write('Entre com um numero'),
		read(X),
		(X =:= 73 ->
		    !, write('Acertou!')
		;
		    (X > 73 ->
                write('Muito alto! Aperte ";" para tentar novamente '),
                write('ou "." para sair.')
	        ;
	            write('Muito Baixo! Aperte ";" para tentar novamente '),
                write('ou "." para sair.')
	        )
		).

guess2 :-
    write('Hello guays').

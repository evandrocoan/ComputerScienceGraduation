/* Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
Orientador, Ano de Ingresso, Ano de Término, Referencias ... */
formacaoNoCursoDe( ['Pessoa9 da Silva1', 'Ciencias da Computacao', 
'ITA', 'Silva1 da Pessoa9', 2010, 2015 ] ).
formacaoNoCursoDe( ['Pessoa9 da Silva2', 'Ciencias da Computacao', 
'ITA', 'Silva2 da Pessoa8', 2003, 2007, 'Avril Lavigne', 'Demi Lovato' ] ).
formacaoNoCursoDe( ['Pessoa8 da Silva2', 'Engenharia Mecanica', 
'UFSC', 'Silva3 da Pessoa7', 2008, 2011, 'Marshall Mathers' ] ).
formacaoNoCursoDe( ['Pessoa1 da Silva1', 'Ciencias da Computacao', 
'UFSC', 'Silva9 da Pessoa1', 2007, 2012, 'Bruno Mars', 'Rock Star' ] ).
formacaoNoCursoDe( ['Pessoa2 da Silva9', 'Ciencias da Automacao', 
'UFMG', 'Silva8 da Pessoa2', 2005, 2010 ] ).
formacaoNoCursoDe( ['Pessoa3 da Silva7', 'Engenharia da Computacao', 
'USP', 'Silva7 da Pessoa3', 2009, 2015, 'Silvio Santos' ] ).
formacaoNoCursoDe( ['Pessoa4 da Silva7', 'Ciencias Biologicas', 
'UNICAMP', 'Silva6 da Pessoa4', 2002, 2012, 'Silvio Santos', 'Bruno Mars' ] ).
formacaoNoCursoDe( ['Pessoa5 da Silva7', 'Engenharia da Computacao', 
'UFSC', 'Silva5 da Pessoa5', 2000, 2005, 'Roberto Carlos' ] ).
formacaoNoCursoDe( ['Pessoa6 da Silva5', 'Ciencias Juridicas', 
'UFSC', 'Silva4 da Pessoa5', 2002, 2012 ] ).
formacaoNoCursoDe( ['Pessoa6 da Silva4', 'Ciencias Sociais', 
'ITA', 'Silva4 da Pessoa6', 2007, 2013 ], 'Demi Lovato' ). 

/* Nome, data de nascimento, cidade, telefone */
informacoesPessoais( ['Pessoa1 da Silva1', date( 1990, 1, 10 ),  
'Sao Paulo', 4837326424 ] ).
informacoesPessoais( ['Pessoa2 da Silva9', date( 1977, 2, 15 ),  
'Florianopolis', 4737326422 ] ).
informacoesPessoais( ['Pessoa3 da Silva7', date( 1960, 5, 13 ), 
'Sao Paulo', 4737326427 ] ).
informacoesPessoais( ['Pessoa4 da Silva7', date( 1930, 12, 09 ), 
'Rio de Janeiro', 4887326455 ] ).
informacoesPessoais( ['Pessoa5 da Silva7', date( 1950, 10, 30 ), 
'Rio Branco', 4887326465 ] ).
informacoesPessoais( ['Pessoa6 da Silva5', date( 1990, 1, 10 ), 
'Florianopolis', 1177326475 ] ).
informacoesPessoais( ['Pessoa6 da Silva4', date( 1981, 8, 20 ), 
'Cuiaba', 1177326825 ] ).
informacoesPessoais( ['Pessoa8 da Silva2', date( 1982, 7, 04 ), 
'Joao Pessoa', 8197326625 ] ).
informacoesPessoais( ['Pessoa9 da Silva2', date( 1986, 6, 12 ), 
'Brasilia', 8177326825 ] ).
informacoesPessoais( ['Pessoa9 da Silva1', date( 1955, 11, 20 ), 
'Florianopolis', 4887347425 ] ).


/* Questão 1 - Primeiro testo se a pessoa é a cabeça da lista.
Segundo pego o telefone dela nesta lista na quarta posição.*/
qualTelefone(Nome,Telefone) :- informacoesPessoais([Nome, _, _, Telefone]).

/* Questão 2*/
quemMora(Cidade, Nome) :- informacoesPessoais([Nome, _, Cidade, _]).

/* Questão 3 -*/


/*Primeiro carrega-se a lista armazenada no predicado 
informacoesPessoais na variável L com informacoesPessoais(L). 
Depois chama-se a função que pega o elemento que queremos na lista.*/
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).
elementoNa(X, K) :- informacoesPessoais(L), element_at(X,[_|L],K).



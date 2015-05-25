/* Para funcionar, dever selecionar o working directory para uma pasta acima
 *   da pasta Trabalho2 executando o arquivo de configuração.
 *
 * Muda para o diretório de trabalho padrão.
 *
 * irParaDiretorioPadrao :- 
 *	
 *	working_directory(_, 'D:/Evandro/Archives/Dropbox/Aplicativos/
 *                     SoftwareVersioning/2015-1_ParadigmasDeProgramacao').   
 *
 * :-irParaDiretorioPadrao.
 * */
importarTrabalho1ParteB :- ['Trabalho1/trabalho1ParteB.pl'].
:-importarTrabalho1ParteB.

/* Incluir novo dado para uma pessoa. Ao chamar este predicado se passa o 
 *   nome da pessoa que se deseja incluir novas informações profissionais. 
 *
 * Organização das informações profissionais. ################################
 * Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 * Nome Completo de Colegas como Referências...
 * */
incluirInformacoesProfissionais(Nome) :-
	write('Qual o Nome da Empresa? '), 
	write('(Insira entre aspas simples) Seguido por um ponto.'),
	read(NomeDaEmpresa),
	
    write('Qual o Nome do Cargo? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(NomeDoCargo), 
    
    write('Qual o Ano de Ingresso? '), 
    write('Seguido por um ponto.'),
    read(AnoDeIngresso), 
    
    write('Qual o Ano de Termino? '), 
    write('Seguido por um ponto.'),
    read(AnoDeTermino), 
    
    write('Quais os Colegas de Profissao? (Insira seus nomes entre \n' ), 
    write('aspas simples e separados por virgula) '),
    write('Seguido por um ponto.'),
    read(ColegasDeProfissao),

    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [informacoesProfissionais, [ Nome, NomeDaEmpresa, NomeDoCargo, 
    AnoDeIngresso, AnoDeTermino, ColegasDeProfissao ] ],
    assert(Predicado),
     
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told. 


/* Incluir novo dado para uma pessoa. Ao chamar este predicado se passa o 
 *   nome da pessoa que se deseja incluir novas informações academicas. 
 *
 * Organização das informações acadêmicas. ###################################
 * Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 * Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
incluirInformacoesAcademicas(Nome) :-
    write('Qual o Nome do Curso de Formacao? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(NomeDoCurso),
    
    write('Qual o Nome da Instituicao de ensino? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(NomeDaInstituicao),
    
    write('Qual o Nome completo do Orientador? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(NomeDoOrientador),
    
    write('Qual o Ano de Ingresso? '), 
    write('Seguido por um ponto.'),
    read(AnoDeIngresso), 
    
    write('Qual o Ano de Termino? '), 
    write('Seguido por um ponto.'),
    read(AnoDeTermino), 
    
    write('Qual o Nome completo do Orientador? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(ColegasDeReferencia),

    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [informacoesAcademicas, [Nome, NomeDoCurso,  
    NomeDaInstituicao, NomeDoOrientador, AnoDeIngresso, AnoDeTermino, 
    ColegasDeReferencia] ],
    assert(Predicado),
     
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told. 


/* Incluir novo dado para uma pessoa. Ao chamar este predicado se passa o 
 *   nome da pessoa que se deseja incluir novas informações pessoais. 
 *
 * Organização das informações pessoais. #####################################
 * Nome, data de nascimento, cidade, telefone  
 * */
incluirInformacoesPessoais(Nome) :-
    write('Qual a Dia de Nascimento? '), 
    write('(Insira separado por virgula: ano,mes,dia) Seguido por um ponto.'),
    read(DataDeNascimentoDia),
    
    write('Qual o Mes de Nascimento? '), 
    write('(Insira separado por virgula: ano,mes,dia) Seguido por um ponto.'),
    read(DataDeNascimentoMes),
    
    write('Qual o Ano de Nascimento? '), 
    write('(Insira separado por virgula: ano,mes,dia) Seguido por um ponto.'),
    read(DataDeNascimentoAno),
    
    write('Qual o nome da Cidade? '), 
    write('(Insira entre aspas simples) Seguido por um ponto.'),
    read(NomeDaCidade),
    
    write('Qual o Telefone? '), 
    write('(Insira somente numeros sem espacos) Seguido por um ponto.'),
    read(NumeroDoTelefone),
    
    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [informacoesPessoais, [ Nome, date(DataDeNascimentoAno,
    DataDeNascimentoMes, DataDeNascimentoDia),  
    NomeDaCidade, NumeroDoTelefone ] ], 
    assert(Predicado),
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told. 


/* Excluir todas as informações Profissionais de uma Pessoa passada como 
 *   parâmetro.
 * */
excluirInformacoesProfissionais(Nome) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    findall( _, retract(informacoesProfissionais([Nome|_])), _ ),
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.


/* Excluir as informações Profissionais de uma Pessoa e uma dada Empresa 
 *   passados como parâmetro.
 * */
excluirInformacoesProfissionais(Nome, Empresa) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    findall( _, retract(informacoesProfissionais([Nome|[Empresa|_]])), _ ), 
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.


/* Excluir todas as informações Academicas de uma Pessoa passada como 
 *   parâmetro.
 * */
excluirInformacoesAcademicas(Nome) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    findall( _, retract(informacoesAcademicas([Nome|_])), _ ),
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.


/* Excluir todas as informações Academicas de uma Pessoa e um dado Curso 
 *   passados como parâmetro.
 * */
excluirInformacoesAcademicas(Nome, Curso) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    findall( _, retract(informacoesAcademicas([Nome|[Curso|_]])), _ ),
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.


/* Excluir todas as informações Pessoais, Academicas e Profisionais de uma 
 *   Pessoa passada como parâmetro.
 * */
excluirInformacoesPessoais(Nome) :-
    privado_ExcluirInformacoesPessoais(Nome),
    excluirInformacoesAcademicas(Nome),
    excluirInformacoesProfissionais(Nome),
    !.

privado_ExcluirInformacoesPessoais(Nome) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    findall( _, retract(informacoesPessoais([Nome|_])), _ ),
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.


/* Altera a Cidade de uma dada Pessoa.
 * */
alterarCidade(Nome, Cidade) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    
    % cria um cópia temporária das informacoesPessoais e paga elas.
    informacoesPessoais(Info),
    dadoNaPosicao(NomeDaPessoa, Info, 0),
    NomeDaPessoa == Nome,
    findall( _, retract(informacoesPessoais([Nome|_])), _ ),
    
    % altera as informações temporárias e salva elas.
    substituidoDaPos(Cidade, 2, Info, NovaInfo),
    adicionarNoInicio('informacoesPessoais', [NovaInfo], AuxNovaInfo),
    Predicado =.. AuxNovaInfo,
    assert(Predicado), 
    !,
    
    /* Lista todas as clausulas e grava no arquivo */
    tell('Trabalho1/bancoDeDados.pl'),
    listing(informacoesAcademicas),
    listing(informacoesPessoais), 
    listing(informacoesProfissionais), 
    told.














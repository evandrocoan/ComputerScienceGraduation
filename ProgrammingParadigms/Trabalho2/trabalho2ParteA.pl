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
 * Organização das Informações Profissionais. ################################
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
    
    write('Quais os Colegas de Profissao? (Insira seus nomes entre ' ), 
    write('aspas simples e separados por virgula) '),
    write('Seguido por um ponto.'),
    read(ColegasConjunto),
    converterConjutoParaLista(ColegasConjunto, ColegasLista),
    
    Informacoes =  [ Nome, NomeDaEmpresa, NomeDoCargo, 
                                  AnoDeIngresso, AnoDeTermino, ColegasLista ],
    flatten(Informacoes, NovasInformacoes), 
    
    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [informacoesProfissionais, NovasInformacoes ],
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
 * Organização das Informações Acadêmicas. ###################################
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
    
    write('Quais os Colegas de Curso ou Professores? (Insira seus nomes ' ), 
    write('entre aspas simples e separados por virgula) '),
    read(ColegasConjunto),
    converterConjutoParaLista(ColegasConjunto, ColegasLista),
    
    Informacoes =  [Nome, NomeDoCurso,  
    NomeDaInstituicao, NomeDoOrientador, AnoDeIngresso, AnoDeTermino, 
                                                                ColegasLista],
    flatten(Informacoes, NovasInformacoes), 
    
    consult('Trabalho1/bancoDeDados.pl'), 
    Predicado =.. [informacoesAcademicas, NovasInformacoes ], 
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
 * Organização das Informações Pessoais. #####################################
 * Nome, data de nascimento, cidade, telefone  
 * */
incluirInformacoesPessoais(Nome) :-
    write('Qual a Dia de Nascimento? '), 
    write('Seguido por um ponto.'),
    read(DataDeNascimentoDia),
    
    write('Qual o Mes de Nascimento? '), 
    write('Seguido por um ponto.'),
    read(DataDeNascimentoMes),
    
    write('Qual o Ano de Nascimento? '), 
    write('Seguido por um ponto.'),
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


/* Altera o Telefone de uma dada Pessoa.
 * */
alterarTelefone(Nome, Telefone) :-
    consult('Trabalho1/bancoDeDados.pl'), 
    
    % cria um cópia temporária das informacoesPessoais e apaga elas.
    informacoesPessoais(Info),
    dadoNaPosicao(NomeDaPessoa, Info, 0),
    NomeDaPessoa == Nome,
    findall( _, retract(informacoesPessoais([Nome|_])), _ ),
    
    % altera as informações temporárias e salva elas.
    substituidoDaPos(Telefone, 3, Info, NovaInfo),
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


/* Retorna um Currículo para um dado Nome. Este currículo é uma lista 
 *   é composta pelas Informações Pessoais, uma Lista de Informações 
 *   Academicas e uma lista de Informações Profissionais. Onde cada uma das 
 *   Informações é também outra lista contendo:
 *
 *   Organização das Informações Pessoais. ###################################
 *   Nome, data de nascimento, cidade, telefone  
 *   
 *   Organização das Informações Profissionais. ##############################
 *   Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 *   Nome Completo de Colegas como Referências...
 *   
 *   Organização das Informações Acadêmicas. #################################
 *   Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 *   Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
curriculoPeloNome(Nome, Curriculo) :-
	% Carrega as informações Pessoais
    informacoesPessoais(Pessoais), 
    dadoNaPosicao(NomeDaPessoa, Pessoais, 0),
    NomeDaPessoa == Nome,
    
    % Carrega todas as Informações Acadêmicas
    findall( Academica, privado_CurriculoAcademicas(Nome, Academica), 
                                                                Academicas),

    % Carrega todas as Informações Profissionais
    findall( Profissional, privado_CurriculoProfissionais(Nome, Profissional), 
                                                                Profissionais),

    Curriculo = [Pessoais, Academicas, Profissionais],
    !.

    /* Carrega informações academicas. 
     * */  
    privado_CurriculoAcademicas(Nome, Academica) :-
	    informacoesAcademicas(Academicas), 
	    dadoNaPosicao(NomeDaPessoa, Academicas, 0),
	    NomeDaPessoa == Nome,
	    copy_term(Academicas, Academica).

    /* Carrega informações profissionais. 
     * */
    privado_CurriculoProfissionais(Nome, Profissional) :-
        informacoesProfissionais(Profissionais), 
        dadoNaPosicao(NomeDaPessoa, Profissionais, 0),
        NomeDaPessoa == Nome,
        copy_term(Profissionais, Profissional).


/* Retorna uma lista com os Currículo de todas as Pessoas para uma dada Cidade. 
 *   Cada currículo é uma lista é composta pelas Informações Pessoais, uma  
 *   Lista de Informações Academicas e uma lista de Informações Profissionais. 
 *   Onde cada uma das Informações é também outra lista contendo:
 *
 *   Organização das Informações Pessoais. ###################################
 *   Nome, data de nascimento, cidade, telefone  
 *   
 *   Organização das Informações Profissionais. ##############################
 *   Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 *   Nome Completo de Colegas como Referências...
 *   
 *   Organização das Informações Acadêmicas. #################################
 *   Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 *   Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
curriculosPelaCidade(Cidade, Curriculos) :-
    % Cria uma lista contendo o nome de todas as pessoas de uma dada cidade.
    findall( Nome, privado_CurriculosPelaCidade(Cidade, Nome), Nomes), 

    % Para cada um dos Nomes da lista, cria uma lista contendo os currículos.
    privado_CurriculosPelaCidadeRecursivo(Nomes, Curriculos),
    !.
    
    /* Para uma dada Cidade, retorna o nome das pessoas que moram nela.
     * */
    privado_CurriculosPelaCidade(Cidade, Nome) :-
        informacoesPessoais(Pessoais), 
	    dadoNaPosicao(NomeDaCidade, Pessoais, 2),
	    NomeDaCidade == Cidade,
	    dadoNaPosicao(Nome, Pessoais, 0).

    /* Cria uma lista contendo todos os Currículos para cada um dos Nomes.
     * */
    privado_CurriculosPelaCidadeRecursivo(Nomes, Curriculos) :-
	    privado_CurriculosPelaCidadeRecursivoInterno(Nomes, [], Curriculos).

	    /* Funcionamento interno de privado_CurriculosPelaCidadeRecursivo. 
	     * */
	    privado_CurriculosPelaCidadeRecursivoInterno([], Curriculos, 
	                                                     CurriculosRetorno) :-
	        copy_term(Curriculos, CurriculosRetorno).
	    
	    /* Funcionamento interno de privado_CurriculosPelaCidadeRecursivo. 
	     * */
	    privado_CurriculosPelaCidadeRecursivoInterno([Nome|Nomes], Curriculos, 
	                                                  CurriculosRetorno ) :-
	        curriculoPeloNome(Nome, Curriculo), 
	        inseridoNoFinal(Curriculo, Curriculos, Curriculos_Saida), 
	        privado_CurriculosPelaCidadeRecursivoInterno(Nomes, 
	                                   Curriculos_Saida, CurriculosRetorno ).


/* Retorna uma lista com os Currículo de todas as Pessoas para uma dada  
 *   Instituição. Cada currículo é uma lista é composta pelas Informações  
 *   Pessoais, uma Lista de Informações Academicas e uma lista de 
 *   Informações Profissionais. Onde cada uma das Informações é também 
 *   outra lista contendo:
 *
 *   Organização das Informações Pessoais. ###################################
 *   Nome, data de nascimento, cidade, telefone  
 *   
 *   Organização das Informações Profissionais. ##############################
 *   Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 *   Nome Completo de Colegas como Referências...
 *   
 *   Organização das Informações Acadêmicas. #################################
 *   Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 *   Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
curriculosPelaInstituicao(Instituicao, Curriculos) :-
    % Cria uma lista contendo o nome de todas as pessoas de uma dada cidade.
    findall( Nome, privado_CurriculosPelaInstituicao(Instituicao, Nome), Nomes), 

    % Para cada um dos Nomes da lista, cria uma lista contendo os currículos.
    privado_CurriculosPelaInstituicaoRecursivo(Nomes, Curriculos),
    !.
    
    /* Para uma dada Instituicao, retorna o nome das pessoas que moram nela.
     * */
    privado_CurriculosPelaInstituicao(Instituicao, Nome) :-
        informacoesAcademicas(Academicas), 
        dadoNaPosicao(NomeDaInstituicao, Academicas, 2),
        NomeDaInstituicao == Instituicao,
        dadoNaPosicao(Nome, Academicas, 0).

    /* Cria uma lista contendo todos os Currículos para cada um dos Nomes.
     * */
    privado_CurriculosPelaInstituicaoRecursivo(Nomes, Curriculos) :-
        privado_CurriculosPelaInstituicaoRecursivoInterno(Nomes, [], 
                                                                   Curriculos).

        /* Funcionamento interno de privado_CurriculosPelaInstituicaoRecursivo. 
         * */
        privado_CurriculosPelaInstituicaoRecursivoInterno([], Curriculos, 
                                                         CurriculosRetorno) :-
            copy_term(Curriculos, CurriculosRetorno).
        
        /* Funcionamento interno de privado_CurriculosPelaInstituicaoRecursivo. 
         * */
        privado_CurriculosPelaInstituicaoRecursivoInterno([Nome|Nomes], 
                                            Curriculos, CurriculosRetorno ) :-
            curriculoPeloNome(Nome, Curriculo), 
            inseridoNoFinal(Curriculo, Curriculos, Curriculos_Saida), 
            privado_CurriculosPelaInstituicaoRecursivoInterno(Nomes, 
                                       Curriculos_Saida, CurriculosRetorno ).


/* Retorna uma lista com os Currículo de todas as Pessoas para uma dada  
 *   Empresa. Cada currículo é uma lista é composta pelas Informações  
 *   Pessoais, uma Lista de Informações Academicas e uma lista de 
 *   Informações Profissionais. Onde cada uma das Informações é também 
 *   outra lista contendo:
 *
 *   Organização das Informações Pessoais. ###################################
 *   Nome, data de nascimento, cidade, telefone  
 *   
 *   Organização das Informações Profissionais. ##############################
 *   Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 *   Nome Completo de Colegas como Referências...
 *   
 *   Organização das Informações Acadêmicas. #################################
 *   Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 *   Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
curriculosPelaEmpresa(Empresa, Curriculos) :-
    % Cria uma lista contendo o nome de todas as pessoas de uma dada cidade.
    findall( Nome, privado_CurriculosPelaEmpresa(Empresa, Nome), Nomes), 

    % Para cada um dos Nomes da lista, cria uma lista contendo os currículos.
    privado_CurriculosPelaEmpresaRecursivo(Nomes, Curriculos),
    !.
    
    /* Para uma dada Empresa, retorna o nome das pessoas que moram nela.
     * */
    privado_CurriculosPelaEmpresa(Empresa, Nome) :-
        informacoesProfissionais(Profissionais), 
        dadoNaPosicao(NomeDaEmpresa, Profissionais, 1),
        NomeDaEmpresa == Empresa,
        dadoNaPosicao(Nome, Profissionais, 0).

    /* Cria uma lista contendo todos os Currículos para cada um dos Nomes.
     * */
    privado_CurriculosPelaEmpresaRecursivo(Nomes, Curriculos) :-
        privado_CurriculosPelaEmpresaRecursivoInterno(Nomes, [], 
                                                                   Curriculos).

        /* Funcionamento interno de privado_CurriculosPelaEmpresaRecursivo. 
         * */
        privado_CurriculosPelaEmpresaRecursivoInterno([], Curriculos, 
                                                         CurriculosRetorno) :-
            copy_term(Curriculos, CurriculosRetorno).
        
        /* Funcionamento interno de privado_CurriculosPelaEmpresaRecursivo. 
         * */
        privado_CurriculosPelaEmpresaRecursivoInterno([Nome|Nomes], 
                                            Curriculos, CurriculosRetorno ) :-
            curriculoPeloNome(Nome, Curriculo), 
            inseridoNoFinal(Curriculo, Curriculos, Curriculos_Saida), 
            privado_CurriculosPelaEmpresaRecursivoInterno(Nomes, 
                                       Curriculos_Saida, CurriculosRetorno ).


/* Retorna uma lista com os Currículo de todas as Pessoas para uma dada Cidade e
 *   uma Idade mínima. Cada currículo é uma lista é composta pelas Informações  
 *   Pessoais, uma Lista de Informações Academicas e uma lista de 
 *   Informações Profissionais. Onde cada uma das Informações é também 
 *   outra lista contendo:
 *
 *   Organização das Informações Pessoais. ###################################
 *   Nome, data de nascimento, cidade, telefone  
 *   
 *   Organização das Informações Profissionais. ##############################
 *   Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 *   Nome Completo de Colegas como Referências...
 *   
 *   Organização das Informações Acadêmicas. #################################
 *   Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 *   Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */
curriculosPelaIdadeCidade(Idade, Cidade, Curriculos) :-
    % Cria uma lista contendo o nome de todas as pessoas de uma dada cidade.
    findall( Nome, privado_CurriculosPelaIdadeCidade(Idade, Cidade, Nome), 
                                                                        Nomes), 

    % Para cada um dos Nomes da lista, cria uma lista contendo os currículos.
    privado_CurriculosPelaIdadeCidadeRecursivo(Nomes, Curriculos),
    !.
    
    /* Para uma dada IdadeCidade, retorna o nome das pessoas que moram nela.
     * */
    privado_CurriculosPelaIdadeCidade(Idade, Cidade, Nome) :-
        informacoesPessoais(Pessoais), 
        dadoNaPosicao(NomeDaCidade, Pessoais, 2),
        dadoNaPosicao(NomeDaPessoa, Pessoais, 0),
        qualIdadeDe(NomeDaPessoa, IdadeDaPessoa),
        NomeDaCidade == Cidade, 
        IdadeDaPessoa > Idade, 
        copy_term(NomeDaPessoa, Nome).

    /* Cria uma lista contendo todos os Currículos para cada um dos Nomes.
     * */
    privado_CurriculosPelaIdadeCidadeRecursivo(Nomes, Curriculos) :-
        privado_CurriculosPelaIdadeCidadeRecursivoInterno(Nomes, [], 
                                                                   Curriculos).

        /* Funcionamento interno de privado_CurriculosPelaIdadeCidadeRecursivo. 
         * */
        privado_CurriculosPelaIdadeCidadeRecursivoInterno([], Curriculos, 
                                                         CurriculosRetorno) :-
            copy_term(Curriculos, CurriculosRetorno).
        
        /* Funcionamento interno de privado_CurriculosPelaIdadeCidadeRecursivo. 
         * */
        privado_CurriculosPelaIdadeCidadeRecursivoInterno([Nome|Nomes], 
                                            Curriculos, CurriculosRetorno ) :-
            curriculoPeloNome(Nome, Curriculo), 
            inseridoNoFinal(Curriculo, Curriculos, Curriculos_Saida), 
            privado_CurriculosPelaIdadeCidadeRecursivoInterno(Nomes, 
                                       Curriculos_Saida, CurriculosRetorno ).







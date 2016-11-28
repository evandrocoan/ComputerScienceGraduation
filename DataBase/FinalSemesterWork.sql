
/*

Curso( CodCurso, NomeCurso )
Pessoa( NumeroCartao, NomePess, Sexo, EMailPref, CodCurso )
    ( CodCurso ) referencia Curso( CodCurso )
OutroEMail( NumeroCartao, EMail )
    ( NumeroCartao ) referencia Pessoa( NumeroCartao )

Projeto( CodProj, NomeProj, AnoInicio, AnoFim, CodProjAnte )
    ( CodProjAnte ) referencia Projeto( CodProj )
ProjetoPessoa( CodProj, NumeroCartao, PapelPessProj )
    ( CodProj )      referencia Projeto( CodProj )
    ( NumeroCartao ) referencia Pessoa ( NumeroCartao )

*/

-- below code assumes that the name of your schema is public
DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

-- For PostgreSQL 9.3 or greater, you may also need to restore the default grants.
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Curso( CodCurso, NomeCurso )'
\echo 'INFO: Pessoa( NumeroCartao, NomePess, Sexo, EMailPref, CodCurso )'
\echo 'INFO:     ( CodCurso ) referencia Curso( CodCurso )'
\echo 'INFO: OutroEMail( NumeroCartao, EMail )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa( NumeroCartao )'
\echo 'INFO: '
\echo 'INFO: Projeto( CodProj, NomeProj, AnoInicio, AnoFim, CodProjAnte )'
\echo 'INFO:     ( CodProjAnte ) referencia Projeto( CodProj )'
\echo 'INFO: ProjetoPessoa( CodProj, NumeroCartao, PapelPessProj )'
\echo 'INFO:     ( CodProj )      referencia Projeto( CodProj )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa ( NumeroCartao )'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Crie as tabelas apresentadas a seguir em um banco de dados, e insira dados de tal'
\echo 'INFO: forma que todas as consultas abaixo tenham retorno de alguma tupla. Formato de'
\echo 'INFO: entrega: arquivos scripts SQL, padrão texto (extensão SQL ou txt).'
\echo 'INFO:'

\echo 'INFO: Tabela com os cursos da Universidade:'
\echo 'INFO: Curso( CodCurso, NomeCurso )'

CREATE TABLE Curso
(
    CodCurso  integer, -- PRIMARY KEY, -- Second method of creation
    NomeCurso varchar,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_curso PRIMARY KEY( CodCurso )
);

\echo 'INFO:'
\echo 'INFO: Tabela de pessoas (alunos e servidores). com número de cartão, nome'
\echo 'INFO: da pessoa, sexo e e-mail preferencial. Para alunos o código do curso'
\echo 'INFO: identifica o curso a que eles estão vinculados:'
\echo 'INFO: Pessoa( NumeroCartao, NomePess, Sexo, EMailPref, CodCurso )'
\echo 'INFO:     ( CodCurso ) referencia Curso( CodCurso )'

CREATE TABLE Pessoa
(
    NumeroCartao integer, -- PRIMARY KEY, -- Second method of creation
    NomePess     varchar,
    Sexo         varchar(1),
    EMailPref    varchar,
    CodCurso     integer,

    CHECK( Sexo = 'M' OR Sexo = 'F' ),

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_pessoa PRIMARY KEY( NumeroCartao ),

    CONSTRAINT fk_empregado FOREIGN KEY( CodCurso )
            REFERENCES Curso( CodCurso )
);

\echo 'INFO:'
\echo 'INFO: Tabela com e-mails alternativos das pessoas:'
\echo 'INFO: OutroEMail( NumeroCartao, EMail )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa( NumeroCartao )'

CREATE TABLE OutroEMail
(
    NumeroCartao integer, -- PRIMARY KEY, -- Second method of creation
    EMail        varchar,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_outro_email PRIMARY KEY( NumeroCartao, EMail )
);

\echo 'INFO:'
\echo 'INFO: Tabela com os projetos em andamento e concluídos. Quando um'
\echo 'INFO: projeto é continuação de outro, a coluna CodProjAnte informa o projeto'
\echo 'INFO: anterior ao projeto em questão. Quando o projeto não é continuação, a'
\echo 'INFO: coluna fica vazia (NULL):'
\echo 'INFO: Projeto( CodProj, NomeProj, AnoInicio, AnoFim, CodProjAnte )'
\echo 'INFO:     ( CodProjAnte ) referencia Projeto( CodProj )'

CREATE TABLE Projeto
(
    CodProj     integer, -- PRIMARY KEY, -- Second method of creation
    NomeProj    varchar,
    AnoInicio   integer,
    AnoFim      integer,
    CodProjAnte integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_projeto PRIMARY KEY( CodProj ),

    CONSTRAINT fk_projeto FOREIGN KEY( CodProjAnte )
            REFERENCES Projeto( CodProj )
);

\echo 'INFO:'
\echo 'INFO: Tabela que vincula projetos a pessoas. O papel da pessoa pode ser:'
\echo 'INFO: ’Líder’, Membro’ e ’Bolsista’:'
\echo 'INFO: ProjetoPessoa( CodProj, NumeroCartao, PapelPessProj )'
\echo 'INFO:     ( CodProj )      referencia Projeto( CodProj )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa ( NumeroCartao )'

CREATE TABLE ProjetoPessoa
(
    CodProj       integer, -- PRIMARY KEY, -- Second method of creation
    NumeroCartao  integer,
    PapelPessProj varchar,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_projeto_pessoa PRIMARY KEY( CodProj, NumeroCartao ),

    CONSTRAINT fk_projeto FOREIGN KEY( CodProj )
            REFERENCES Projeto( CodProj ),

    CONSTRAINT fk_pessoa FOREIGN KEY( NumeroCartao )
            REFERENCES Pessoa( NumeroCartao )
);

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Inserir dados com INSERT (deve haver dados para ter resultados para todas as consultas):'
\echo 'INFO:'

\echo 'INFO: Curso( CodCurso, NomeCurso )'

INSERT INTO Curso VALUES(  1, 'Curso 1'  );
INSERT INTO Curso VALUES(  2, 'Curso 2'  );
INSERT INTO Curso VALUES(  3, 'Curso 3'  );
INSERT INTO Curso VALUES(  4, 'Curso 4'  );

\echo 'INFO: Pessoa( NumeroCartao, NomePess, Sexo, EMailPref, CodCurso )'
\echo 'INFO:     ( CodCurso ) referencia Curso( CodCurso )'

INSERT INTO Pessoa VALUES( 1,  'NomePessoa 1' , 'F', 'email_1@email.com' , 1 );
INSERT INTO Pessoa VALUES( 2,  'NomePessoa 2' , 'F', 'email_2@email.com' , 2 );
INSERT INTO Pessoa VALUES( 3,  'NomePessoa 3' , 'F', 'email_3@email.com' , 3 );
INSERT INTO Pessoa VALUES( 4,  'NomePessoa 4' , 'F', 'email_4@email.com' , 4 );
INSERT INTO Pessoa VALUES( 5,  'NomePessoa 5' , 'M', 'email_5@email.com' , 1 );
INSERT INTO Pessoa VALUES( 6,  'NomePessoa 6' , 'M', 'email_6@email.com' , 2 );
INSERT INTO Pessoa VALUES( 7,  'NomePessoa 7' , 'M', 'email_7@email.com' , 3 );
INSERT INTO Pessoa VALUES( 8,  'NomePessoa 8' , 'M', 'email_8@email.com' , 4 );
INSERT INTO Pessoa VALUES( 9,  'NomePessoa 9' , 'F', 'email_9@email.com' , 1 );
INSERT INTO Pessoa VALUES( 10, 'NomePessoa 10', 'F', 'email_10@email.com', 2 );

\echo 'INFO: OutroEMail( NumeroCartao, EMail )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa( NumeroCartao )'

INSERT INTO OutroEMail VALUES( 1, 'email2_1@email.com' );
INSERT INTO OutroEMail VALUES( 4, 'email2_2@email.com' );
INSERT INTO OutroEMail VALUES( 5, 'email2_3@email.com' );
INSERT INTO OutroEMail VALUES( 7, 'email2_4@email.com' );
INSERT INTO OutroEMail VALUES( 1, 'email2_5@email.com' );
INSERT INTO OutroEMail VALUES( 4, 'email2_6@email.com' );
INSERT INTO OutroEMail VALUES( 5, 'email2_7@email.com' );
INSERT INTO OutroEMail VALUES( 7, 'email2_8@email.com' );
INSERT INTO OutroEMail VALUES( 1, 'email2_9@email.com' );

\echo 'INFO: Projeto( CodProj, NomeProj, AnoInicio, AnoFim, CodProjAnte )'
\echo 'INFO:     ( CodProjAnte ) referencia Projeto( CodProj )'

INSERT INTO Projeto VALUES( 1 , 'NomeProjeto 1'  , 1966, 2016     );
INSERT INTO Projeto VALUES( 2 , 'NomeProjeto 2'  , 1992, 2010     );
INSERT INTO Projeto VALUES( 3 , 'NomeProjeto 3'  , 1963, 1999     );
INSERT INTO Projeto VALUES( 4 , 'NomeProjeto 4'  , 1977, 1998     );
INSERT INTO Projeto VALUES( 5 , 'NomeProjeto 5'  , 1994, 2016     );
INSERT INTO Projeto VALUES( 6 , 'NomeProjeto 6'  , 1955, 2008     );
INSERT INTO Projeto VALUES( 7 , 'NomeProjeto 7'  , 1955, 2010, 1 );
INSERT INTO Projeto VALUES( 8 , 'NomeProjeto 8'  , 1984, 2010, 7  );
INSERT INTO Projeto VALUES( 9 , 'NomeProjeto 9'  , 1954, 2013, 6  );
INSERT INTO Projeto VALUES( 10, 'NomeProjeto 10' , 1988, 2014, 10 );
INSERT INTO Projeto VALUES( 11, 'NomeProjeto 11' , 1986, 2011, 8  );
INSERT INTO Projeto VALUES( 12, 'NomeProjeto 12' , 1980, 2007, 4  );
INSERT INTO Projeto VALUES( 13, 'NomeProjeto 13' , 1976, 2013, 6  );
INSERT INTO Projeto VALUES( 14, 'NomeProjeto 14' , 1967, 2005, 1  );
INSERT INTO Projeto VALUES( 15, 'NomeProjeto 15' , 1970, 2001, 6  );
INSERT INTO Projeto VALUES( 16, 'NomeProjeto 16' , 1977, 2013, 6  );

\echo 'INFO: ProjetoPessoa( CodProj, NumeroCartao, PapelPessProj )'
\echo 'INFO:     ( CodProj )      referencia Projeto( CodProj )'
\echo 'INFO:     ( NumeroCartao ) referencia Pessoa ( NumeroCartao )'

INSERT INTO ProjetoPessoa VALUES( 6 , 8 , 'Papel 1' );
INSERT INTO ProjetoPessoa VALUES( 11, 4 , 'Papel 2' );
INSERT INTO ProjetoPessoa VALUES( 12, 8 , 'Papel 3' );
INSERT INTO ProjetoPessoa VALUES( 16, 10, 'Papel 4' );
INSERT INTO ProjetoPessoa VALUES( 6 , 4 , 'Papel 5' );
INSERT INTO ProjetoPessoa VALUES( 9 , 8 , 'Papel 6' );
INSERT INTO ProjetoPessoa VALUES( 2 , 6 , 'Papel 1' );
INSERT INTO ProjetoPessoa VALUES( 3 , 10, 'Papel 2' );
INSERT INTO ProjetoPessoa VALUES( 10, 9 , 'Papel 3' );
INSERT INTO ProjetoPessoa VALUES( 7 , 1 , 'Papel 4' );
INSERT INTO ProjetoPessoa VALUES( 2 , 8 , 'Papel 5' );
INSERT INTO ProjetoPessoa VALUES( 5 , 9 , 'Papel 6' );
INSERT INTO ProjetoPessoa VALUES( 16, 5 , 'Papel 1' );
INSERT INTO ProjetoPessoa VALUES( 16, 3 , 'Papel 2' );
INSERT INTO ProjetoPessoa VALUES( 10, 10, 'Papel 3' );
INSERT INTO ProjetoPessoa VALUES( 15, 5 , 'Papel 4' );
INSERT INTO ProjetoPessoa VALUES( 13, 2 , 'Papel 5' );
INSERT INTO ProjetoPessoa VALUES( 5 , 4 , 'Papel 6' );

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Crie consultas em SQL, seguindo as seguintes instruções (não esqueça de fornecer o enunciado de sua consulta):'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: I. Crie uma consulta que teste duas colunas no WHERE, mas que selecione outras duas.'
\echo 'INFO: A cláusula WHERE deve possuir o operador BETWEEN.'
\echo 'INFO:'

\echo 'INFO: Selecione os projetos the iniciaram entre 1990 e 2000, mas que terminaram depois de'
\echo 'INFO: 2010. A cláusula WHERE deve possuir o operador BETWEEN.'
\echo 'INFO:'

SELECT Projeto.CodProj, Projeto.NomeProj
FROM Projeto
WHERE Projeto.AnoInicio BETWEEN 1990 AND 2000 AND Projeto.AnoFim > 2010;

--
-- linhas  <- sig Projeto.AnoInicio > 1990 AND Projeto.AnoInicio < 2000 AND Projeto.AnoFim < 2010 (Projeto)
-- colunas <- pi Projeto.CodProj, Projeto.NomeProj (linhas)
--
--  codproj |   nomeproj
-- ---------+---------------
--        5 | NomeProjeto 5

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: II. Crie uma consulta que acesse duas tabelas. A cláusula SELECT deve possui mais'
\echo 'INFO: de dois atributos. A cláusula WHERE deve possuir AND e OR. A consulta deve possuir as seguintes'
\echo 'INFO: duas variantes:'
\echo 'INFO: (a) com operador de JOIN'
\echo 'INFO:'

\echo 'INFO: Selecione o codigo do projeto, seu nome e o papel das pessoas envolvidas nos projetos'
\echo 'INFO: que começaram depois de 2000 e terminaram antes de 2016 ou que iniciaram antes de 1960.'
\echo 'INFO:'

SELECT Projeto.CodProj, Projeto.NomeProj, ProjetoPessoa.PapelPessProj
FROM Projeto JOIN ProjetoPessoa ON Projeto.CodProj = ProjetoPessoa.CodProj
WHERE Projeto.AnoInicio > 2000 AND Projeto.AnoFim < 2016 OR Projeto.AnoInicio < 1960;

--
-- linhas  <- Projeto        JOIN Projeto.CodProj = ProjetoPessoa.CodProj       ProjetoPessoa
-- linhas2 <- sig Projeto.AnoInicio > 2000 AND Projeto.AnoFim < 2016 OR Projeto.AnoInicio < 1960 (linhas)
-- pi Projeto.CodProj, Projeto.NomeProj, ProjetoPessoa.PapelPessProj (linhas2)
--

\echo 'INFO: (b) com operador de produto cartesiano'
\echo 'INFO:'

SELECT Projeto.CodProj, Projeto.NomeProj, ProjetoPessoa.PapelPessProj
FROM Projeto, ProjetoPessoa
WHERE Projeto.CodProj = ProjetoPessoa.CodProj AND
(
    Projeto.AnoInicio > 2000 AND Projeto.AnoFim < 2016 OR Projeto.AnoInicio < 1960
);

--
-- linhas  <- sig Projeto.CodProj   =     ProjetoPessoa.CodProj                 (Projeto X ProjetoPessoa)
-- linhas2 <- sig Projeto.AnoInicio > 2000 AND Projeto.AnoFim < 2016 OR Projeto.AnoInicio < 1960 (linhas)
-- pi Projeto.CodProj, Projeto.NomeProj, ProjetoPessoa.PapelPessProj (linhas2)
--
--  codproj |   nomeproj    | papelpessproj
-- ---------+---------------+---------------
--        6 | NomeProjeto 6 | Papel 1
--        6 | NomeProjeto 6 | Papel 5
--        9 | NomeProjeto 9 | Papel 6
--        7 | NomeProjeto 7 | Papel 4
-- (4 rows)

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: III. Crie uma consulta que acesse três tabelas sendo que uma delas deve ter sido'
\echo 'INFO: criada por causa de um relacionamento n_n do modelo conceitual. A cláusula SELECT deve possuir'
\echo 'INFO: pelo menos um atributo de cada uma das tabelas relacionadas. A cláusula WHERE deve possuir'
\echo 'INFO: pelo menos três filtros. A consulta deve ser feita com operador de JOIN e possuir alguma'
\echo 'INFO: função de agregação.'
\echo 'INFO:'

\echo 'INFO: Selecione o nome do projeto, o papel e o nome das pessoas envolvidas nos projetos'
\echo 'INFO: que começaram depois de 2000 e terminaram antes de 2016 ou que iniciaram antes de 1960.'
\echo 'INFO: Também conte em quantos dos projetos existentes cada uma das pessoas está/esteve'
\echo 'INFO: envolvida, como uma nova coluna chamada `contagem_de_projetos`'
\echo 'INFO:'

SELECT Projeto.NomeProj, ProjetoPessoa.PapelPessProj, Pessoa.NomePess,
       Contagem.count AS contagem_de_projetos
FROM Projeto JOIN ProjetoPessoa ON Projeto.CodProj            = ProjetoPessoa.CodProj
             JOIN Pessoa        ON ProjetoPessoa.NumeroCartao = Pessoa.NumeroCartao
             JOIN
(
    SELECT ProjetoPessoa.NumeroCartao, COUNT( * )
    FROM ProjetoPessoa
    GROUP BY 1
) Contagem ON Contagem.NumeroCartao = ProjetoPessoa.NumeroCartao
WHERE Projeto.AnoInicio > 2000 AND Projeto.AnoFim < 2016 OR Projeto.AnoInicio < 1960;

--
-- linhas  <- Projeto  JOIN Projeto.CodProj            = ProjetoPessoa.CodProj         ProjetoPessoa
-- linhas2 <- Pessoa   JOIN ProjetoPessoa.NumeroCartao = Pessoa.NumeroCartao           linhas
-- linhas3 <- Contagem JOIN Contagem.NumeroCartao      = ProjetoPessoa.NumeroCartao    linhas2
-- linhas4 <- sig Projeto.AnoInicio > 2000     AND      Projeto.AnoFim < 2016          (linhas3)
-- pi Projeto.NomeProj, ProjetoPessoa.PapelPessProj, Pessoa.NomePessoa, Contagem.count (linhas4)
--
--    nomeproj    | papelpessproj |   nomepess   | contagem_de_projetos
-- ---------------+---------------+--------------+----------------------
--  NomeProjeto 6 | Papel 1       | NomePessoa 8 |                    4
--  NomeProjeto 6 | Papel 5       | NomePessoa 4 |                    3
--  NomeProjeto 9 | Papel 6       | NomePessoa 8 |                    4
--  NomeProjeto 7 | Papel 4       | NomePessoa 1 |                    1

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: IV. Crie uma consulta, que acesse três tabelas, selecionando dois atributos'
\echo 'INFO: quaisquer e usando a função COUNT(). Utilize o ORDER BY e GROUP BY.'
\echo 'INFO: A cláusula WHERE deve possuir pelo menos dois filtros.'
\echo 'INFO:'

\echo 'INFO: Selecione o codigo e nome dos cursos. Também conte quantos emails alternativos'
\echo 'INFO: existem para todas as pessoas que fazem os cursos, i.e., o total de emails'
\echo 'INFO: alternativos por cada curso.'
\echo 'INFO:'

SELECT Curso.CodCurso, Curso.NomeCurso, OutraTabela.count
FROM Curso JOIN
(
    SELECT Curso.CodCurso, COUNT( OutroEMail.NumeroCartao )
    FROM Pessoa JOIN OutroEMail ON Pessoa.NumeroCartao = OutroEMail.NumeroCartao
                JOIN Curso      ON Pessoa.CodCurso     = Curso.CodCurso
    GROUP BY 1
) OutraTabela ON Curso.CodCurso = OutraTabela.CodCurso
WHERE Curso.NomeCurso LIKE '%Curso%' AND Curso.CodCurso < 4
ORDER BY 2;

--
-- linhas      <- Pessoa JOIN Pessoa.NumeroCartao = OutroEMail.NumeroCartao          OutroEMail
-- linhas2     <- Curso  JOIN Pessoa.CodCurso     = Curso.CodCurso                   linhas
-- OutraTabela <- pi          Curso.CodCurso, count                                  (linhas2)
-- linhas4     <- Curso  JOIN Curso.CodCurso      = OutraTabela.CodCurso             OutraTabela
-- linhas5     <- sig Curso.NomeCurso             = '%Curso%' AND Curso.CodCurso < 4 linhas4
-- pi       Curso.CodCurso, Curso.NomeCurso, OutraTabela.count      (linhas5)
--
--  codcurso | nomecurso | count
-- ----------+-----------+-------
--         1 | Curso 1   |     5
--         3 | Curso 3   |     2
-- (2 rows)

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: V. Crie uma consulta, que acesse três tabelas, que utilize o HAVING . A cláusula WHERE'
\echo 'INFO: deve possuir pelo menos dois filtros. O HAVING deve testar o valor de retorno de'
\echo 'INFO: uma função de agregação escrita em uma cláusula SELECT.'
\echo 'INFO:'

\echo 'INFO: Selecione o nome do projeto, o papel e o nome das pessoas envolvidas nos projetos'
\echo 'INFO: que começaram depois de 1970 e terminaram antes de 2016 ou que iniciaram antes de 1960.'
\echo 'INFO: Também conte em quantos dos projetos existentes cada uma das pessoas está/esteve'
\echo 'INFO: envolvida, como uma nova coluna chamada `contagem_de_projetos`, que somente deve'
\echo 'INFO: incluir as `contagem_de_projetos` maiores do que 3.'
\echo 'INFO:'

SELECT Projeto.NomeProj, ProjetoPessoa.PapelPessProj, Pessoa.NomePess,
       Contagem.count AS contagem_de_projetos
FROM Projeto JOIN ProjetoPessoa ON Projeto.CodProj            = ProjetoPessoa.CodProj
             JOIN Pessoa        ON ProjetoPessoa.NumeroCartao = Pessoa.NumeroCartao
             JOIN
(
    SELECT ProjetoPessoa.NumeroCartao, COUNT( * )
    FROM ProjetoPessoa
    GROUP BY 1
    HAVING COUNT( * ) > 3
) Contagem ON Contagem.NumeroCartao = ProjetoPessoa.NumeroCartao
WHERE Projeto.AnoInicio > 1970 AND Projeto.AnoFim < 2016;

--
-- Contagem <- pi      ProjetoPessoa.NumeroCartao, count         ProjetoPessoa
-- linhas   <- Projeto  JOIN Projeto.CodProj            = ProjetoPessoa.CodProj      ProjetoPessoa
-- linhas2  <- Pessoa   JOIN ProjetoPessoa.NumeroCartao = Pessoa.NumeroCartao        linhas
-- linhas3  <- Contagem JOIN Contagem.NumeroCartao      = ProjetoPessoa.NumeroCartao linhas2
-- linhas4  <- sig    Projeto.AnoInicio > 1970 AND Projeto.AnoFim < 2016     (linhas3)
-- pi    Projeto.NomeProj, ProjetoPessoa.PapelPessProj, Pessoa.NomePessoa, Contagem.count    (linhas4)
--
--     nomeproj    | papelpessproj |   nomepess   | contagem_de_projetos
-- ----------------+---------------+--------------+----------------------
--  NomeProjeto 12 | Papel 3       | NomePessoa 8 |                    4
--  NomeProjeto 2  | Papel 5       | NomePessoa 8 |                    4
-- (2 rows)





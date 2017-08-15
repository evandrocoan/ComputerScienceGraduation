
/*

Departamento (codDepto, nome)
Empregado (codemp, nome, endereco, dataNasc, salario, codDepto)
    codDepto referencia departamento (codDepto)

Projeto (codproj, titulo)
Trabalhaem(codEmp, codProj, horasTrab)
    codEmp referencia Empregado (codEmp)
    codProj referencia Projeto (codProj)

*/

-- below code assumes that the name of your schema is public
DROP SCHEMA PUBLIC CASCADE;
CREATE SCHEMA PUBLIC;

-- For PostgreSQL 9.3 or greater, you may also need to restore the default grants.
GRANT ALL ON SCHEMA PUBLIC TO postgres;
GRANT ALL ON SCHEMA PUBLIC TO PUBLIC;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Departamento (codDepto, nome)'
\echo 'INFO: Empregado (codemp, nome, endereco, dataNasc, salario, codDepto)'
\echo 'INFO:     codDepto referencia departamento (codDepto)'
\echo 'INFO:'
\echo 'INFO: Projeto (codproj, titulo)'
\echo 'INFO: Trabalhaem(codEmp, codProj, horasTrab)'
\echo 'INFO:     codEmp referencia Empregado (codEmp)'
\echo 'INFO:     codProj referencia Projeto (codProj)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: I. Criar as tabelas com CREATE TABLE.'
\echo 'INFO:'

\echo 'INFO: Departamento (codDepto, nome)'

CREATE TABLE Departamento
(
    codDepto integer, -- PRIMARY KEY, -- Second method of creation
    nome     varchar,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_departamento PRIMARY KEY(codDepto)
);

\echo 'INFO: Empregado (codemp, nome, endereco, dataNasc, salario, codDepto)'
\echo 'INFO:     codDepto referencia departamento (codDepto)'

CREATE TABLE Empregado
(
    codemp   integer, -- PRIMARY KEY, -- Second method of creation
    nome     varchar,
    endereco varchar,
    fone     integer,
    dataNasc date,
    salario  integer,
    codDepto integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_empregado PRIMARY KEY(codemp),

    CONSTRAINT fk_empregado FOREIGN KEY(codDepto)
            REFERENCES Departamento(codDepto) ON DELETE CASCADE
);

\echo 'INFO: Projeto (codproj, titulo)'

CREATE TABLE Projeto
(
    codproj integer, -- PRIMARY KEY, -- Second method of creation
    titulo  varchar,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_projeto PRIMARY KEY(codproj)
);

\echo 'INFO: Trabalhaem(codEmp, codProj, horasTrab)'
\echo 'INFO:     codEmp referencia Empregado (codEmp)'
\echo 'INFO:     codProj referencia Projeto (codProj)'

CREATE TABLE Trabalhaem
(
    codEmp    integer, -- PRIMARY KEY, -- Second method of creation
    codProj   integer,
    horasTrab integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_trabalhaem PRIMARY KEY(codEmp,codProj),

    CONSTRAINT fk_empregado FOREIGN KEY(codEmp)
            REFERENCES Empregado(codemp) ON DELETE CASCADE,

    CONSTRAINT fk_projeto FOREIGN KEY(codProj)
            REFERENCES Projeto(codproj) ON DELETE CASCADE
);

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: II. Inserir dados com INSERT (deve haver dados para ter resultados para todas as consultas)'
\echo 'INFO:'

\echo 'INFO: Departamento (codDepto, nome)'

INSERT INTO Departamento VALUES( 1, 'Dep: Ticular' );
INSERT INTO Departamento VALUES( 2, 'Dep: ParUni'  );
INSERT INTO Departamento VALUES( 3, 'Pesquisa'     );
INSERT INTO Departamento VALUES( 4, 'Dep: Tarar'   );

\echo 'INFO: Empregado (codemp, nome, endereco, dataNasc, salario, codDepto)'
\echo 'INFO:     codDepto referencia departamento (codDepto)'

INSERT INTO Empregado( codemp, nome, endereco, dataNasc, salario, codDepto ) VALUES( 1, 'Maria Aparecida' , 'Rua Ortências', '1970-01-04', 20000, 1 );
INSERT INTO Empregado( codemp, nome, endereco, dataNasc, salario, codDepto ) VALUES( 2, 'Carolina Pereira', 'Av. das Rosas', '1979-11-07', 10000, 2 );
INSERT INTO Empregado( codemp, nome, endereco, dataNasc, salario, codDepto ) VALUES( 3, 'Antonio Carlos'  , 'Rua Caligari' , '1980-11-04', 50000, 3 );
INSERT INTO Empregado( codemp, nome, endereco, dataNasc, salario, codDepto ) VALUES( 4, 'Mariana Faria'   , 'Rua Luiz XI'  , '1961-10-14', 60000, 4 );
INSERT INTO Empregado( codemp, nome, endereco, dataNasc, salario, codDepto ) VALUES( 5, 'Faria Antonio'   , 'Rua XI'       , '1991-10-14', 1000 , 4 );

\echo 'INFO: Projeto (codproj, titulo)'

INSERT INTO Projeto VALUES( 1, 'Proj: Particular' );
INSERT INTO Projeto VALUES( 2, 'Proj: Uni'        );
INSERT INTO Projeto VALUES( 3, 'Transmogrifador'  );
INSERT INTO Projeto VALUES( 4, 'Tarar'            );

\echo 'INFO: Trabalhaem(codEmp, codProj, horasTrab)'
\echo 'INFO:     codEmp referencia Empregado (codEmp)'
\echo 'INFO:     codProj referencia Projeto (codProj)'

INSERT INTO Trabalhaem VALUES( 1, 2, 5   );
INSERT INTO Trabalhaem VALUES( 2, 3, 101 );
INSERT INTO Trabalhaem VALUES( 3, 1, 110 );
INSERT INTO Trabalhaem VALUES( 4, 2, 12  );
INSERT INTO Trabalhaem VALUES( 1, 3, 120 );
INSERT INTO Trabalhaem VALUES( 2, 4, 120 );

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: III. Responder as questões abaixo com consultas SQL.'
\echo 'INFO:'

\echo 'INFO: 1. Alterar o salário do empregado de código 3 para 28000'
\echo 'INFO:'

UPDATE empregado
SET salario = 28000
WHERE empregado.codEmp = 3;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 2 - Obter nomes de empregados com salario > 30000.'
\echo 'INFO:'

SELECT empregado.nome, empregado.salario
FROM empregado
WHERE empregado.salario > 30000;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 3. Obter nomes de empregados que trabalham no projeto \'Transmogrifador\'. '
\echo 'INFO:'

SELECT empregado.nome, projeto.titulo
FROM empregado JOIN Trabalhaem ON empregado.codemp   = Trabalhaem.codEmp
               JOIN Projeto    ON Trabalhaem.codProj = Projeto.codproj
WHERE Projeto.titulo = 'Transmogrifador';

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 4. Obter nomes e endereços de todos os empregados que trabalham no departamento '
\echo 'INFO: de \'Pesquisa\'. '
\echo 'INFO:'

SELECT empregado.nome, empregado.endereco, Departamento.nome
FROM empregado JOIN Departamento ON empregado.codDepto = Departamento.codDepto
WHERE departamento.nome = 'Pesquisa';

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 5. Obter nomes de empregados que começam com a letra \'A\'.'
\echo 'INFO:'

SELECT empregado.nome
FROM empregado
WHERE empregado.nome LIKE 'A%';

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 6. Obter os nomes e datas de nascimento dos empregados que fazem aniversário no mês de'
\echo 'INFO: outubro. Considere a data como tipo char(10).'
\echo 'INFO:'

SELECT empregado.nome, empregado.dataNasc
FROM empregado
WHERE EXTRACT( MONTH FROM empregado.dataNasc ) = 10;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 7. Obter os nomes dos empregados nascidos entre as datas 1950-01-01 e 1970-01-01.'
\echo 'INFO:'

SELECT empregado.nome, empregado.dataNasc
FROM empregado
WHERE empregado.dataNasc > '1950-01-01'
  AND empregado.dataNasc < '1970-01-01';

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 8. Listar os títulos de projetos em ordem alfabética.'
\echo 'INFO:'

SELECT Projeto.titulo
FROM Projeto
ORDER BY Projeto.titulo ASC;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 9. Listar nomes e horas trabalhadas por empregados no projeto de código 3, em ordem'
\echo 'INFO: decrescente de horas trabalhadas.'
\echo 'INFO:'

SELECT empregado.nome, Trabalhaem.horasTrab
FROM empregado JOIN Trabalhaem ON empregado.codemp   = Trabalhaem.codEmp
               JOIN Projeto    ON Trabalhaem.codProj = Projeto.codproj
WHERE Projeto.codproj = 3
ORDER BY Trabalhaem.horasTrab DESC;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 10. Obter códigos de empregados que trabalham mais de 10 horas em algum projeto.'
\echo 'INFO: O resultado da consulta não deve ter repetições de códigos de empregados.'
\echo 'INFO:'

SELECT DISTINCT ON( empregado.codemp )
        empregado.codemp, Trabalhaem.horasTrab
FROM empregado JOIN Trabalhaem ON empregado.codemp = Trabalhaem.codEmp
WHERE Trabalhaem.horasTrab > 20;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 11. Obter a quantidade de empregados pertencentes ao departamento 4.'
\echo 'INFO:'

SELECT COUNT( empregado.codDepto )
FROM empregado
WHERE empregado.codDepto = 4;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 12. Obter, a partir da tabela trabalhaEm, os números mínimo, máximo e médio de'
\echo 'INFO: horas trabalhadas por empregados em cada projeto. O resultado deve possuir 4'
\echo 'INFO: colunas nomeadas: projeto, minimo, maximo e media.'
\echo 'INFO:'

SELECT Projeto.titulo AS projeto, MIN( Trabalhaem.horasTrab ) AS minimo, MAX( Trabalhaem.horasTrab ) AS maximo, AVG( Trabalhaem.horasTrab ) AS media
FROM Projeto JOIN Trabalhaem ON Projeto.codproj = Trabalhaem.codProj
GROUP BY 1;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 13. Obter os códigos de projetos cuja média de horas trabalhadas seja maior que 20.'
\echo 'INFO:'

SELECT Projeto.codproj, AVG( Trabalhaem.horasTrab )
FROM Projeto JOIN Trabalhaem ON Projeto.codproj = Trabalhaem.codProj
GROUP BY 1
HAVING AVG( Trabalhaem.horasTrab ) > 20;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 14. Obter os nomes de projetos correspondentes à consulta anterior. Usar a consulta'
\echo 'INFO: anterior como uma consulta aninhada à nova consulta.'
\echo 'INFO:'

SELECT Projeto.titulo
FROM ( SELECT Projeto.codproj, AVG( Trabalhaem.horasTrab )
        FROM Projeto JOIN Trabalhaem ON Projeto.codproj = Trabalhaem.codProj
        GROUP BY 1
        HAVING AVG( Trabalhaem.horasTrab ) > 20
     ) resultTable JOIN Projeto ON resultTable.codproj = Projeto.codproj;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 15. Obter o nome dos empregados e quantidade de projeto em que cada um deles'
\echo 'INFO: já trabalhou. Empregados que não trabalharam em projetos também devem'
\echo 'INFO: aparecer no resultado.'
\echo 'INFO:'

SELECT Empregado.nome, COUNT( Projeto.codProj ) AS projetos
FROM Empregado LEFT OUTER JOIN Trabalhaem ON Empregado.codemp   = Trabalhaem.codEmp
               LEFT OUTER JOIN Projeto    ON Trabalhaem.codProj = Projeto.codproj
GROUP BY 1;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 16. Obter o nome do projeto que mais teve empregados trabalhando.'
\echo 'INFO:'

WITH ContagemTable AS
(
    SELECT Projeto.codproj, Projeto.titulo, COUNT( Trabalhaem.codEmp )
    FROM Empregado JOIN Trabalhaem ON Empregado.codemp   = Trabalhaem.codEmp
                   JOIN Projeto    ON Trabalhaem.codproj = Projeto.codproj
    GROUP BY 1
)

SELECT ContagemTable.titulo
FROM ContagemTable
WHERE ContagemTable.count =
    (
        SELECT MAX( ContagemTable.count )
        FROM ContagemTable JOIN Projeto ON ContagemTable.codproj = Projeto.codproj
    );









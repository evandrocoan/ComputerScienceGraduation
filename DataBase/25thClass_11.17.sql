
/*

pais (codigo, nome, populacao)
cidade (codigo, nome, UF, regiao, codPais#)
    codPais REFERENCIA pais (codigo)
federacao (codigo, nome, codPais#)
    codPais REFERENCIA pais (codigo)

equipe (codigo, nome, codFederacao#, codCidade#)
    codFederacao REFERENCIA federacao (codigo)
    codPais REFERENCIA pais (codigo)
jogo (codTimeA#, codTimeB#, dataJogo, codCidade#, vencedor)
    codTimeA REFERENCIA equipe (codigo)
    codTimeB REFERENCIA equipe (codigo)
    codCidade REFERENCIA cidade (codigo)

*/

-- below code assumes that the name of your schema is public
DROP SCHEMA PUBLIC CASCADE;
CREATE SCHEMA PUBLIC;

-- For PostgreSQL 9.3 or greater, you may also need to restore the default grants.
GRANT ALL ON SCHEMA PUBLIC TO postgres;
GRANT ALL ON SCHEMA PUBLIC TO PUBLIC;

-- Include this exercise database.
\i 25thClass_11.17_jogos.sql


\echo 'INFO:'
\echo 'INFO: Exercícios Visão e Permissão'
\echo 'INFO: Para os exercícios abaixo, utilize a base de dados ‘jogos’ fornecida no Moodle.'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: pais (codigo, nome, populacao)'
\echo 'INFO: cidade (codigo, nome, UF, regiao, codPais#)'
\echo 'INFO:     codPais REFERENCIA pais (codigo)'
\echo 'INFO: federacao (codigo, nome, codPais#)'
\echo 'INFO:     codPais REFERENCIA pais (codigo)'
\echo 'INFO: '
\echo 'INFO: equipe (codigo, nome, codFederacao#, codCidade#)'
\echo 'INFO:     codFederacao REFERENCIA federacao (codigo)'
\echo 'INFO:     codPais REFERENCIA pais (codigo)'
\echo 'INFO: jogo (codTimeA#, codTimeB#, dataJogo, codCidade#, vencedor)'
\echo 'INFO:     codTimeA REFERENCIA equipe (codigo)'
\echo 'INFO:     codTimeB REFERENCIA equipe (codigo)'
\echo 'INFO:     codCidade REFERENCIA cidade (codigo)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 1. Construa as seguintes visões no Banco de Dados'
\echo 'INFO: a. Obter o nome das cidades que têm mais de um time.'
\echo 'INFO:'

-- DROP VIEW IF EXISTS view_cidades_com_mais_de_um_time CASCADE;

CREATE VIEW view_cidades_com_mais_de_um_time
AS
    SELECT cidade.nome, COUNT( cidade.nome )
    FROM cidade JOIN equipe ON cidade.codigo = equipe.codCidade
    GROUP BY 1
    HAVING COUNT( cidade.nome ) > 1;

SELECT *
FROM view_cidades_com_mais_de_um_time;

--      nome      | count
-- ---------------+-------
--  Florianópolis |     2
-- (1 row)

\echo 'INFO:'
\echo 'INFO: b. Obter a equipe que mais jogou jogos, e o número de partidas disputadas.'
\echo 'INFO:'

-- DROP VIEW IF EXISTS view_equipe_que_mais_jogou_jogos CASCADE;
-- DROP VIEW IF EXISTS view_equipes_e_contagem_de_jogos CASCADE;

CREATE VIEW view_equipes_e_contagem_de_jogos
AS
    SELECT equipe.codigo, COUNT( equipe.codigo )
    FROM equipe JOIN jogo ON equipe.codigo = jogo.codTimeA OR equipe.codigo = jogo.codTimeB
    GROUP BY 1
    HAVING COUNT( equipe.codigo ) > 1;

CREATE VIEW view_equipe_que_mais_jogou_jogos
AS
    SELECT equipe.nome, view_equipes_e_contagem_de_jogos.count
    FROM equipe JOIN view_equipes_e_contagem_de_jogos ON equipe.codigo = view_equipes_e_contagem_de_jogos.codigo
    WHERE view_equipes_e_contagem_de_jogos.count =
    (
        SELECT MAX( view_equipes_e_contagem_de_jogos.count )
        FROM view_equipes_e_contagem_de_jogos
    );

SELECT *
FROM view_equipe_que_mais_jogou_jogos;

--     nome     | count
-- -------------+-------
--  Corinthians |     3
-- (1 row)

\echo 'INFO:'
\echo 'INFO: c. Equipes que nunca empataram ou perderam como mandante.'
\echo 'INFO:'

-- DROP VIEW IF EXISTS view_equipes_que_nunca_empataram CASCADE;

CREATE VIEW view_equipes_que_nunca_empataram
AS
    (
        SELECT equipe.codigo
        FROM equipe
    )
    EXCEPT
    (
        SELECT equipe.codigo
        FROM equipe JOIN jogo ON equipe.codigo = jogo.codTimeA
        WHERE jogo.vencedor = 'Empate' OR jogo.vencedor = 'Visitante'
    )
    UNION
    (
        SELECT equipe.codigo
        FROM equipe JOIN jogo ON equipe.codigo = jogo.codTimeA
        WHERE jogo.vencedor = 'Mandante'
    );

    -- Short alternative query:
    -- SELECT * FROM equipe WHERE codigo not IN (
    --    SELECT equipe.codigo
    --    FROM equipe JOIN jogo ON equipe.codigo = jogo.codTimeA
    --    WHERE jogo.vencedor = 'Empate' OR jogo.vencedor = 'Visitante')

SELECT *
FROM view_equipes_que_nunca_empataram;

-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO: b. Obter a equipe que mais jogou jogos, e o número de partidas disputadas.'
-- \echo 'INFO:'





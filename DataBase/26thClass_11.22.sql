
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


\echo 'INFO: '
\echo 'INFO: Exercícios função e trigger'
\echo 'INFO: Para os exercícios abaixo, utilize a base de dados ‘jogos’ fornecida no Moodle.'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 1. Crie uma função que receba o nome de um time e retorne a sua pontuação após todos'
\echo 'INFO: os jogos. Dicas:'
\echo 'INFO:'
\echo 'INFO: - Utilize o seguinte código para iterar sobre o resultado de uma consulta.'
\echo 'INFO:     DECLARE target RECORD;'
\echo 'INFO:         ...'
\echo 'INFO:         FOR target IN query LOOP'
\echo 'INFO:             statements'
\echo 'INFO:     END LOOP'
\echo 'INFO:'
\echo 'INFO: - Se o time for mandante (time A) e o resultado do jogo foi ‘Mandante’, somou 3 pontos.'
\echo 'INFO: Se for mandante e empatou, somou 1 ponto. Se for visitante (time B) e o resultado'
\echo 'INFO: do jogo foi ‘Visitante’, somou 3 pontos. E se for visitante e empatou, somou 1 ponto.'
\echo 'INFO:'

-- SELECT * FROM equipe;
-- SELECT * FROM jogo;


CREATE OR REPLACE FUNCTION maiorPontiacaoDoTime( nomeDoTime varchar(100) )
RETURNS integer
AS $$

    DECLARE partida         RECORD;
    DECLARE codigoDoTime    integer;
    DECLARE pontuacaoDoTime integer DEFAULT 0;

    BEGIN

        SELECT codigo into codigoDoTime FROM equipe WHERE nomeDoTime = equipe.nome;
        -- RAISE INFO 'O codigo da equipe % eh %', nomeDoTime, codigoDoTime;

        -- Instead of save the whole table on memory (1.000.000 records), this saves memory iterating
        -- through each element as they are found.
        FOR partida IN

            SELECT *
            FROM jogo

        LOOP

            --
            -- http://stackoverflow.com/questions/11299037/postgresql-if-statement/37476851
            -- https://www.postgresql.org/docs/9.1/static/plpgsql-control-structures.html
            CASE
                WHEN partida.codTimeA = codigoDoTime AND partida.vencedor = 'Mandante' THEN

                    pontuacaoDoTime := pontuacaoDoTime + 3;
                    -- RAISE INFO 'A_MANDANT: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;

                WHEN partida.codTimeB = codigoDoTime AND partida.vencedor = 'Mandante' THEN

                    pontuacaoDoTime := pontuacaoDoTime + 1;
                    -- RAISE INFO 'B_MANDANT: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;

                WHEN partida.codTimeA = codigoDoTime AND partida.vencedor = 'Visitante' THEN

                    pontuacaoDoTime := pontuacaoDoTime + 1;
                    -- RAISE INFO 'A_VISITAN: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;

                WHEN partida.codTimeB = codigoDoTime AND partida.vencedor = 'Visitante' THEN

                    pontuacaoDoTime := pontuacaoDoTime + 3;
                    -- RAISE INFO 'B_VISITAN: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;

                WHEN partida.vencedor = 'Empate'
                AND ( partida.codTimeA = codigoDoTime OR partida.codTimeB = codigoDoTime ) THEN

                    pontuacaoDoTime := pontuacaoDoTime + 1;
                    -- RAISE INFO 'EMPATE___: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;

                ELSE

                    -- RAISE INFO 'ELSE_____: Doing nothing.';

            END CASE;

        END LOOP;

        RAISE INFO 'RETURNING: A pontuacaoDoTime da equipe % eh %', nomeDoTime, pontuacaoDoTime;
        -- RAISE INFO '';
        return pontuacaoDoTime;

    END;

$$ LANGUAGE plpgsql;


-- SELECT maiorPontiacaoDoTime( 'Figueirense' );
-- SELECT maiorPontiacaoDoTime( 'Avai' );

DO $$ BEGIN

    PERFORM maiorPontiacaoDoTime( 'Figueirense' );
    PERFORM maiorPontiacaoDoTime( 'Avai' );

END $$;

-- INFO:  RETURNING: A pontuacaoDoTime da equipe Figueirense eh 2
-- INFO:  RETURNING: A pontuacaoDoTime da equipe Avai eh 3

-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO:'
-- \echo 'INFO: b. Obter a equipe que mais jogou jogos, e o número de partidas disputadas.'
-- \echo 'INFO:'





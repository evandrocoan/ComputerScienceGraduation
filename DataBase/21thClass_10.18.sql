
/*

Always use the OUTTER JOIN's using the WHERE clause on the ON filter.
It is because the where may easily take out all the OUTTER JOIN's clauses.
Example:

SELECT m.nome, c.data
FROM medico m LEFT OUTER JOIN consulta c
ON m.codigo = c.codmed
WHERE data < ’01/01/2009’
GROUP BY m.nome

It will take all the NULL data returned by the OUTTER JOIN!

*/
/*

profissao (codigo, area, nome)
cidade (codigo, nome, UF)
paciente (codigo, nome, email, idade, fone, codProf, codCid)
   codProf REFERENCIA profissao (codigo)
   codCid REFERENCIA cidade (codigo)
medico (codigo, nome, email, CRM, codCid)
   codCid REFERENCIA cidade (codigo)
especializacao (codigo, nome, area)
convenio (codigo, nome)

medEsp (codEsp, codMed)
   codEsp REFERENCIA especializacao (codigo)
   codMed REFERENCIA medico (codigo)
consulta (data, hora, codPac, codMed, valor, codconv#)
   codPac REFERENCIA paciente (codigo)
   codMed REFERENCIA medico (codigo)
   codConv REFERENCIA convenio (codigo)
medicamento (codigo, descricao)
cons_medicame (data, hora, codPac, codMedica)
    codMedica REFERENCIA medicamento (codigo)
    (data, hora, codPac) REFERENCIA consulta (data, hora, codPac)

*/
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: Recupere os seguintes dados, utilizando comandos SQL:'
\echo 'INFO: Obs.: se houver alguma consulta cujo retorno seja diferente do apresentado na questão,'
\echo 'INFO: favor, avisar a professora.'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 1. O nome do paciente mais novo da clínica.'
\echo 'INFO:'

SELECT paciente.nome
FROM paciente
WHERE paciente.idade =
    ( SELECT MIN( paciente.idade )
      FROM paciente );

\echo 'INFO:       nome       '
\echo 'INFO: -----------------'
\echo 'INFO: Maria Aparecida'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 2. Obter a data e o horário das consultas da consulta mais cara (o valor  aparece neste'
\echo 'INFO: resultado apenas para que vejam o valor da consulta mais cara – a consulta SQL não deve'
\echo 'INFO: retornar isso no select).'
\echo 'INFO:'

SELECT consulta.data, consulta.hora
FROM consulta
WHERE consulta.valor =
    ( SELECT MAX( consulta.valor )
      FROM consulta );

\echo 'INFO:      Data        hora      valor'
\echo 'INFO: "2002-03-21"; "09:00:00";122.0000'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 3. (nível difícil) A data que representa o dia com o maior valor total arrecadado com'
\echo 'INFO: consultas (o sum aparece neste resultado abaixo apenas para que vejam o maior valor'
\echo 'INFO: arrecadado – a consulta SQL não deve retornar isso no select).'
\echo 'INFO:'

SELECT resultTableAlias.data
FROM
    (
        SELECT consulta.data, SUM( consulta.valor ) AS resultColumnAlias
        FROM consulta
        GROUP BY 1
    ) resultTableAlias
WHERE resultTableAlias.resultColumnAlias =
    (
        SELECT MAX( valoresTotais.valor )
        FROM
            ( SELECT consulta.data, SUM( consulta.valor ) AS valor
              FROM consulta
              GROUP BY 1
            ) valoresTotais
    );

\echo 'INFO:     data    '
\echo 'INFO: ------------'
\echo 'INFO:  2006-03-21'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 4. Data e hora das consultas, e nome dos convênios usados. Recupere todas as consultas,'
\echo 'INFO: mesmo quando não foi usado nenhum convenio.'
\echo 'INFO:'

SELECT consulta.data, consulta.hora, convenio.nome
FROM consulta LEFT OUTER JOIN convenio ON consulta.codconv = convenio.codigo;

\echo 'INFO:     data    |   hora   |    nome    '
\echo 'INFO: ------------+----------+------------'
\echo 'INFO:  2006-02-20 | 10:00:00 | Particular'
\echo 'INFO:  2006-02-21 | 11:00:00 | Particular'
\echo 'INFO:  2006-02-22 | 14:00:00 | Particular'
\echo 'INFO:  2006-02-23 | 13:00:00 | Uni'
\echo 'INFO:  2005-02-20 | 15:00:00 | Particular'
\echo 'INFO:  2005-02-21 | 16:00:00 | Solar'
\echo 'INFO:  2006-03-20 | 17:00:00 | Solar'
\echo 'INFO:  2006-03-21 | 09:00:00 | Particular'
\echo 'INFO:  2006-03-21 | 09:00:00 | Particular'
\echo 'INFO:  2006-03-21 | 09:00:00 | Particular'
\echo 'INFO:  2002-03-21 | 09:00:00 | '
\echo 'INFO:  2004-10-20 | 15:00:00 | '
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 5. No Banco de Dados acima, existe alguma consulta usando NATURAL JOIN que faria sentido?'
\echo 'INFO: Qual? Mostre o código SQL desta consulta com um filtro na cláusula WHERE.'
\echo 'INFO:'

SELECT DISTINCT medicamento.descricao
FROM consulta NATURAL JOIN cons_medicame
              JOIN medicamento ON cons_medicame.codMedica = medicamento.codigo
WHERE consulta.data > '2000/05/05';

\echo 'INFO:   descricao   '
\echo 'INFO: --------------'
\echo 'INFO:  Diclofenaco'
\echo 'INFO:  Hipoglos'
\echo 'INFO:  Sonrisal'
\echo 'INFO:  Tylenol'
\echo 'INFO:  Moura Brasil'
\echo 'INFO:  Engove'
\echo 'INFO:  Eno'
\echo 'INFO:  Aspirina'
\echo 'INFO:  Olina'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 6. Data das consultas e descrição dos medicamentos usados. Recupere todas as consultas,'
\echo 'INFO: mesmo aquelas em que não houve prescrição de nenhum medicamento. Ordene a resposta por'
\echo 'INFO: ordem crescente de descrição.'
\echo 'INFO:'

SELECT consulta.data, medicamento.descricao
FROM consulta LEFT OUTER JOIN cons_medicame ON ( consulta.data   = cons_medicame.data
                                              AND consulta.hora   = cons_medicame.hora
                                              AND consulta.codPac = cons_medicame.codPac )
              LEFT OUTER JOIN medicamento ON cons_medicame.codMedica = medicamento.codigo
ORDER BY medicamento.descricao;

\echo 'INFO:     data    |  descricao   '
\echo 'INFO: ------------+--------------'
\echo 'INFO:  2006-03-21 | Aspirina'
\echo 'INFO:  2006-02-23 | Diclofenaco'
\echo 'INFO:  2005-02-21 | Engove'
\echo 'INFO:  2006-02-23 | Engove'
\echo 'INFO:  2006-03-21 | Eno'
\echo 'INFO:  2006-02-21 | Hipoglos'
\echo 'INFO:  2005-02-21 | Moura Brasil'
\echo 'INFO:  2005-02-20 | Moura Brasil'
\echo 'INFO:  2006-02-23 | Moura Brasil'
\echo 'INFO:  2006-03-20 | Moura Brasil'
\echo 'INFO:  2006-03-21 | Moura Brasil'
\echo 'INFO:  2006-02-21 | Olina'
\echo 'INFO:  2006-02-20 | Olina'
\echo 'INFO:  2006-03-21 | Sonrisal'
\echo 'INFO:  2006-02-20 | Tylenol'
\echo 'INFO:  2006-03-21 | '
\echo 'INFO:  2004-10-20 | '
\echo 'INFO:  2006-02-22 | '
\echo 'INFO:  2006-03-21 | '
\echo 'INFO:  2002-03-21 | '
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 7. Selecionar o nome do paciente que seja mais velho do que todos os pacientes da cidade'
\echo 'INFO: de “Cruz Alta” (Coloquei mais colunas no SELECT para vocês visualizarem as idades).'
\echo 'INFO:'

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT paciente.nome, paciente.idade, cidade.nome
FROM paciente JOIN cidade ON paciente.codCid = cidade.codigo
WHERE cidade.nome = 'Cruz Alta'
    AND paciente.idade =
    (
        SELECT MAX( pacientesDeCruzAlta.idade ) AS paciente_mais_velho
        FROM
            (
                SELECT paciente.nome, paciente.idade
                FROM paciente JOIN cidade ON paciente.codCid = cidade.codigo
                WHERE cidade.nome = 'Cruz Alta'
            ) pacientesDeCruzAlta
    );

\echo 'INFO:        nome       | idade |   nome    '
\echo 'INFO: ------------------+-------+-----------'
\echo 'INFO:  Carolina Pereira |    44 | Cruz Alta'

SELECT paciente.nome, paciente.idade, cidade.nome
FROM paciente JOIN cidade ON paciente.codCid = cidade.codigo
WHERE cidade.nome = 'Cruz Alta';

\echo 'INFO:         nome        | idade |     nome     '
\echo 'INFO: --------------------+-------+--------------'
\echo 'INFO:  João Carlos        |    20 | Carazinho'
\echo 'INFO:  Maria Aparecida    |    10 | Cruz Alta'
\echo 'INFO:  Pedro Antonio      |    22 | Porto Alegre'
\echo 'INFO:  Marcos Vinicius    |    45 | Casca'
\echo 'INFO:  Carolina Pereira   |    44 | Cruz Alta'
\echo 'INFO:  Antonio Carlos     |    12 | Carazinho'
\echo 'INFO:  Mariana Faria      |    15 | São Paulo'
\echo 'INFO:  Joaquim José Silva |    34 | Carazinho'
\echo 'INFO:  Joana Darq         |    67 | Carazinho'
\echo 'INFO:  Pedro Avares       |    20 | Carazinho'
\echo 'INFO: (10 rows)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 8. Uma consulta com NATURAL JOIN entre paciente e medico, qual seria o resultado?'
\echo 'INFO: Mostre o SQL.'
\echo 'INFO:'

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT *
FROM paciente NATURAL JOIN medico;

\echo 'INFO:  codigo | nome | email | codcid | fone | codprof | idade | crm '
\echo 'INFO: --------+------+-------+--------+------+---------+-------+-----'
\echo 'INFO: (0 rows)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 9. Nome dos médicos e áreas de suas especializações. Recupere médicos que não tem'
\echo 'INFO: especialização e especializações que não foram associadas a nenhum médico.'
\echo 'INFO:'

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT medico.nome, especializacao.nome
FROM medEsp FULL OUTER JOIN medico        ON medEsp.codMed = medico.codigo
            FULL OUTER JOIN especializacao ON medEsp.codEsp = especializacao.codigo;

\echo 'INFO: "Paulina Tirou";"Pressao Arterial"'
\echo 'INFO: "José Paulo O";"Pressao Arterial"'
\echo 'INFO: "Nena Lina";"Enxaqueca"'
\echo 'INFO: "Ana Maria";"Problemas Mentais"'
\echo 'INFO: "Carla Ana";"Reabilitação Mental"'
\echo 'INFO: "Nena Lina";"Coluna"'
\echo 'INFO: "Paulina Tirou";"Joelho"'
\echo 'INFO: "Paulo Rangel";"Reabilitação"'
\echo 'INFO: "Paulina Tirou";"Obstetras"'
\echo 'INFO: "Paulo Rangel";"Enxaqueca"'
\echo 'INFO: "Luara dos Santos";"Geral"'
\echo 'INFO: "Luan dos Santos";"           "'
\echo 'INFO: "               ";"Surdez Temporária"'
\echo 'INFO: "               ";"Problemas Renais"'
\echo 'INFO: "               ";"Problemas Pulmonares"'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:        nome       |      nome      '
\echo 'INFO: ------------------+----------------'
\echo 'INFO:  Paulina Tirou    | Cardiologia'
\echo 'INFO:  José Paulo O     | Cardiologia'
\echo 'INFO:  Nena Lina        | Urologia'
\echo 'INFO:  Ana Maria        | Psiquiatria'
\echo 'INFO:  Carla Ana        | Fisioterapia'
\echo 'INFO:  Nena Lina        | Ortopedia'
\echo 'INFO:  Paulina Tirou    | Ortopedia'
\echo 'INFO:  Paulo Rangel     | Psicologia'
\echo 'INFO:  Paulina Tirou    | Ginecologista'
\echo 'INFO:  Paulo Rangel     | Urologia'
\echo 'INFO:  Luara dos Santos | Pediatria'
\echo 'INFO:  Luan dos Santos  | '
\echo 'INFO:                   | Fonoaudiologia'
\echo 'INFO:                   | Clínico Geral'
\echo 'INFO:                   | Clínico Geral'
\echo 'INFO: (15 rows)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 10. Nomes dos pacientes e datas de suas consultas anteriores a 2007. Mesmo os'
\echo 'INFO: pacientes que não tiveram consulta nesta época devem aparecer no resultado.'
\echo 'INFO:'

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT paciente.nome, consulta.data
FROM paciente LEFT OUTER JOIN consulta ON paciente.codigo = consulta.codPac
                                       AND consulta.data < '2007/01/01';

\echo 'INFO:         nome        |    data    '
\echo 'INFO: --------------------+------------'
\echo 'INFO:  João Carlos        | 2006-02-20'
\echo 'INFO:  Pedro Antonio      | 2006-02-21'
\echo 'INFO:  Marcos Vinicius    | 2006-02-22'
\echo 'INFO:  Carolina Pereira   | 2006-02-23'
\echo 'INFO:  Antonio Carlos     | 2005-02-20'
\echo 'INFO:  Marcos Vinicius    | 2005-02-21'
\echo 'INFO:  Mariana Faria      | 2006-03-20'
\echo 'INFO:  Pedro Antonio      | 2006-03-21'
\echo 'INFO:  Maria Aparecida    | 2006-03-21'
\echo 'INFO:  Antonio Carlos     | 2006-03-21'
\echo 'INFO:  Antonio Carlos     | 2002-03-21'
\echo 'INFO:  Antonio Carlos     | 2004-10-20'
\echo 'INFO:  Pedro Avares       | '
\echo 'INFO:  Joaquim José Silva | '
\echo 'INFO:  Joana Darq         | '
\echo 'INFO: (15 rows)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 11. Nome, email e idade dos pacientes e quantidade de consultas já realizadas na'
\echo 'INFO: clínica. Mesmo os pacientes que não tiveram consulta devem aparecer no resultado.'
\echo 'INFO: Ordenar o resultado pela quantidade.'
\echo 'INFO:'

SELECT paciente.nome, paciente.email, paciente.idade, COUNT( consulta.codPac )
FROM consulta RIGHT OUTER JOIN paciente ON consulta.codPac = paciente.codigo
GROUP BY 1, 2, 3
ORDER BY 4;

\echo 'INFO:         nome        |     email      | idade | count '
\echo 'INFO: --------------------+----------------+-------+-------'
\echo 'INFO:  Joaquim José Silva |                |    34 |     0'
\echo 'INFO:  Pedro Avares       |                |    20 |     0'
\echo 'INFO:  Joana Darq         |                |    67 |     0'
\echo 'INFO:  João Carlos        |                |    20 |     1'
\echo 'INFO:  Maria Aparecida    | MARIA@A.COM.BR |    10 |     1'
\echo 'INFO:  Mariana Faria      | mf@a.bcd.efg   |    15 |     1'
\echo 'INFO:  Carolina Pereira   | cp@a.bcd.efg   |    44 |     1'
\echo 'INFO:  Pedro Antonio      |                |    22 |     2'
\echo 'INFO:  Marcos Vinicius    |                |    45 |     2'
\echo 'INFO:  Antonio Carlos     | ac@a.bcd.efg   |    12 |     4'
\echo 'INFO: (10 rows)'

\echo 'INFO: "Joaquim José Silva";"";34;0'
\echo 'INFO: "Pedro Avares";"";20;0'
\echo 'INFO: "Joana Darq";"";67;0'
\echo 'INFO: "João Carlos";"";20;1'
\echo 'INFO: "Maria Aparecida";"MARIA@A.COM.BR";10;1'
\echo 'INFO: "Mariana Faria";"mf@a.bcd.efg";15;1'
\echo 'INFO: "Carolina Pereira";"cp@a.bcd.efg";44;1'
\echo 'INFO: "Pedro Antonio";"";22;2'
\echo 'INFO: "Marcos Vinicius";"";45;2'
\echo 'INFO: "Antonio Carlos";"ac@a.bcd.efg";12;4'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 12. Nome e fone dos pacientes e valor total já gasto com consultas. Mesmo os'
\echo 'INFO: pacientes que não tiveram consulta devem aparecer no resultado. Ordenar o resultado pelo somatório.'
\echo 'INFO:'

SELECT paciente.nome, paciente.fone, SUM( consulta.valor )
FROM paciente LEFT OUTER JOIN consulta ON paciente.codigo = consulta.codPac
GROUP BY 1, 2
ORDER BY 3;

\echo 'INFO: "Mariana Faria";"9999.9999";44.0000'
\echo 'INFO: "Carolina Pereira";"9999.9999";50.0000'
\echo 'INFO: "João Carlos";"33303930";100.0000'
\echo 'INFO: "Maria Aparecida";"33303325";100.0000'
\echo 'INFO: "Marcos Vinicius";"33303930";130.0000'
\echo 'INFO: "Pedro Antonio";"";200.0000'
\echo 'INFO: "Antonio Carlos";"9999.9999";442.0000'
\echo 'INFO: "Joaquim José Silva";"33559630";'
\echo 'INFO: "Joana Darq";"22304430";'
\echo 'INFO: "Pedro Avares";"13239830";'
\echo 'INFO:'
\echo 'INFO:'










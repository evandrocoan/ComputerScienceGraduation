
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
DO $$ BEGIN
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO 'Recupere os seguintes dados, utilizando comandos SQL:';
    RAISE INFO 'Obs.: se houver alguma consulta cujo retorno seja diferente do apresentado na questão,';
    RAISE INFO 'favor, avisar a professora.';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '1. O nome do paciente mais novo da clínica.';
END $$;

SELECT paciente.nome
FROM paciente
WHERE paciente.idade =
    ( SELECT MIN( paciente.idade )
      FROM paciente );

DO $$ BEGIN
    RAISE INFO '      nome       ';
    RAISE INFO '-----------------';
    RAISE INFO 'Maria Aparecida';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '2. Obter a data e o horário das consultas da consulta mais cara (o valor  aparece neste';
    RAISE INFO 'resultado apenas para que vejam o valor da consulta mais cara – a consulta SQL não deve';
    RAISE INFO 'retornar isso no select).';
END $$;

SELECT consulta.data, consulta.hora
FROM consulta
WHERE consulta.valor =
    ( SELECT MAX( consulta.valor )
      FROM consulta );

DO $$ BEGIN
    RAISE INFO '     Data        hora      valor';
    RAISE INFO '"2002-03-21"; "09:00:00";122.0000';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '3. (nível difícil) A data que representa o dia com o maior valor total arrecadado com';
    RAISE INFO 'consultas (o sum aparece neste resultado abaixo apenas para que vejam o maior valor';
    RAISE INFO 'arrecadado – a consulta SQL não deve retornar isso no select).';
END $$;

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

DO $$ BEGIN
    RAISE INFO '    data    ';
    RAISE INFO '------------';
    RAISE INFO ' 2006-03-21';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '4. Data e hora das consultas, e nome dos convênios usados. Recupere todas as consultas,';
    RAISE INFO 'mesmo quando não foi usado nenhum convenio.';
END $$;

SELECT consulta.data, consulta.hora, convenio.nome
FROM consulta LEFT OUTER JOIN convenio ON consulta.codconv = convenio.codigo;

DO $$ BEGIN
    RAISE INFO '    data    |   hora   |    nome    ';
    RAISE INFO '------------+----------+------------';
    RAISE INFO ' 2006-02-20 | 10:00:00 | Particular';
    RAISE INFO ' 2006-02-21 | 11:00:00 | Particular';
    RAISE INFO ' 2006-02-22 | 14:00:00 | Particular';
    RAISE INFO ' 2006-02-23 | 13:00:00 | Uni';
    RAISE INFO ' 2005-02-20 | 15:00:00 | Particular';
    RAISE INFO ' 2005-02-21 | 16:00:00 | Solar';
    RAISE INFO ' 2006-03-20 | 17:00:00 | Solar';
    RAISE INFO ' 2006-03-21 | 09:00:00 | Particular';
    RAISE INFO ' 2006-03-21 | 09:00:00 | Particular';
    RAISE INFO ' 2006-03-21 | 09:00:00 | Particular';
    RAISE INFO ' 2002-03-21 | 09:00:00 | ';
    RAISE INFO ' 2004-10-20 | 15:00:00 | ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '5. No Banco de Dados acima, existe alguma consulta usando NATURAL JOIN que faria sentido?';
    RAISE INFO 'Qual? Mostre o código SQL desta consulta com um filtro na cláusula WHERE.';
END $$;

SELECT DISTINCT medicamento.descricao
FROM consulta NATURAL JOIN cons_medicame
              JOIN medicamento ON cons_medicame.codMedica = medicamento.codigo
WHERE consulta.data > '2000/05/05';

DO $$ BEGIN
    RAISE INFO '  descricao   ';
    RAISE INFO '--------------';
    RAISE INFO ' Diclofenaco';
    RAISE INFO ' Hipoglos';
    RAISE INFO ' Sonrisal';
    RAISE INFO ' Tylenol';
    RAISE INFO ' Moura Brasil';
    RAISE INFO ' Engove';
    RAISE INFO ' Eno';
    RAISE INFO ' Aspirina';
    RAISE INFO ' Olina';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '6. Data das consultas e descrição dos medicamentos usados. Recupere todas as consultas,';
    RAISE INFO 'mesmo aquelas em que não houve prescrição de nenhum medicamento. Ordene a resposta por';
    RAISE INFO 'ordem crescente de descrição.';
END $$;

SELECT consulta.data, medicamento.descricao
FROM consulta LEFT OUTER JOIN cons_medicame ON ( consulta.data   = cons_medicame.data
                                              AND consulta.hora   = cons_medicame.hora
                                              AND consulta.codPac = cons_medicame.codPac )
              LEFT OUTER JOIN medicamento ON cons_medicame.codMedica = medicamento.codigo
ORDER BY medicamento.descricao;

DO $$ BEGIN
    RAISE INFO '    data    |  descricao   ';
    RAISE INFO '------------+--------------';
    RAISE INFO ' 2006-03-21 | Aspirina';
    RAISE INFO ' 2006-02-23 | Diclofenaco';
    RAISE INFO ' 2005-02-21 | Engove';
    RAISE INFO ' 2006-02-23 | Engove';
    RAISE INFO ' 2006-03-21 | Eno';
    RAISE INFO ' 2006-02-21 | Hipoglos';
    RAISE INFO ' 2005-02-21 | Moura Brasil';
    RAISE INFO ' 2005-02-20 | Moura Brasil';
    RAISE INFO ' 2006-02-23 | Moura Brasil';
    RAISE INFO ' 2006-03-20 | Moura Brasil';
    RAISE INFO ' 2006-03-21 | Moura Brasil';
    RAISE INFO ' 2006-02-21 | Olina';
    RAISE INFO ' 2006-02-20 | Olina';
    RAISE INFO ' 2006-03-21 | Sonrisal';
    RAISE INFO ' 2006-02-20 | Tylenol';
    RAISE INFO ' 2006-03-21 | ';
    RAISE INFO ' 2004-10-20 | ';
    RAISE INFO ' 2006-02-22 | ';
    RAISE INFO ' 2006-03-21 | ';
    RAISE INFO ' 2002-03-21 | ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '7. Selecionar o nome do paciente que seja mais velho do que todos os pacientes da cidade';
    RAISE INFO 'de “Cruz Alta” (Coloquei mais colunas no SELECT para vocês visualizarem as idades).';
END $$;

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

DO $$ BEGIN
RAISE INFO '       nome       | idade |   nome    ';
RAISE INFO '------------------+-------+-----------';
RAISE INFO ' Carolina Pereira |    44 | Cruz Alta';
END $$;

SELECT paciente.nome, paciente.idade, cidade.nome
FROM paciente JOIN cidade ON paciente.codCid = cidade.codigo
WHERE cidade.nome = 'Cruz Alta';

DO $$ BEGIN
    RAISE INFO '        nome        | idade |     nome     ';
    RAISE INFO '--------------------+-------+--------------';
    RAISE INFO ' João Carlos        |    20 | Carazinho';
    RAISE INFO ' Maria Aparecida    |    10 | Cruz Alta';
    RAISE INFO ' Pedro Antonio      |    22 | Porto Alegre';
    RAISE INFO ' Marcos Vinicius    |    45 | Casca';
    RAISE INFO ' Carolina Pereira   |    44 | Cruz Alta';
    RAISE INFO ' Antonio Carlos     |    12 | Carazinho';
    RAISE INFO ' Mariana Faria      |    15 | São Paulo';
    RAISE INFO ' Joaquim José Silva |    34 | Carazinho';
    RAISE INFO ' Joana Darq         |    67 | Carazinho';
    RAISE INFO ' Pedro Avares       |    20 | Carazinho';
    RAISE INFO '(10 rows)';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '8. Uma consulta com NATURAL JOIN entre paciente e medico, qual seria o resultado?';
    RAISE INFO 'Mostre o SQL.';
END $$;

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT *
FROM paciente NATURAL JOIN medico;

DO $$ BEGIN
    RAISE INFO ' codigo | nome | email | codcid | fone | codprof | idade | crm ';
    RAISE INFO '--------+------+-------+--------+------+---------+-------+-----';
    RAISE INFO '(0 rows)';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '9. Nome dos médicos e áreas de suas especializações. Recupere médicos que não tem';
    RAISE INFO 'especialização e especializações que não foram associadas a nenhum médico.';
END $$;

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT medico.nome, especializacao.nome
FROM medEsp FULL OUTER JOIN medico        ON medEsp.codMed = medico.codigo
            FULL OUTER JOIN especializacao ON medEsp.codEsp = especializacao.codigo;

DO $$ BEGIN
    RAISE INFO '"Paulina Tirou";"Pressao Arterial"';
    RAISE INFO '"José Paulo O";"Pressao Arterial"';
    RAISE INFO '"Nena Lina";"Enxaqueca"';
    RAISE INFO '"Ana Maria";"Problemas Mentais"';
    RAISE INFO '"Carla Ana";"Reabilitação Mental"';
    RAISE INFO '"Nena Lina";"Coluna"';
    RAISE INFO '"Paulina Tirou";"Joelho"';
    RAISE INFO '"Paulo Rangel";"Reabilitação"';
    RAISE INFO '"Paulina Tirou";"Obstetras"';
    RAISE INFO '"Paulo Rangel";"Enxaqueca"';
    RAISE INFO '"Luara dos Santos";"Geral"';
    RAISE INFO '"Luan dos Santos";"           "';
    RAISE INFO '"               ";"Surdez Temporária"';
    RAISE INFO '"               ";"Problemas Renais"';
    RAISE INFO '"               ";"Problemas Pulmonares"';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '       nome       |      nome      ';
    RAISE INFO '------------------+----------------';
    RAISE INFO ' Paulina Tirou    | Cardiologia';
    RAISE INFO ' José Paulo O     | Cardiologia';
    RAISE INFO ' Nena Lina        | Urologia';
    RAISE INFO ' Ana Maria        | Psiquiatria';
    RAISE INFO ' Carla Ana        | Fisioterapia';
    RAISE INFO ' Nena Lina        | Ortopedia';
    RAISE INFO ' Paulina Tirou    | Ortopedia';
    RAISE INFO ' Paulo Rangel     | Psicologia';
    RAISE INFO ' Paulina Tirou    | Ginecologista';
    RAISE INFO ' Paulo Rangel     | Urologia';
    RAISE INFO ' Luara dos Santos | Pediatria';
    RAISE INFO ' Luan dos Santos  | ';
    RAISE INFO '                  | Fonoaudiologia';
    RAISE INFO '                  | Clínico Geral';
    RAISE INFO '                  | Clínico Geral';
    RAISE INFO '(15 rows)';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '10. Nomes dos pacientes e datas de suas consultas anteriores a 2007. Mesmo os';
    RAISE INFO 'pacientes que não tiveram consulta nesta época devem aparecer no resultado.';
END $$;

-- WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG, WRONG,
SELECT paciente.nome, consulta.data
FROM paciente LEFT OUTER JOIN consulta ON paciente.codigo = consulta.codPac
                                       AND consulta.data < '2007/01/01';

DO $$ BEGIN
    RAISE INFO '        nome        |    data    ';
    RAISE INFO '--------------------+------------';
    RAISE INFO ' João Carlos        | 2006-02-20';
    RAISE INFO ' Pedro Antonio      | 2006-02-21';
    RAISE INFO ' Marcos Vinicius    | 2006-02-22';
    RAISE INFO ' Carolina Pereira   | 2006-02-23';
    RAISE INFO ' Antonio Carlos     | 2005-02-20';
    RAISE INFO ' Marcos Vinicius    | 2005-02-21';
    RAISE INFO ' Mariana Faria      | 2006-03-20';
    RAISE INFO ' Pedro Antonio      | 2006-03-21';
    RAISE INFO ' Maria Aparecida    | 2006-03-21';
    RAISE INFO ' Antonio Carlos     | 2006-03-21';
    RAISE INFO ' Antonio Carlos     | 2002-03-21';
    RAISE INFO ' Antonio Carlos     | 2004-10-20';
    RAISE INFO ' Pedro Avares       | ';
    RAISE INFO ' Joaquim José Silva | ';
    RAISE INFO ' Joana Darq         | ';
    RAISE INFO '(15 rows)';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '11. Nome, email e idade dos pacientes e quantidade de consultas já realizadas na';
    RAISE INFO 'clínica. Mesmo os pacientes que não tiveram consulta devem aparecer no resultado.';
    RAISE INFO 'Ordenar o resultado pela quantidade.';
END $$;

SELECT paciente.nome, paciente.email, paciente.idade, COUNT( consulta.codPac )
FROM consulta RIGHT OUTER JOIN paciente ON consulta.codPac = paciente.codigo
GROUP BY 1, 2, 3
ORDER BY 4;

DO $$ BEGIN
    RAISE INFO '        nome        |     email      | idade | count ';
    RAISE INFO '--------------------+----------------+-------+-------';
    RAISE INFO ' Joaquim José Silva |                |    34 |     0';
    RAISE INFO ' Pedro Avares       |                |    20 |     0';
    RAISE INFO ' Joana Darq         |                |    67 |     0';
    RAISE INFO ' João Carlos        |                |    20 |     1';
    RAISE INFO ' Maria Aparecida    | MARIA@A.COM.BR |    10 |     1';
    RAISE INFO ' Mariana Faria      | mf@a.bcd.efg   |    15 |     1';
    RAISE INFO ' Carolina Pereira   | cp@a.bcd.efg   |    44 |     1';
    RAISE INFO ' Pedro Antonio      |                |    22 |     2';
    RAISE INFO ' Marcos Vinicius    |                |    45 |     2';
    RAISE INFO ' Antonio Carlos     | ac@a.bcd.efg   |    12 |     4';
    RAISE INFO '(10 rows)';

    RAISE INFO '"Joaquim José Silva";"";34;0';
    RAISE INFO '"Pedro Avares";"";20;0';
    RAISE INFO '"Joana Darq";"";67;0';
    RAISE INFO '"João Carlos";"";20;1';
    RAISE INFO '"Maria Aparecida";"MARIA@A.COM.BR";10;1';
    RAISE INFO '"Mariana Faria";"mf@a.bcd.efg";15;1';
    RAISE INFO '"Carolina Pereira";"cp@a.bcd.efg";44;1';
    RAISE INFO '"Pedro Antonio";"";22;2';
    RAISE INFO '"Marcos Vinicius";"";45;2';
    RAISE INFO '"Antonio Carlos";"ac@a.bcd.efg";12;4';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '    ';
    RAISE INFO '12. Nome e fone dos pacientes e valor total já gasto com consultas. Mesmo os';
    RAISE INFO 'pacientes que não tiveram consulta devem aparecer no resultado. Ordenar o resultado pelo somatório.';
END $$;

SELECT paciente.nome, paciente.fone, SUM( consulta.valor )
FROM paciente LEFT OUTER JOIN consulta ON paciente.codigo = consulta.codPac
GROUP BY 1, 2
ORDER BY 3;

DO $$ BEGIN
    RAISE INFO '"Mariana Faria";"9999.9999";44.0000';
    RAISE INFO '"Carolina Pereira";"9999.9999";50.0000';
    RAISE INFO '"João Carlos";"33303930";100.0000';
    RAISE INFO '"Maria Aparecida";"33303325";100.0000';
    RAISE INFO '"Marcos Vinicius";"33303930";130.0000';
    RAISE INFO '"Pedro Antonio";"";200.0000';
    RAISE INFO '"Antonio Carlos";"9999.9999";442.0000';
    RAISE INFO '"Joaquim José Silva";"33559630";';
    RAISE INFO '"Joana Darq";"22304430";';
    RAISE INFO '"Pedro Avares";"13239830";';
    RAISE INFO 'col1      col2';
    RAISE INFO '-----    -------';
END $$;













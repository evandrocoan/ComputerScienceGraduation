

-- Always use the OUTTER JOIN's using the WHERE clause on the ON filter.
-- It is because the where may easily take out all the OUTTER JOIN's clauses.
-- Example:
-- 
-- SELECT m.nome, c.data
-- FROM medico m LEFT OUTER
--  JOIN
--  consulta c
-- ON m.codigo = c.codmed
-- WHERE data < ’01/01/2009’
-- GROUP BY m.nome
-- 
-- It will take all the NULL data returned by the OUTTER JOIN!

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
/*

-- Recupere os seguintes dados, utilizando comandos SQL:
-- Obs.: se houver alguma consulta cujo retorno seja diferente do apresentado na questão,
-- favor, avisar a professora. 

--
-- 1. O nome do paciente mais novo da clínica.
--
--       nome       
-- -----------------
--  Maria Aparecida

SELECT paciente.nome
FROM paciente
WHERE paciente.idade = 
    ( SELECT MIN( paciente.idade )
      FROM paciente );

-- 
-- 2. Obter a data e o horário das consultas da consulta mais cara (o valor  aparece neste
-- resultado apenas para que vejam o valor da consulta mais cara – a consulta SQL não deve
-- retornar isso no select).
--  
--      Data        hora      valor
-- "2002-03-21"; "09:00:00";122.0000

SELECT consulta.data, consulta.hora
FROM consulta
WHERE consulta.valor = 
    ( SELECT MAX( consulta.valor )
      FROM consulta );

--        
-- 3. (nível difícil) A data que representa o dia com o maior valor total arrecadado com
-- consultas (o sum aparece neste resultado abaixo apenas para que vejam o maior valor
-- arrecadado – a consulta SQL não deve retornar isso no select).
-- 
*/
SELECT consulta.data, SUM( consulta.valor ) AS valor
FROM consulta
WHERE consulta.valor = 
    ( SELECT MAX( valoresTotais.valor )
      FROM 
          ( SELECT consulta.data, SUM( consulta.valor ) AS valor
            FROM consulta
            GROUP BY 1 ) valoresTotais
    )


-- 
-- 4. Data e hora das consultas, e nome dos convênios usados. Recupere todas as consultas,
-- mesmo quando não foi usado nenhum convenio.
-- 
-- 
-- 
-- 
-- 5. No Banco de Dados acima, existe alguma consulta usando NATURAL JOIN que faria sentido?
-- Qual? Mostre o código SQL desta consulta com um filtro na cláusula WHERE.
-- 
-- 6. Data das consultas e descrição dos medicamentos usados. Recupere todas as consultas,
-- mesmo aquelas em que não houve prescrição de nenhum medicamento. Ordene a resposta por
-- ordem crescente de descrição.
-- 
-- 7. Selecionar o nome do paciente que seja mais velho do que todos os pacientes da cidade
-- de “Cruz Alta” (Coloquei mais colunas no SELECT para vocês visualizarem as idades).
-- 
-- 
-- 
-- 8. Uma consulta com NATURAL JOIN entre paciente e medico, qual seria o resultado?
-- Mostre o SQL.
-- 
-- 9. Nome dos médicos e áreas de suas especializações. Recupere médicos que não tem
-- especialização e especializações que não foram associadas a nenhum médico. 
-- 
-- "Paulina Tirou";"Pressao Arterial"
-- "José Paulo O";"Pressao Arterial"
-- "Nena Lina";"Enxaqueca"
-- "Ana Maria";"Problemas Mentais"
-- "Carla Ana";"Reabilitação Mental"
-- "Nena Lina";"Coluna"
-- "Paulina Tirou";"Joelho"
-- "Paulo Rangel";"Reabilitação"
-- "Paulina Tirou";"Obstetras"
-- "Paulo Rangel";"Enxaqueca"
-- "Luara dos Santos";"Geral"
-- "Luan dos Santos";"           "
-- "               ";"Surdez Temporária"
-- "               ";"Problemas Renais"
-- "               ";"Problemas Pulmonares"
-- 
-- 10. Nomes dos pacientes e datas de suas consultas anteriores a 2007. Mesmo os
-- pacientes que não tiveram consulta nesta época devem aparecer no resultado.  
-- 
-- 
-- 11. Nome, email e idade dos pacientes e quantidade de consultas já realizadas na
-- clínica. Mesmo os pacientes que não tiveram consulta devem aparecer no resultado.
-- Ordenar o resultado pela quantidade.
-- 
-- "Joaquim José Silva";"";34;0
-- "Pedro Avares";"";20;0
-- "Joana Darq";"";67;0
-- "João Carlos";"";20;1
-- "Maria Aparecida";"MARIA@A.COM.BR";10;1
-- "Mariana Faria";"mf@a.bcd.efg";15;1
-- "Carolina Pereira";"cp@a.bcd.efg";44;1
-- "Pedro Antonio";"";22;2
-- "Marcos Vinicius";"";45;2
-- "Antonio Carlos";"ac@a.bcd.efg";12;4
-- 
-- 12. Nome e fone dos pacientes e valor total já gasto com consultas. Mesmo os
-- pacientes que não tiveram consulta devem aparecer no resultado. Ordenar o resultado pelo somatório.
-- 
-- "Mariana Faria";"9999.9999";44.0000
-- "Carolina Pereira";"9999.9999";50.0000
-- "João Carlos";"33303930";100.0000
-- "Maria Aparecida";"33303325";100.0000
-- "Marcos Vinicius";"33303930";130.0000
-- "Pedro Antonio";"";200.0000
-- "Antonio Carlos";"9999.9999";442.0000
-- "Joaquim José Silva";"33559630";
-- "Joana Darq";"22304430";
-- "Pedro Avares";"13239830";

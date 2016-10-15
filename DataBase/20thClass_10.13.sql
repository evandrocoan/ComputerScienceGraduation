/*

SELECT the BD keywords to be UPPER case automatically on the VM.

# Aggregation functions

1. We may only use AVG, COUNT, MAX, MIN, etc. clauses on the SELECT statement.

2. SELECT COUNT will only not count a line when all its values are NULL.

3. When we need to use the aggregation function within more columns on the SELECT clause,
we need to use the SQL extended with the GROUP BY clause beyond the SELECT, FROM, WHERE.

SELECT AVG( salary ), nome
FROM workers
WHERE birthDate > '10/10/1990'
GROUP BY nome ASC

ASC  - Ascending order ( it is the default, does not need to be explicit.
DESC - Descending order

Otherwise, it will return doubled results:
400 RS
400 SC
400 SP

But within GROUP BY, it will do: (the operation to each one, on each one)
550 RS
500 SC
250 SP

4. The best GROUP BY analogy is the FOR EACH.

5. The full GROUP BY command is:
GROUP BY <attributes> HAVING

Here in, the HAVING is optional. Its purpose is to test each one of the results, as:
SELECT AVG( salary ), nome
FROM workers
WHERE birthDate > '10/10/1990'
GROUP BY nome ASC
HAVING count(*) > 10

6. Never EVER use count(*) > 10 on the WHERE clause.

*/


-- Recupere os seguintes dados, utilizando comandos SQL:
-- Obs.: se houver alguma consulta cujo retorno seja diferente do apresentado na
-- questão, favor, avisar a professora.

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
--
-- 1. Quantidade de médicos com consultas anteriores a ‘2005/09/01’
-- 2

SELECT COUNT( DISTINCT codMed )
FROM consulta
WHERE DATA < '2005/09/01';

--
-- 2. Valor total das consultas já feitas.
--
-- 824.0000

SELECT SUM( valor )
FROM consulta;


--
-- 3. O código, nome e CRM dos médicos, e a quantidade de consultas realizadas por cada
--  um em ‘2006’. O resultado deve ser ordenado por nome do médico.
--
-- 2;"Ana Maria";"555453";2
-- 4;"Carla Ana";"987666";1
-- 3;"José Paulo O";"677755";2
-- 7;"Luara dos Santos";"983456";2
-- 1;"Paulo Rangel";"23453";1

SELECT medico.codigo, medico.nome, medico.CRM, COUNT(*)
FROM consulta JOIN medico ON consulta.codMed = medico.codigo
WHERE consulta.data BETWEEN '2006/01/01' AND'2006/12/31'
GROUP BY medico.codigo, medico.nome, medico.CRM
ORDER BY medico.nome ASC;

--
-- 4. Data e hora da consulta, nome do médico e nome do paciente. Pacientes devem ter
-- idade inferior a 18 anos, e a especialização do médico deve ser ‘Pediatria’.
-- Ordenar por data e hora da consulta.
--
-- "2006-03-21";"09:00:00";"Luara dos Santos";"Maria Aparecida"
-- "2006-03-21";"09:00:00";"Luara dos Santos";"Antonio Carlos"

SELECT consulta.data, consulta.hora, medico.nome, paciente.nome
FROM consulta JOIN medico         ON consulta.codMed = medico.codigo
              JOIN paciente       ON consulta.codPac = paciente.codigo
              JOIN medEsp         ON medEsp.codMed   = medico.codigo
              JOIN especializacao ON medEsp.codEsp   = especializacao.codigo
WHERE especializacao.nome = 'Pediatria'
ORDER BY consulta.data, consulta.hora ASC;

--
-- 5. Data das consultas, e para cada data o somatório total dos valores, desde que
-- este total seja maior do que 100.00.
--
-- "2006-03-21";300.0000

SELECT consulta.data, sum( consulta.valor )
FROM consulta
GROUP BY consulta.data
HAVING sum( consulta.valor ) > 100;

--
-- 6. Nome das cidades, e a quantidade de pacientes moradores em cada uma delas.
-- Ordenar o resultado por ordem decrescente de nome de cidade.
-- "São Paulo";1
-- "Porto Alegre";1
-- "Cruz Alta";2
-- "Casca";1
-- "Carazinho";5
*/
SELECT cidade.nome, count(*)
FROM cidade JOIN paciente ON paciente.codCid = cidade.codigo
GROUP BY cidade.nome
ORDER BY cidade.nome DESC;

--
-- 7. Descrição do medicamento e para cada um deles a quantidade receitada
-- nas consultas. Ordenar o resultado por pela descrição do medicamento, e
-- colocar na resposta apenas aqueles cuja quantidade receitada é maior do que 1.
--
-- "Engove";2
-- "Moura Brasil";5
-- "Olina";2



--
-- 8. Nome da especialização e nome do médico que possui CRM = 23453.
-- Ordenar o resultado por ordem decrescente do nome da especialização.
--
-- "Urologia";"Paulo Rangel"
-- "Psicologia";"Paulo Rangel"



--
-- 9. Código, nome e CRM dos médicos que possuem consulta. Ordenar o resultado
-- por CRM do médico, por ordem decrescente.
--
-- 4;"Carla Ana";"987666"
-- 7;"Luara dos Santos";"983456"
-- 3;"José Paulo O";"677755"
-- 2;"Ana Maria";"555453"
-- 1;"Paulo Rangel";"23453"



--
-- 10. Nome do médico e para cada um deles a quantidade de consultas efetuadas.
-- Devem aparecer no resultado apenas médicos com mais de uma consulta efetuada.
--
-- "Luara dos Santos";2
-- "José Paulo O";2
-- "Paulo Rangel";2
-- "Ana Maria";3



--
-- 11. Descrição dos medicamentos prescritos, e para cada um deles a quantidade
-- total prescrita. Ordenar pela descrição.
--
-- "Aspirina";1
-- "Diclofenaco";1
-- "Engove";2
-- "Eno";1
-- "Hipoglos";1
-- "Moura Brasil";5
-- "Olina";2
-- "Sonrisal";1
-- "Tylenol";1



--
-- 12. Data da consulta, e a quantidade de pacientes com idade menor que 25.
--
-- "2006-03-20";1
-- "2006-02-21";1
-- "2006-02-20";1
-- "2006-03-21";3
-- "2005-02-20";1



--
-- 13. Nome das cidades, e a quantidade de pacientes moradores em cada uma delas,
-- desde que o número de pacientes moradores seja mais do que 1 (>=2). Ordenar o
-- resultado por ordem decrescente de nome de cidade.
--
-- "Carazinho";5
-- "Cruz Alta";2



--
-- 14. Nome das cidades, e a quantidade de pacientes moradores em cada uma delas,
-- cujo nome da cidade comece com ‘C’. Ordenar o resultado por ordem decrescente de
-- nome de cidade.
--
-- "Cruz Alta";2
-- "Casca";1
-- "Carazinho";5



--
-- 15. Nome do médico, e para cada um deles a quantidade de consultas efetuadas; o
-- nome do médico deve começar com ‘P’ ou com ‘C’.
--
-- "Paulo Rangel";2
-- "Carla Ana";1






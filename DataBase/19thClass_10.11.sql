/*

FROM livro                --> FROM indica a(s) tabela(s)
WHERE nrPaginas > 350     --> WHERE representa a sig (seleção)
SELECT titulo, ano        --> SELECT representa a pi (projeção)


tipo (codigo, nome)
cidade (codigo, nome, UF)
editora (codigo, nome, endereço, codCid#)
   codCid referencia cidade(codigo)
autor (codigo, nome, email, dataNasc, codCid#)
   codCid referencia cidade(codigo)
livro (codigo, titulo, idioma, codTip#, codEdi#,precoSugerido)
   codTip referencia tipo(codigo)
   codEdi referencia editora(codigo)

autoria (codAut#, codLiv#, data)
   codAut referencia autor (codigo)
   codLiv referencia livro (codigo)
revisao (codAut#, codLiv#, codRev#, data, alteracao, obs)
   (codAut, codLiv) referencia autoria (codAut, codLiv)
   codRev referencia autor (codigo)

*/

--
-- 1. Recuperar todos os atributos e valores de livros.
--
-- Retorno:
-- 300;"Maravilhas da terra";"Português";170;100;100.00
-- 340;"Espaço Literal";"Português";190;150;70.00
-- 400;"Hello World";"Inglês";120;120;90.00
-- 500;"Hi There";"Inglês";120;120;115.00600;
-- "Hei you!!!";"Inglês";120;120;112.00

SELECT *
FROM LIVRO;

--
-- 2. Recuperar todos os dados de livros cujo idioma é ‘Português’.
--
-- Retorno:
-- 300;"Maravilhas da terra";"Português";170;100;100.00
-- 340;"Espaço Literal";"Português";190;150;70.00

SELECT *
FROM LIVRO
WHERE IDIOMA = 'Português';

--
-- 3. Recuperar todos os dados de autores cujo nome comece com ‘C’.
--
-- Retorno:
-- 123;"Carlito";"ca@abc.com";"1981-01-23";120
-- 500;"Charlie";"ca@abc.com";"1981-01-23";120

SELECT *
FROM autor
WHERE nome like 'C%';

--
-- 4. Recuperar o nome das cidades, que comece com ‘S’ e que sejam do ‘RS’ ou de ‘SP’.
--
-- Retorno:
-- "São Paulo"
-- "Santa Maria"

SELECT *
FROM cidade
WHERE cidade.nome like 'S%' and ( UF = 'SP' OR UF = 'RS' );

--
-- 5. Recuperar titulo dos livros e preco sugerido. O preço deve ser maior do que 100 e
-- o título deve iniciar com ‘H’.  Use renomeação de tabela e atributos para responder
-- esta consulta.
--
-- Retorno:
-- "Hi There";115.00
-- "Hei you!!!";112.00
--

SELECT titulo, precoSugerido
FROM livro
WHERE precoSugerido > 100 and titulo LIKE 'H%';

--
-- 6. Recuperar todos os autores que tenham nascido na década de 80 e cujo nome comece com C.
--
-- Retorno:
-- 123;"Carlito";"ca@abc.com";"1981-01-23";120
-- 500;"Charlie";"ca@abc.com";"1981-01-23";120

SELECT *
FROM autor
WHERE dataNasc < '1990/01/01' and dataNasc > '1979/12/31' and nome LIKE 'C%';

--
-- 7. Recuperar idiomas existentes para os livros cadastrados. A resposta não pode possuir
-- resultados repetidos.
--
-- Retorno:
-- "Inglês"
-- "Português"

SELECT DISTINCT idioma
FROM livro;

--
-- 8. Selecionar título dos livros com preços sugeridos superiores a 50,00
--
-- Retorno:
-- "Maravilhas da terra"
--  "Espaço Literal"
--  "Hello World"
--  "Hi There"
--   "Hei you!!!"

SELECT titulo
FROM livro
WHERE precoSugerido > 50;

--
-- 9. Selecionar nome do autor, e nome e UF das cidades dos autores cuja data de nascimento
-- seja maior do que ‘1970-01-01’.
--
-- a. Opção 1: produto cartesiano
-- b. Opção 2: join
--
-- Retorno:
-- "Luiz";"Marau";"RS"
-- "Luana";"Floripa";"SC"
-- "Ana Paula";"São Paulo";"SP"
-- "Leila";"Niterói";"RJ"
-- "Carlito";"Niterói";"RJ"
-- "Charlie";"Niterói";"RJ"

SELECT autor.nome, cidade.nome, cidade.UF
FROM autor, cidade
WHERE dataNasc > '1970/01/01' and autor.codCid = cidade.codigo;

--
-- 10. Selecionar nome dos autores e títulos dos livros cujo idioma ‘Português’.
--
-- Retorno:
-- "Luiz";"Maravilhas da terra"
-- "Luiz";"Espaço Literal"
--
-- a. Opção 1:produto cartesiano

SELECT autor.nome, livro.titulo
FROM autor, livro, autoria
WHERE livro.idioma = 'Português'
	and autoria.codAut = autor.codigo
	and autoria.codLiv = livro.codigo;

--
-- b. Opção 2:join

SELECT autor.nome, livro.titulo
FROM autoria JOIN autor ON autoria.codAut = autor.codigo
    JOIN livro ON autoria.codLiv = livro.codigo
WHERE livro.idioma = 'Português';

--
-- 11. Selecionar as alterações solicitadas para a as autorias feitas no ano de 2000
--
-- Retorno:
-- "sem alterações"
-- "sem alterações"
-- "conferir numeração de páginas"
--
-- a. Opção 1:produto cartesiano

SELECT alteracao
FROM autoria, revisao
WHERE autoria.data >= '01/01/2000'
        and autoria.data < '01/01/2001'
        and autoria.codLiv = revisao.codLiv;

--
-- b. Opção 2:JOIN

SELECT alteracao
FROM autoria join revisao on autoria.codLiv = revisao.codLiv
WHERE autoria.data >= '01/01/2000'
        and autoria.data < '01/01/2001';

--
-- 12. Selecionar título dos livros, nomes de seus autores, nomes de seus revisores e qual a
-- alteração solicitada.
--
-- Retorno:
-- "Maravilhas da terra";"Luiz";"Charlie";"sem alterações"
-- "Espaço Literal";"Luiz";"Charlie";"ver labels das tabelas"
-- "Hello World";"Charlie";"Luana";"arrumar titulos capitulos"
-- "Hi There";"Charlie";"Carlito";"sem alterações"
-- "Hei you!!!";"Charlie";"Luiz";"conferir numeração de páginas"
--
-- a. Opção 1:produto cartesiano

SELECT livro.titulo, autorEscritor.nome, autorRevisor.nome, revisao.alteracao
FROM livro, autor autorEscritor, autor autorRevisor, autoria, revisao
WHERE autoria.codLiv = livro.codigo
  and autoria.codAut = autorEscritor.codigo
  and revisao.codLiv = livro.codigo
  and revisao.codRev = autorRevisor.codigo;

--
-- b. Opção 2:JOIN

SELECT livro.titulo, autorEscritor.nome, autorRevisor.nome, revisao.alteracao
FROM autoria join livro               on autoria.codLiv = livro.codigo
             join autor autorEscritor on autoria.codAut = autorEscritor.codigo
             join revisao             on revisao.codLiv = livro.codigo
             join autor autorRevisor  on revisao.codRev = autorRevisor.codigo;

--
-- 13. Selecionar título dos livros e preço sugerido, cujo o nome do tipo contenha a
-- palavra ‘Literatura’.
--
-- Retorno:
-- "Hello World";90.00
-- "Hi There";115.00
-- "Hei you!!!";112.00
--
-- a. Opção 1:produto cartesiano

SELECT livro.titulo, livro.precoSugerido
FROM livro, tipo
WHERE livro.codTip = tipo.codigo
  and tipo.nome like '%Literatura%';

--
-- b. Opção 2:JOIN

SELECT livro.titulo, livro.precoSugerido
FROM livro join tipo on livro.codTip = tipo.codigo
where tipo.nome like '%Literatura%';

--
-- 14. Selecionar o nome dos autores, nome e UF das cidades. O nome da cidade deve
-- ser igual a ‘Floripa’ ou ‘Marau’.
--
-- Retorno:
-- "Luiz";"Marau";"RS"
-- "Luana";"Floripa";"SC"
--
-- a. Opção 1:produto cartesiano

SELECT autor.nome, cidade.nome, cidade.UF
FROM autor, cidade
WHERE autor.codCid = cidade.codigo
  and ( cidade.nome like 'Floripa'
        or cidade.nome like 'Marau' );

--
-- b. Opção 2:JOIN

SELECT autor.nome, cidade.nome, cidade.UF
FROM autor join cidade on autor.codCid = cidade.codigo
where cidade.nome like 'Floripa'
      or cidade.nome like 'Marau';







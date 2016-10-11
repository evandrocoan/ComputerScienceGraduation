/*
-- 
-- Notes: Show line number on SQL editor and set .sql files to open with Sublime Text.
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
FROM LIVRO

--
-- 2. Recuperar todos os dados de livros cujo idioma é ‘Português’.
-- 
-- Retorno:
-- 300;"Maravilhas da terra";"Português";170;100;100.00
-- 340;"Espaço Literal";"Português";190;150;70.00

SELECT *
FROM LIVRO
WHERE IDIOMA = 'Português'

-- 
-- 3. Recuperar todos os dados de autores cujo nome comece com ‘C’.
-- 
-- Retorno:
-- 123;"Carlito";"ca@abc.com";"1981-01-23";120
-- 500;"Charlie";"ca@abc.com";"1981-01-23";120

SELECT *
FROM autor
WHERE nome like 'C%'

-- 
-- 4. Recuperar o nome das cidades, que comece com ‘S’ e que sejam do ‘RS’ ou de ‘SP’.
-- 
-- Retorno:
-- "São Paulo"
-- "Santa Maria"

SELECT *
FROM cidade
WHERE cidade.nome like 'S%' and ( UF = 'SP' OR UF = 'RS' )

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
WHERE precoSugerido > 100 and titulo LIKE 'H%'

-- 
-- 6. Recuperar todos os autores que tenham nascido na década de 80 e cujo nome comece com C.
-- 
-- Retorno:
-- 123;"Carlito";"ca@abc.com";"1981-01-23";120
-- 500;"Charlie";"ca@abc.com";"1981-01-23";120

SELECT *
FROM autor
WHERE dataNasc < '1990/01/01' and dataNasc > '1979/12/31' and nome LIKE 'C%'

-- 
-- 7. Recuperar idiomas existentes para os livros cadastrados. A resposta não pode possuir
-- resultados repetidos. 
-- 
-- Retorno:
-- "Inglês"
-- "Português"

SELECT DISTINCT idioma
FROM livro

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
WHERE precoSugerido > 50

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
WHERE dataNasc > '1970/01/01' and autor.codCid = cidade.codigo

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
	and autoria.codLiv = livro.codigo

--
-- b. Opção 2:join
*/
SELECT autor.nome, livro.titulo
FROM autor JOIN livro ON 

JOIN autoria ON
	autoria.codAut = autor.codigo
	and autoria.codLiv = livro.codigo
WHERE livro.idioma = 'Português'

-- 
-- 11. Selecionar as alterações solicitadas para a as autorias feitas no ano de 2000
-- a. Opção 1:produto cartesiano
-- b. Opção 2:JOIN
-- 
-- Retorno:
-- "sem alterações"
-- "sem alterações"
-- "conferir numeração de páginas"


-- 
-- 12. Selecionar título dos livros, nomes de seus autores, nomes de seus revisores e qual
-- a alteração solicitada.
-- a. Opção 1:produto cartesiano
-- b. Opção 2:JOIN
-- 
-- Retorno:
-- "Maravilhas da terra";"Luiz";"Charlie";"sem alterações"
-- "Espaço Literal";"Luiz";"Charlie";"ver labels das tabelas"
-- "Hello World";"Charlie";"Luana";"arrumar titulos capitulos"
-- "Hi There";"Charlie";"Carlito";"sem alterações"
-- "Hei you!!!";"Charlie";"Luiz";"conferir numeração de páginas"


-- 
-- 13. Selecionar título dos livros e preço sugerido, cujo o nome do tipo contenha a
-- palavra ‘Literatura’.
-- a. Opção 1:produto cartesiano
-- b. Opção 2:JOIN
-- 
-- Retorno:
-- "Hello World";90.00
-- "Hi There";115.00
-- "Hei you!!!";112.00


-- 
-- 14. Selecionar o nome dos autores, nome e UF das cidades. O nome da cidade deve
-- ser igual a ‘Floripa’ ou ‘Marau’.
-- a. Opção 1:
-- b. Opção 2:
-- 
-- Retorno:
-- "Luiz";"Marau";"RS"
-- "Luana";"Floripa";"SC"




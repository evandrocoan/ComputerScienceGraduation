
/*

EXISTS returns on the first match on the subquery.

EXISTS is faster than JOIN because on the first match it returns the result,
the JOIN otherwise, will perform all the table consults.

Use always NOT IN instead of ... JOIN ... ON ...colunm1 <> colunm2,
they are not the same thing. The <> will return all the , or JOINs trashed values.

You should use the primary key on the EXISTS because it is faster than *
and it will always evaluate correctly when there is the requested data on
the table as the primary key will never get a null value.

When do you need to return some result from the most inner aligned subquery,
you cannot to use aligned subqueries, you need to use JOIN, instead of.

*/

/*

cliente (codigo, nome, email, telefone)
funcionario (codigo, nome, email, dtaNasc, salario)
venda (numero, data, hora, codclie#, codFun#, tipo)
   codClie REFERENCES cliente (codigo)
   codFun REFERENCES funcionario (codigo)

produto (codigo, nome, preco, qtdEstoque)
produtoVendido (numero#, codProd#, qtd, valor)
   numero REFERENCES venda (numero),
   codProd REFERENCES produto (codigo)

*/

\echo 'INFO:'
\echo 'INFO: Os exercícios englobam consultas usando todos as possíveis construções'
\echo 'INFO: vistas em aula: aninhamentos com IN, ANY ou ALL, operadores de conjunto'
\echo 'INFO: UNION, INTERSECT ou EXCEPT, uso do EXISTS. '
\echo 'INFO: '
\echo 'INFO: No entanto, sempre que possível, procure usar o EXISTS para exercitar'
\echo 'INFO: a sintaxe e o raciocínio em cima do operador.'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: cliente (codigo, nome, email, telefone)'
\echo 'INFO: funcionario (codigo, nome, email, dtaNasc, salario)'
\echo 'INFO: venda (numero, data, hora, codclie#, codFun#, tipo)'
\echo 'INFO:    codClie REFERENCES cliente (codigo)'
\echo 'INFO:    codFun REFERENCES funcionario (codigo)'
\echo 'INFO: produto (codigo, nome, preco, qtdEstoque)'
\echo 'INFO: produtoVendido (numero#, codProd#, qtd, valor)'
\echo 'INFO:    numero REFERENCES venda (numero),'
\echo 'INFO:    codProd REFERENCES produto (codigo)'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 1. Nome do produto de maior preço de custo, desde que não tenha sido vendido ainda.'
\echo 'INFO: Usando NOT EXISTS'
\echo 'INFO:'

WITH naoVendidos AS
(
    SELECT produto.nome, produto.preco
    FROM produto
    WHERE NOT EXISTS
    (
        SELECT *
        FROM produtoVendido
        WHERE produtoVendido.codProd = produto.codigo
    )
)

SELECT naoVendidos.nome, naoVendidos.preco
FROM naoVendidos
WHERE naoVendidos.preco =
(
    SELECT MAX( naoVendidos.preco )
    FROM naoVendidos
);

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 2. A consulta abaixo retorna uma dupla do banco:'
\echo 'INFO: "Leinaura"'
\echo 'INFO: O que quer dizer este resultado? Explique: '
\echo 'INFO:'

\echo 'INFO: This query return the salesman\'s name which does not made any sell.'

SELECT f.nome
FROM funcionario f
WHERE NOT EXISTS
(
    SELECT *
    FROM cliente c
    WHERE NOT EXISTS
    (
        SELECT *
        FROM venda v
        WHERE v.codclie =c.codigo AND v.codfun = f.codigo
    )
);

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 3. Data e hora das vendas, e nomes dos clientes. As vendas efetuadas para'
\echo 'INFO: clientes sem cadastro também devem ser listadas.'
\echo 'INFO:'

SELECT venda.data, venda.hora, cliente.nome
FROM venda LEFT JOIN cliente ON venda.codclie = cliente.codigo;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 4. Nomes dos produtos que não foram vendidos no período de: início de 2004 até'
\echo 'INFO: final de 2007. Usando NOT EXISTS.'
\echo 'INFO:'

SELECT produto.nome
FROM produto
WHERE NOT EXISTS
(
    SELECT *
    FROM produtoVendido
    WHERE produto.codigo = produtoVendido.codProd AND EXISTS
    (
        SELECT *
        FROM venda
        WHERE venda.data > '2003/12/31' AND venda.data < '2008/01/01'
    )
);

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 5. Nomes dos produtos cujo preço de venda seja inferior ao seu preço de custo.'
\echo 'INFO: USAR EXISTS.'
\echo 'INFO:'

SELECT produto.nome, produto.preco
FROM produto
WHERE EXISTS
(
    SELECT *
    FROM produtoVendido
    WHERE produto.codigo = produtoVendido.codProd AND produto.preco > produtoVendido.valor
);

/*
SELECT *
FROM produto;

SELECT *
FROM produtoVendido;
*/

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 6. Retornar o nome do funcionário que também já foi cliente. Neste caso, uma mesma'
\echo 'INFO: pessoa é identificada pelo nome + e-mail, ou seja, cliente e funcionário que têm'
\echo 'INFO: o mesmo nome e o mesmo e-mail são consideradas a mesma pessoa.'
\echo 'INFO: a. Usando EXISTS'
\echo 'INFO:'

SELECT funcionario.nome
FROM funcionario
WHERE EXISTS
(
    SELECT *
    FROM cliente
    WHERE funcionario.nome = cliente.nome AND funcionario.email = cliente.email
);

/*
SELECT *
FROM cliente;

SELECT *
FROM funcionario;
*/

\echo 'INFO: b. Usando JOIN'
\echo 'INFO:'

SELECT funcionario.nome
FROM funcionario JOIN cliente ON funcionario.nome  = cliente.nome
                             AND funcionario.email = cliente.email;

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 7. Data de venda, nome do produto e o valor total vendido. Só recuperar as vendas'
\echo 'INFO: realizadas no período de: início de 2003 até final de 2004, com o total superior'
\echo 'INFO: a 100. Ordene por data de venda.'
\echo 'INFO:'

SELECT *
FROM
(
    SELECT venda.data, produto.nome, produtoVendido.qtd * produtoVendido.valor AS TotalVendido
    FROM produto JOIN produtoVendido ON produto.codigo = produtoVendido.codProd
                 JOIN venda          ON venda.numero   = produtoVendido.numero
    WHERE venda.data > '2002/12/31' AND venda.data < '2005/01/01'
) resultTable
WHERE resultTable.TotalVendido > 100;

/*
SELECT *
FROM venda;
*/

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 8. Data da venda, nome do funcionário que efetuou a venda e total vendido para o'
\echo 'INFO: cliente \'Monira Rosa\'. Ordene por data de venda.'
\echo 'INFO:'

SELECT venda.data, funcionario.nome, produtoVendido.valor * produtoVendido.qtd AS TotalVendido
FROM venda JOIN funcionario    ON venda.codFun = funcionario.codigo
           JOIN produtoVendido ON venda.numero = produtoVendido.numero
WHERE EXISTS
(
    SELECT *
    FROM cliente
    WHERE cliente.nome = 'Monira Rosa' AND cliente.codigo = venda.codclie
)
ORDER BY venda.data;

/*
SELECT *
FROM cliente JOIN venda          ON cliente.codigo = venda.codclie
             JOIN produtoVendido ON venda.numero   = produtoVendido.numero; 
*/

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 9. Selecionar o nome do funcionário sujo salário é maior do que o funcionário'
\echo 'INFO: mais velho da empresa.'
\echo 'INFO:'

SELECT funcionario.nome, funcionario.salario
FROM funcionario
WHERE EXISTS
(
    SELECT *
    FROM funcionario funcionarioMaisVelhos
    WHERE funcionarioMaisVelhos.dtaNasc =
    (
        SELECT MIN( idadeDoMaisVelhos.dtaNasc )
        FROM funcionario idadeDoMaisVelhos
    ) AND funcionarioMaisVelhos.salario < funcionario.salario
);





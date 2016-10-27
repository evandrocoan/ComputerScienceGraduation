
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

SELECT naoVendidos.nome, naoVendidos.preco
FROM
(
    SELECT produto.nome, produto.preco
    FROM produto
    WHERE NOT EXISTS
    (
        SELECT *
        FROM produtoVendido
        WHERE produtoVendido.codProd = produto.codigo
    )
) naoVendidos
WHERE naoVendidos.preco =
(
    SELECT MAX( naoVendidos.preco )
    FROM
    (
        SELECT produto.nome, produto.preco
        FROM produto
        WHERE NOT EXISTS
        (
            SELECT *
            FROM produtoVendido
            WHERE produtoVendido.codProd = produto.codigo
        )
    ) naoVendidos
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




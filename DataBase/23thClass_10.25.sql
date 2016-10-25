
/*
The the first table column's will name the UNION/INTERSECTION/EXCEPT a.k.a.
Union, Intersection and minus. The tables must to be compatible, as one table
is put down the other, instead of the non set operators as JOIN.

These operators always eliminates the duplicate lines. To keep them, use the
ALL operator as: UNION ALL/INTERSECTION ALL/EXCEPT ALL.

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

\echo 'INFO: cliente (codigo, nome, email, telefone)'
\echo 'INFO: funcionario (codigo, nome, email, dtaNasc, salario)'
\echo 'INFO: venda (numero, data, hora, codclie#, codFun#, tipo)'
\echo 'INFO:    codClie REFERENCES cliente (codigo)'
\echo 'INFO:    codFun REFERENCES funcionario (codigo)'
\echo 'INFO:'
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
\echo 'INFO: 1. Retornar uma tabela com as colunas nomes e e-mails,'
\echo 'INFO: de clientes e funcionários. Deve-se criar uma terceira coluna que'
\echo 'INFO: indique se ele é ‘cliente’ ou ‘funcionário’. Os funcionários devem'
\echo 'INFO: ser apenas os vendedores - funcionário que aparece na tabela venda'
\echo 'INFO: é um vendedor.'
\echo 'INFO:'

(
    (
      SELECT cliente.nome, cliente.email, 'Cliente' AS Tipo
      FROM cliente
    )
    UNION
    (
      SELECT funcionario.nome, funcionario.email, 'Funcionario' AS Tipo
      FROM funcionario JOIN venda ON funcionario.codigo = venda.codFun
    )
)
ORDER BY 3 ASC, 1 DESC;

/*
SELECT *
FROM (
        (
          SELECT cliente.nome, cliente.email, 'Cliente' AS Tipo
          FROM cliente
        )
        UNION
        (
          SELECT funcionario.nome, funcionario.email, 'Funcionario' AS Tipo
          FROM funcionario JOIN venda ON funcionario.codigo = venda.codFun
        )
     ) resultado
ORDER BY 3;
*/

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 2. Código e nome dos funcionários e clientes que nunca apareceram'
\echo 'INFO: em uma venda.'
\echo 'INFO:'

(
    (
        SELECT funcionario.codigo, funcionario.nome
        FROM funcionario
    )
    UNION
    (
        SELECT cliente.codigo, cliente.nome
        FROM cliente
    )
)
EXCEPT
(
    (
        SELECT funcionario.codigo, funcionario.nome
        FROM funcionario JOIN venda ON funcionario.codigo = venda.codFun
    )
    UNION
    (
        SELECT cliente.codigo, cliente.nome
        FROM cliente JOIN venda ON cliente.codigo = venda.codclie
    )
);

/*
SELECT *
FROM
(
        SELECT *
        FROM
        (
            (
                SELECT funcionario.codigo, funcionario.nome
                FROM funcionario
            )
            UNION
            (
                SELECT cliente.codigo, cliente.nome
                FROM cliente
            )
        ) todoMundo
        
    EXCEPT
        
        SELECT *
        FROM
        (
            (
                SELECT funcionario.codigo, funcionario.nome
                FROM funcionario JOIN venda ON funcionario.codigo = venda.codFun
            )
            UNION
            (
                SELECT cliente.codigo, cliente.nome
                FROM cliente JOIN venda ON cliente.codigo = venda.codclie
            )
        
        ) todasVendas

) resultado
*/

\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO:'
\echo 'INFO: 3. Nome e email dos clientes que realizaram compras do tipo'
\echo 'INFO: "A VISTA". Usar pelo menos uma vez um operador de conjunto.'
\echo 'INFO:'

(
    SELECT cliente.nome, cliente.email
    FROM cliente
)
EXCEPT
(
    SELECT cliente.nome, cliente.email
    FROM cliente JOIN venda ON cliente.codigo = venda.codclie
    WHERE venda.tipo <> 'A VISTA'
);


SELECT cliente.nome, cliente.email, venda.tipo
FROM cliente JOIN venda ON cliente.codigo = venda.codclie
WHERE venda.tipo = 'A VISTA'


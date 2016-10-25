CREATE TABLE cliente
(codigo INTEGER NOT NULL, 
nome VARCHAR (40), 
email VARCHAR (40) NOT NULL,
telefone varchar(15),
PRIMARY KEY(codigo));

CREATE TABLE funcionario 
(codigo INTEGER NOT NULL, 
nome VARCHAR (40), 
email VARCHAR (40) NOT NULL,
dtaNasc date,
salario numeric(18,3),
PRIMARY KEY(codigo));

CREATE TABLE venda 
(numero INTEGER NOT NULL, 
data DATE NOT NULL, 
hora TIME NOT NULL, 
codClie INTEGER, 
codFun INTEGER NOT NULL,
tipo varchar(10),
PRIMARY KEY(numero),
FOREIGN KEY(codClie) REFERENCES cliente (codigo)
   ON DELETE SET NULL
   ON UPDATE CASCADE,
FOREIGN KEY(codFun) REFERENCES funcionario (codigo)
   ON UPDATE CASCADE
);
CREATE TABLE produto 
(codigo INTEGER NOT NULL, 
nome VARCHAR (40), 
preco numeric (18,4),
qtdEstoque integer,
PRIMARY KEY(codigo));

CREATE TABLE produtoVendido 
(numero INTEGER NOT NULL, 
codProd INTEGER NOT NULL, 
qtd INTEGER, 
valor NUMERIC (18,2),
PRIMARY KEY(numero, codProd),
FOREIGN KEY(numero) REFERENCES venda (numero),
FOREIGN KEY(codProd) REFERENCES produto (codigo)
   ON DELETE CASCADE
   ON UPDATE CASCADE);

INSERT INTO cliente (codigo, nome, email, telefone) VALUES (1, 'Juliana Paula', 'juju@abc.de.br', '51.3131.0000');
INSERT INTO cliente (codigo, nome, email) VALUES (2, 'Paulinha Vera', 'pv@abc.de.br');
INSERT INTO cliente (codigo, nome, email, telefone) VALUES (3, 'Claudia Regina', 'cr@abc.de.br', '55.3131.0000');
INSERT INTO cliente (codigo, nome, email) VALUES (4, 'Marco Antonio', 'mat@abc.de.br');
INSERT INTO cliente (codigo, nome, email, telefone) VALUES (5, 'Claudio Rogério', 'crog@abc.de.br', '51.3131.0000');
INSERT INTO cliente (codigo, nome, email) VALUES (6, 'Flavio Augusto', 'flau@abc.de.br');
INSERT INTO cliente (codigo, nome, email, telefone) VALUES (7, 'Monira Rosa', 'moro@abc.de.br', '54.0101.9090');
INSERT INTO cliente (codigo, nome, email) VALUES (8, 'Paulo Marcos', 'pama@abc.de.br');
INSERT INTO cliente (codigo, nome, email, telefone) VALUES (9, 'Linda Regina', 'lire@abc.de.br', '54.0909.9898');
INSERT INTO cliente (codigo, nome, email, telefone) VALUES (10, 'Tamires Ana', 'tana@abc.de.br', '54.3131.0000');
INSERT INTO cliente (codigo, nome, email) VALUES (11, 'Luciano Paulo', 'lupa@abc.de.br');
INSERT INTO cliente (codigo, nome, email) VALUES (12, 'Tatiana Clara', 'tacl@abc.de.br');
INSERT INTO cliente (codigo, nome, email) VALUES (13, 'Robercleberson', 'rob@empresa.com');
INSERT INTO cliente (codigo, nome, email) VALUES (16, 'Julili', 'juli@abc.de.br');

INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (1, 'Claudison', 'clau@empresa.com', '1969-10-10', 1000.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (2, 'Leinaura', 'lei@empresa.com', '1975-10-10', 750.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (3, 'Lobervau', 'lob@empresa.com', '1971-11-10', 350.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (4, 'Norlindson', 'lind@empresa.com', '1974-05-10', 400.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (5, 'Berlindo', 'be@empresa.com', '1978-06-17', 300.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (6, 'fluvialson', 'flu@empresa.com', '1980-03-01', 470.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (7, 'Robercleberson', 'rob@empresa.com', '1981-04-23', 1500.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (8, 'Aristideles', 'ari@empresa.com', '1977-04-28', 1240.00);
INSERT INTO funcionario (codigo, nome, email, dtaNasc, salario) VALUES (9, 'Jucemar', 'juce@empresa.com', '1978-04-28', 1550.00);

INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (123, '2000-10-01', '14:01', 1, 2, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (124, '2000-09-01', '14:01', 3, 2);
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (143, '2001-08-01', '16:04', 5, 2, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (125, '2001-07-01', '18:02', 7, 2);
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (126, '2001-06-01', '13:26', 9, 2);
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (127, '2006-05-01', '16:21', 10, 2);
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (128, '2006-04-01', '14:31', 1, 2, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (129, '2006-07-01', '18:02', 7, 5);
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (130, '2006-10-10', '16:26', 9, 5, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (131, '2006-05-03', '15:21', 10, 3, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (132, '2006-04-06', '15:31', 1, 3, 'A PRAZO');
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (133, '2004-04-06', '15:31', 1, 3);
INSERT INTO venda (numero, data, hora, codClie, codFun) VALUES (134, '2004-04-06', '15:31', 2, 3);
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (135, '2002-04-06', '15:31', 1, 3, 'A PRAZO');


INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (300, '2005-07-06', '13:01', null, 3, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (301, '2005-07-06', '15:31', null, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (305, '2005-07-06', '17:33', null, 1, 'A VISTA');

INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (500, '2006-07-06', '16:31', null, 8, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (501, '2006-04-06', '14:07', null, 7, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (502, '2006-04-06', '19:31', null, 6, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (503, '2006-04-06', '17:01', null, 6, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (504, '2007-04-06', '18:31', null, 6, 'A VISTA');

INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (700, '2007-04-06', '18:31', 2, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (701, '2007-04-06', '18:31', 4, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (702, '2007-04-06', '18:31', 6, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (703, '2007-04-06', '18:31', 8, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (704, '2007-04-06', '18:31', 11, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (705, '2007-04-06', '18:31', 12, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (706, '2007-04-06', '18:31', 13, 2, 'A VISTA');
INSERT INTO venda (numero, data, hora, codClie, codFun, tipo) VALUES (707, '2007-04-06', '18:31', 16, 2, 'A VISTA');


INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (1, 'Camisa', 90.00, 10000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (2, 'Meia', 6.00, 5000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (3, 'Gravata', 12.00, 7000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (4, 'Camiseta Branca', 25.00, 12000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (5, 'Jaqueta', 120.00, 4000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (6, 'Cinto', 50.00, 13000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (7, 'Carteira', 35.00, 14000);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (8, 'Bolsa', 89.00, 14500);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (9, 'Boné', 15.00, 9500);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (10, 'Meia Calça', 12.00, 18000);

INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (12, 'Chapéu', 22.00, 180);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (13, 'Carteira de couro', 120.00, 500);
INSERT INTO produto (codigo, nome, preco, qtdEstoque) VALUES (14, 'Brinco Argola', 23.50, 58);


INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,1, 1, 80.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,3, 2, 38.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,4, 2, 25.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,6, 1, 34.50);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,9, 1, 15.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (123,10, 3, 7.00);

INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (124,5,1, 48.90);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (124,8,1, 37.75);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (143, 9, 1, 16.50);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (125, 6, 1, 35.77);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (126, 7, 1, 10.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (127, 6, 2, 23.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (127, 10, 1, 7.50);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (128, 2, 2, 5.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (128, 9, 1, 10.00);

INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (129, 1, 1, 129.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (130, 3, 2, 23.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (130, 5, 1, 234.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (131, 2, 1, 2.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (131, 7, 1, 36.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (132, 8, 1, 123.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (132, 10, 3, 9.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (132, 4, 1, 23.00);

INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (133,3, 1, 13.00); 
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (133,4, 1, 23.00); 
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (133,5, 1, 130.00); 

INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (134,5, 1, 134.00);

INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (135,7, 1, 36.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (135,9, 1, 16.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (135,10, 1, 6.00);
INSERT INTO produtoVendido (numero, codProd, qtd, valor) VALUES (135,2, 6, 3.00);

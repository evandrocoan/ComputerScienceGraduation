INSERT INTO usuarioBanco VALUES (100, 'Anibal');
INSERT INTO usuarioBanco VALUES (110, 'Luc');
INSERT INTO usuarioBanco VALUES (120, 'Luciana');
INSERT INTO usuarioBanco VALUES (130, 'Laline');
INSERT INTO usuarioBanco VALUES (140, 'Edgar');


INSERT INTO  Tipo VALUES (100, 'Literatura Contemporânea');
INSERT INTO  Tipo VALUES (120, 'Literatura Americana');
INSERT INTO  Tipo VALUES (150, 'Literatura Européia');
INSERT INTO  Tipo VALUES (170, 'Pop literal');
INSERT INTO  Tipo VALUES (190, 'Ficção');


INSERT INTO  Cidade VALUES (100, 'Marau', 'RS');
INSERT INTO  Cidade VALUES (101, 'Floripa', 'SC');
INSERT INTO  Cidade VALUES (110, 'São Paulo', 'SP');
INSERT INTO  Cidade VALUES (120, 'Niterói', 'RJ');
INSERT INTO  Cidade VALUES (130, 'Maringá', 'PR');
INSERT INTO  Cidade VALUES (133, 'Curitiba', 'PR');
INSERT INTO  Cidade VALUES (135, 'Santa Maria', 'RS');

INSERT INTO Editora VALUES (100, 'Editora Linda', 'Rua Ortências', 100);
INSERT INTO Editora VALUES (120, 'Edt Beuati', 'Av. das Rosas', 120);
INSERT INTO Editora VALUES (140, 'Ed. Azul', 'Rua Caligari', 130);
INSERT INTO Editora VALUES (150, 'Lindios Livrios', 'Rua Luiz XI', 133);
INSERT INTO Editora VALUES (170, 'Larari Books', 'Av. Maravilha', 135);

INSERT INTO Autor VALUES (122, 'Luiz', 'lu@abc.com', '1970-01-04', 100);
INSERT INTO Autor VALUES (144, 'Luana', 'lua@abc.com', '1979-11-07', 101);
INSERT INTO Autor VALUES (152, 'Ana Paula', 'ana@abc.com', '1980-11-04', 110);
INSERT INTO Autor VALUES (125, 'Leila', 'lei@abc.com', '1979-10-14', 120);
INSERT INTO Autor VALUES (123, 'Carlito', 'ca@abc.com', '1981-01-23', 120);
INSERT INTO Autor VALUES (500, 'Charlie', 'ca@abc.com', '1981-01-23', 120);



INSERT INTO Livro VALUES (300, 'Maravilhas da terra', 'Português', 170, 100, 100);
INSERT INTO Livro VALUES (340, 'Espaço Literal', 'Português', 190, 150, 70.00);
INSERT INTO Livro VALUES (400, 'Hello World', 'Inglês', 120, 120, 90.00);
INSERT INTO Livro VALUES (500, 'Hi There', 'Inglês', 120, 120, 115.00);
INSERT INTO Livro VALUES (600, 'Hei you!!!', 'Inglês', 120, 120, 112.00);


INSERT INTO  Autoria VALUES (122, 300,'2000-01-23');
INSERT INTO  Autoria VALUES (122, 340,'2001-04-23');
INSERT INTO  Autoria VALUES (500, 400,'2002-05-13');
INSERT INTO  Autoria VALUES (500, 500,'2000-01-03');
INSERT INTO  Autoria VALUES (500, 600,'2000-10-26');

INSERT INTO  revisao VALUES (122, 300, 500, '1999-11-23', 'sem alterações', NULL );
INSERT INTO  revisao VALUES (122, 340, 500, '2001-01-03', 'ver labels das tabelas', NULL);
INSERT INTO  revisao VALUES (500, 400, 144, '2002-02-24', 'arrumar titulos capitulos', 'reenviar para nova revisão');
INSERT INTO  revisao VALUES (500, 500, 123, '1999-11-01', 'sem alterações', 'ótimo livro');
INSERT INTO  revisao VALUES (500, 600, 122, '2000-02-08', 'conferir numeração de páginas', 'enviar para editor');

CREATE TABLE usuarioBanco 
(
codigo INTEGER NOT NULL, 
nome VARCHAR(40),
PRIMARY KEY(codigo)
);

CREATE TABLE Tipo 
(
codigo INTEGER NOT NULL, 
nome VARCHAR(40),
PRIMARY KEY(codigo)
);

CREATE TABLE Cidade 
(
codigo INTEGER NOT NULL, 
nome VARCHAR(40),
UF CHAR (2),
PRIMARY KEY(codigo)
);

CREATE TABLE Editora 
(
codigo INTEGER NOT NULL, 
nome VARCHAR(40),
endereco VARCHAR(40),
codCid INTEGER,
PRIMARY KEY(codigo),
FOREIGN KEY (codCid) REFERENCES cidade(codigo)
);


CREATE TABLE Autor 
(
codigo INTEGER NOT NULL, 
nome VARCHAR(40),
email VARCHAR(15),
dataNasc DATE,
codCid INTEGER,
PRIMARY KEY(codigo),
FOREIGN KEY (codCid) REFERENCES cidade(codigo)
);


CREATE TABLE Livro
(
codigo INTEGER NOT NULL, 
titulo VARCHAR(40),
idioma VARCHAR(20),
codTip INTEGER,
codEdi INTEGER,
precoSugerido numeric(14,2),
PRIMARY KEY(codigo),
FOREIGN KEY (codTip) REFERENCES tipo(codigo),
FOREIGN KEY (codEdi) REFERENCES editora(codigo)
);

CREATE TABLE Autoria 
(
codAut INTEGER NOT NULL,
codLiv INTEGER NOT NULL,
data date,
PRIMARY KEY(codAut, codLiv),
FOREIGN KEY (codAut) REFERENCES autor (codigo),
FOREIGN KEY (codLiv) REFERENCES livro (codigo)
);

CREATE TABLE revisao 
(codAut INTEGER NOT NULL, 
codLiv INTEGER NOT NULL, 
codRev INTEGER NOT NULL, 
data DATE, 
alteracao VARCHAR, 
obs VARCHAR,
CONSTRAINT PK_REV PRIMARY KEY(codAut, codLiv, codRev),
constraint fk_rev FOREIGN KEY(codAut, codLiv) REFERENCES AUTORIA (codAut, codLiv),
constraint fk_revISOR FOREIGN KEY(codRev) REFERENCES AUTOR (codigo)
);
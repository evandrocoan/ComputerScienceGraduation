
drop table if exists jogo;
drop table if exists equipe;
drop table if exists federacao;
drop table if exists cidade;
drop table if exists pais;

BEGIN TRANSACTION;

CREATE TABLE pais (
codigo integer not null,
nome varchar (100),
populacao integer,
CONSTRAINT pk_pais PRIMARY KEY (codigo)
);

CREATE TABLE cidade (
codigo integer not null,
nome varchar (100),
UF char (2),
regiao varchar (40),
codPais integer,
CONSTRAINT pk_cidade PRIMARY KEY (codigo),
CONSTRAINT fk_pais_cidade FOREIGN KEY (codPais) REFERENCES pais (codigo)
);

CREATE TABLE federacao (
codigo integer not null,
nome varchar (100) UNIQUE,
codPais integer,
CONSTRAINT pk_federacao PRIMARY KEY (codigo),
CONSTRAINT fk_pais_federacao FOREIGN KEY (codPais) REFERENCES pais (codigo)
);

CREATE TABLE equipe (
codigo integer not null,
nome varchar (100),
codFederacao integer,
codCidade integer,
CONSTRAINT pk_equipe PRIMARY KEY (codigo),
CONSTRAINT fk_federacao_equipe FOREIGN KEY (codFederacao) REFERENCES federacao (codigo),
CONSTRAINT fk_cidade_equipe FOREIGN KEY (codCidade) REFERENCES cidade (codigo)
);

CREATE TABLE jogo (
codTimeA integer not null,
codTimeB integer not null,
dataJogo date not null,
codCidade integer,
vencedor varchar (20) check (vencedor = 'Mandante' OR vencedor = 'Visitante' OR vencedor = 'Empate' OR vencedor = 'Sem resultado') DEFAULT 'Sem resultado',
CONSTRAINT pk_jogo PRIMARY KEY (codTimeA, codTimeB, dataJogo),
CONSTRAINT fk_equipeA_jogo FOREIGN KEY (codTimeA) REFERENCES equipe (codigo),
CONSTRAINT fk_equipeB_jogo FOREIGN KEY (codTimeB) REFERENCES equipe (codigo),
CONSTRAINT fk_cidade_jogo FOREIGN KEY (codCidade) REFERENCES cidade (codigo)
);

COMMIT TRANSACTION;

BEGIN TRANSACTION;

INSERT INTO pais (codigo, nome, populacao) VALUES (1, 'Brasil', 200);
INSERT INTO pais (codigo, nome, populacao) VALUES (2, 'Argentina', 41);

INSERT INTO cidade (codigo, nome, UF, regiao, codPais) VALUES (1, 'Florianópolis', 'SC', 'Sul', 1);
INSERT INTO cidade (codigo, nome, UF, regiao, codPais) VALUES (2, 'Porto Alegre', 'RS', 'Sul', 1);
INSERT INTO cidade (codigo, nome, UF, regiao, codPais) VALUES (3, 'Belo Horizonte', 'MG', 'Sudeste', 1);
INSERT INTO cidade (codigo, nome, UF, regiao, codPais) VALUES (4, 'São Paulo', 'SC', 'Sudeste', 1);
INSERT INTO cidade (codigo, nome, codPais) VALUES (5, 'Buenos Aires', 2);
INSERT INTO cidade (codigo, nome, codPais) VALUES (6, 'Rio de Janeiro', 1);

INSERT INTO federacao (codigo, nome, codPais) VALUES (1, 'CBF', 1);
INSERT INTO federacao (codigo, nome, codPais) VALUES (2, 'AFA', 2);

INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (1, 'Avai', 1, 1);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (2, 'Figueirense', 1, 1);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (3, 'Gremio', 1, 2);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (4, 'Corinthians', 1, 4);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (5, 'Atletico', 1, 3);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (6, 'Boca Juniors', 2, 5);

INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (1, 2, current_date, 1, 'Mandante');
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (5, 3, '2016-11-23'::date, 3, 'Empate');
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (3, 5, '2016-11-30'::date, 2, 'Mandante');
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (2, 4, '2016-11-16'::date, 2, 'Visitante');
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (6, 4, '2012-06-27'::date, 5, 'Empate');
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade, vencedor) VALUES (4, 6, '2012-07-04'::date, 4, 'Mandante');

COMMIT TRANSACTION;

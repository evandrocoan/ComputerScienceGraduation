/*

To install postgree: apt-get install postgresql pgadmin3

Follow the following post-installation steps to create your database:

[root@host]# su - postgres
Password:
bash-4.1$ createdb testdb
bash-4.1$ psql testdb
psql (8.4.13, server 9.2.4)

test=#


You can start/restart postgres server in case it is not running using the following command:

[root@host]# service postgresql restart
Stopping postgresql service:                               [  OK  ]
Starting postgresql service:                               [  OK  ]

https://www.tutorialspoint.com/postgresql/postgresql_environment.htm



Up this:

Modify password for role postgres:

sudo -u postgres psql postgres

alter user postgres with password 'postgres';
Now connect to pgadmin using username postgres and password postgres

Now you can create roles & databases using pgAdmin


Name:              localhost
Host:              localhost
Port:              5432
Service:           empty
Maintenance DB:    postgres
Username:          postgres
Password:          postgres

http://stackoverflow.com/questions/24917832/how-connect-postgres-to-localhost-server-using-pgadmin-on-ubuntu

*/

-- Use rollback to undo the current operation over the database.
-- Use commit to send your changes to the database.
BEGIN TRANSACTION;


-- Using CASCADE to drop all depend tables.
DROP TABLE pais CASCADE;

CREATE TABLE pais
(
    codigo    integer NOT NULL, -- PRIMARY KEY, -- Second method of creation
    nome      varchar (40),
    populacao integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_medico PRIMARY KEY(codigo)
);

/*
    CRM     integer,
    codCid  integer,
    FOREIGN KEY(codCid) REFERENCES cidade(codigo),

    -- Permite exclusão automática
    -- de registros filhos quando o
    -- registro pai for excluído
    CONSTRAINT fk_cidade_medico FOREIGN KEY (codCid)
            REFERENCES cidade(codigo) ON DELETE CASCADE,
*/



-- This cannot be dropped, as depends:
DROP TABLE IF EXISTS cidade CASCADE;
-- 
-- ERROR:  cannot drop table cidade because other objects depend on it
-- DETAIL:  constraint fk_cidade_pais on table federacao depends on table cidade
-- HINT:  Use DROP ... CASCADE to drop the dependent objects too.

CREATE TABLE cidade
(
    codigo  integer NOT NULL, -- PRIMARY KEY, -- Second method of creation
    nome    varchar (40),
    UF      varchar (40),
    regiao  varchar (40),
    codPais integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_cidade PRIMARY KEY(codigo),

    -- Permite exclusão automática
    -- de registros filhos quando o
    -- registro pai for excluído
    CONSTRAINT fk_cidade_pais FOREIGN KEY (codPais)
            REFERENCES cidade(codigo) ON DELETE CASCADE
);



-- This cannot be dropped, as depends:
DROP TABLE IF EXISTS federacao CASCADE;

-- 
-- ERROR:  cannot drop table cidade because other objects depend on it
-- DETAIL: constraint fk_cidade_pais on table federacao depends on table cidade
-- HINT:   Use DROP ... CASCADE to drop the dependent objects too.

CREATE TABLE federacao
(
    codigo  integer NOT NULL, -- PRIMARY KEY, -- Second method of creation
    nome    varchar (40),
    codPais integer,

    -- CONSTRAINT + nome_da_constraint
    CONSTRAINT pk_federacao PRIMARY KEY(codigo),

    -- Permite exclusão automática
    -- de registros filhos quando o
    -- registro pai for excluído
    CONSTRAINT fk_federacao_pais FOREIGN KEY (codPais)
            REFERENCES pais(codigo) ON DELETE CASCADE
);

-- Undo all operations, change this comments to effectevly implement the changes into the data base.
-- Or comment everything and also the `BEGIN TRANSACTION;` at this script beginning.

ROLLBACK TRANSACTION;
-- COMMIT TRANSACTION;




--Questão 1
BEGIN TRANSACTION;

/*
CREATE TABLE pais (
codigo integer not null primary key,
nome varchar (100),
populacao integer
);
*/

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
dataJogo date not null CHECK (dataJogo >= current_date),
codCidade integer,
CONSTRAINT pk_jogo PRIMARY KEY (codTimeA, codTimeB, dataJogo),
CONSTRAINT fk_equipeA_jogo FOREIGN KEY (codTimeA) REFERENCES equipe (codigo) ON DELETE CASCADE,
CONSTRAINT fk_equipeB_jogo FOREIGN KEY (codTimeB) REFERENCES equipe (codigo) ON DELETE CASCADE,
CONSTRAINT fk_cidade_jogo FOREIGN KEY (codCidade) REFERENCES cidade (codigo)
);

CREATE TABLE resultado (
codTimeA integer not null,
codTimeB integer not null,
dataJogo date not null,
vencedor varchar (20) check (vencedor = 'Mandante' OR vencedor = 'Visitante' OR vencedor = 'Empate' OR vencedor = 'Sem resultado') DEFAULT 'Sem resultado',
CONSTRAINT pk_resultado PRIMARY KEY (codTimeA, codTimeB, dataJogo),
CONSTRAINT fk_jogo_resultado FOREIGN KEY (codTimeA, codTimeB, dataJogo) REFERENCES jogo (codTimeA, codTimeB, dataJogo)
);

-- ROLLBACK TRANSACTION;
COMMIT TRANSACTION;

--Questão 2
BEGIN TRANSACTION;

ALTER TABLE jogo
ADD vencedor varchar (20) check (vencedor = 'Mandante' OR vencedor = 'Visitante' OR vencedor = 'Empate' OR vencedor = 'Sem resultado') DEFAULT 'Sem resultado';

DROP TABLE resultado;

ALTER TABLE cidade
ALTER UF SET DEFAULT '-',
DROP regiao,
ADD regiao_nacional varchar (40);

-- ROLLBACK TRANSACTION;
COMMIT TRANSACTION;

--Questão 3
BEGIN TRANSACTION;
INSERT INTO pais (codigo, nome, populacao) VALUES (1, 'Brasil', 200);
INSERT INTO pais (codigo, nome, populacao) VALUES (2, 'Argentina', 41);

INSERT INTO cidade (codigo, nome, UF, regiao_nacional, codPais) VALUES (1, 'Florianópolis', 'SC', 'Sul', 1);
INSERT INTO cidade (codigo, nome, UF, regiao_nacional, codPais) VALUES (2, 'Porto Alegre', 'RS', 'Sul', 1);
INSERT INTO cidade (codigo, nome, codPais) VALUES (3, 'Buenos Aires', 2);

INSERT INTO federacao (codigo, nome, codPais) VALUES (1, 'CBF', 1);

INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (1, 'Avai', 1, 1);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (2, 'Figueirense', 1, 1);
INSERT INTO equipe (codigo, nome, codFederacao, codCidade) VALUES (3, 'Gremio', 1, 2);
INSERT INTO equipe (codigo, nome, codCidade) VALUES (4, 'Boca Juniors', 3);

INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade) VALUES (1, 2, current_date, 1);
INSERT INTO jogo (codTimeA, codTimeB, dataJogo, codCidade) VALUES (3, 4, current_date + interval '1' day, 2);

--Se tabela resultado ainda existisse
/*
INSERT INTO resultado (codTimeA, codTimeB, dataJogo, vencedor) VALUES (1, 2, current_date, 'Mandante');
INSERT INTO resultado (codTimeA, codTimeB, dataJogo) VALUES (3, 4, current_date + interval '1' day);
*/

UPDATE jogo
SET vencedor = 'Mandante'
WHERE codTimeA = 1 AND codTimeB = 2 and dataJogo = current_date;

UPDATE jogo
SET vencedor = 'Sem resultado' --valor default
WHERE codTimeA = 1 AND codTimeB = 2 and dataJogo = current_date + interval '1' day;

--ROLLBACK TRANSACTION;
COMMIT TRANSACTION;

--Questão 4
BEGIN TRANSACTION;

UPDATE equipe
SET codCidade = 2
WHERE codigo = 4;

DELETE FROM jogo
WHERE codTimeA = 1;
DELETE FROM equipe
WHERE codigo = 1;

DELETE FROM equipe
WHERE codigo = 1;

--ROLLBACK TRANSACTION;
COMMIT TRANSACTION;




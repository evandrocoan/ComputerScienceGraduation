/*

1. Cite duas restrições que podem ser utilizadas no esquema lógico acima:

De domínio: The pais(name) IS NOT NULL, and its maximum size is 100.
nome varchar(100) NOT NULL

De PK: NOT NULL + UNIQUE for the pais(codigo)
CONSTRAINT pk_pais PRIMARY KEY(codigo)

De Null: equipe(nome) and jogo(vencedor) cannot be set to NULL.


2. Crie uma restrição (utilizando CONSTRAINT):
De valor único:
CREATE TABLE pais
(
    ...
    nome varchar UNIQUE
);

De valor possível:
CREATE TABLE pais
(
    ...
    nome integer,
    ...
    CONSTRAINT check_name CHECK(nome == 'Brazil' OR nome == 'EUA')
);

De entidade (PK composta):
CREATE TABLE pais
(
    ...
    codigo integer,
    nome varchar,
    ...
    CONSTRAINT pk_pais PRIMARY KEY(codigo,nome)
);

De integridade referencial (FK composta):
CREATE TABLE equipe
(
    ...
    codFederacao integer,
    codCidade integer,
    ...
    CONSTRAINT fk_cidade_federacao FOREIGN KEY(codFederacao,codCidade)
            REFERENCES federacao(codigo,codCidade)
);


3. Em relação às restrições de integridade

Caso um país seja deletado,
º Se seu valor estiver sendo referenciado por uma cidade, o SGBD
deve impedir a sua deleção:
CREATE TABLE pais
(
    codigo    integer NOT NULL,
    nome      varchar (40),
    populacao integer,

    CONSTRAINT pk_medico PRIMARY KEY(codigo)
);

CREATE TABLE cidade
(
    codigo  integer NOT NULL,
    nome    varchar (40),
    UF      varchar (40),
    regiao  varchar (40),
    codPais integer,

    CONSTRAINT pk_cidade PRIMARY KEY(codigo),
    CONSTRAINT fk_cidade_pais FOREIGN KEY (codPais) REFERENCES cidade(codigo)
);


Caso uma equipe seja deletada,
º Se a equipe for mandante de um jogo, o seu código no jogo deverá
ser setado para NULL no jogo,
º Se a equipe for visitante, ela poderá ser deletada:
CREATE TABLE equipe
(
    codigo  integer NOT NULL,
    nome    varchar (40),
    codFederacao integer,
    codCidade integer,

    CONSTRAINT pk_equipe PRIMARY KEY(codigo),
    CONSTRAINT fk_equipe_federacao FOREIGN KEY(codFederacao)
            REFERENCES federacao(codigo),
    CONSTRAINT fk_equipe_cidade FOREIGN KEY(codCidade)
            REFERENCES cidade(codigo)
);

CREATE TABLE jogo
(
    codEquipeA integer,
    codEquipeB integer,
    dataJogo   date,
    codCidade  integer,
    vencedor   varchar (40),

    CONSTRAINT pk_equipe PRIMARY KEY(codEquipeA,codEquipeB,dataJogo),
    CONSTRAINT fk_jogo_cidade FOREIGN KEY(codCidade)
            REFERENCES cidade(codigo),

    CONSTRAINT fk_jogo_equipeA FOREIGN KEY(codEquipeA)
            REFERENCES equipe(codigo) ON DELETE SET NULL,
    CONSTRAINT fk_jogo_equipeB FOREIGN KEY(codEquipeB)
            REFERENCES equipe(codigo) ON DELETE CASCADE
);


Caso o código de uma federação seja alterado,
º Se houver uma equipe na federação, a alteração no código deverá ser propagada:
CREATE TABLE federacao
(
    codigo  integer NOT NULL,
    nome    varchar (40),
    codPais integer,

    CONSTRAINT pk_federacao PRIMARY KEY(codigo),
    CONSTRAINT fk_federacao_pais FOREIGN KEY (codPais)
            REFERENCES pais(codigo) ON DELETE CASCADE
);

CREATE TABLE equipe
(
    codigo  integer NOT NULL,
    nome    varchar (40),
    codFederacao integer,
    codCidade integer,

    CONSTRAINT pk_equipe PRIMARY KEY(codigo),
    CONSTRAINT fk_equipe_federacao FOREIGN KEY(codFederacao)
            REFERENCES federacao(codigo) ON UPDATE CASCADE,
    CONSTRAINT fk_equipe_cidade FOREIGN KEY(codCidade)
            REFERENCES cidade(codigo)
);



-----------------------------------------------------------
17thClass_10.03.sql

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
-- DETAIL:  constraint fk_cidade_pais on table federacao depends on table cidade
-- HINT:  Use DROP ... CASCADE to drop the dependent objects too.

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

CREATE TABLE equipe
(
    codigo  integer NOT NULL,
    nome    varchar (40),
    codFederacao integer,
    codCidade integer,

    CONSTRAINT pk_equipe PRIMARY KEY(codigo),
    CONSTRAINT fk_equipe_federacao FOREIGN KEY(codFederacao)
            REFERENCES federacao(codigo) ON DELETE CASCADE,
    CONSTRAINT fk_equipe_cidade FOREIGN KEY(codCidade)
            REFERENCES cidade(codigo) ON DELETE CASCADE
);

-- Undo all operations, change this comments to effectevly implement the changes into the data base.
-- Or comment everything and also the `BEGIN TRANSACTION;` at this script beginning.
ROLLBACK TRANSACTION;
-- COMMIT TRANSACTION;

















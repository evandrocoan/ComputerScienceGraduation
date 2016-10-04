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

-- Undo all operations, change this comments to effectevly implement the changes into the data base.
-- Or comment everything and also the `BEGIN TRANSACTION;` at this script beginning.
ROLLBACK TRANSACTION;
-- COMMIT TRANSACTION;

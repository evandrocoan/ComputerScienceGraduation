CREATE TABLE profissao
(
	codigo INTEGER NOT NULL,
	area VARCHAR(20) NOT NULL,
	nome VARCHAR(20) NOT NULL,
	PRIMARY KEY (codigo)
);

create table convenio
(
codigo integer not null,
nome varchar(40),
primary key(codigo)
);

CREATE TABLE cidade
(
	codigo INTEGER NOT NULL,
	nome VARCHAR(20) NOT NULL,
	UF CHAR(2) NOT NULL,
	PRIMARY KEY (codigo)
);

CREATE TABLE paciente
(
	codigo INTEGER NOT NULL,
	nome VARCHAR(20) NOT NULL,
	email VARCHAR(20),
	fone VARCHAR(20),
	codProf INTEGER NOT NULL,
        idade integer not null,
	codCid INTEGER NOT NULL,
	PRIMARY KEY (codigo),
	FOREIGN KEY (codProf) REFERENCES profissao (codigo),
	FOREIGN KEY (codCid) REFERENCES cidade (codigo)
);

CREATE TABLE laboratorio
(
	codigo INTEGER NOT NULL,
	nome VARCHAR(20) NOT NULL,
	codCid INTEGER,
	PRIMARY KEY (codigo),
	FOREIGN KEY (codCid) REFERENCES CIDADE (codigo)
);

CREATE TABLE medico
(
	codigo INTEGER NOT NULL,
	nome VARCHAR(20) NOT NULL,
	email VARCHAR(20),
	CRM VARCHAR(20),
	codCid INTEGER NOT NULL,
	PRIMARY KEY (codigo),
	FOREIGN KEY (codCid) REFERENCES CIDADE (codigo)
);

CREATE TABLE especializacao
(
	codigo INTEGER NOT NULL,
	nome VARCHAR(20) NOT NULL,
	area VARCHAR(20) NOT NULL,
	PRIMARY KEY (codigo)
);

CREATE TABLE medEsp
(
	codEsp INTEGER NOT NULL,
	codMed INTEGER NOT NULL,
	PRIMARY KEY (codEsp, codMed),
	FOREIGN KEY (codEsp) REFERENCES especializacao (codigo),
	FOREIGN KEY (codMed) REFERENCES medico (codigo)
);

CREATE TABLE consulta
(
	data DATE NOT NULL,
	hora TIME NOT NULL,
	codPAc INTEGER NOT NULL,
	codMed INTEGER NOT NULL,
	valor numeric(18,4),
	codconv integer,
	PRIMARY KEY (data, hora, codPac),
	foreign key(codconv) references convenio (codigo),
	FOREIGN KEY (codPac) REFERENCES paciente (codigo),
	FOREIGN KEY (codMed) REFERENCES medico (codigo)
);

CREATE TABLE medicamento
(
	codigo INTEGER NOT NULL,
	descricao VARCHAR(20) NOT NULL,
        tipo VARCHAR (10),
	PRIMARY KEY (codigo)
);

CREATE TABLE cons_medicame
(
	data DATE  NOT NULL,
	hora TIME NOT NULL,
	codPac INTEGER NOT NULL,
	codMedica INTEGER NOT NULL,
	PRIMARY KEY (codMedica, data, hora, codPac),
	FOREIGN KEY (codMedica) REFERENCES medicamento (codigo),
	FOREIGN KEY (data, hora, codPac) REFERENCES consulta (data, hora, codPac)
)

/* tabela profissao */

INSERT INTO profissao (codigo, nome, area) VALUES (1, 'mecânico', 'utensilio do lar');
INSERT INTO profissao (codigo, nome, area) VALUES (2, 'professor', 'saúde');
INSERT INTO profissao (codigo, nome, area) VALUES (3, 'padeiro', 'pães doces');
INSERT INTO profissao (codigo, nome, area) VALUES (4, 'motorista', 'transporte vivo');
INSERT INTO profissao (codigo, nome, area) VALUES (5, 'advogado', 'civil');
INSERT INTO profissao (codigo, nome, area) VALUES (6, 'empresário', 'vestuário');

/* tabela convenio*/

insert into convenio values (1, 'Particular');
insert into convenio values (2, 'Uni');
insert into convenio values (3, 'Solar');

/* tabela cidade */
INSERT INTO cidade (codigo,nome, uf) VALUES (1,'Carazinho', 'RS');
INSERT INTO cidade (codigo,nome, uf) VALUES (2,'São Paulo', 'SP');
INSERT INTO cidade (codigo,nome, uf) VALUES (3,'Rio de Janeiro', 'RJ');
INSERT INTO cidade (codigo,nome, uf) VALUES (4,'Joenville', 'SC');
INSERT INTO cidade (codigo,nome, uf) VALUES (5,'Porto Alegre', 'RS');
INSERT INTO cidade (codigo,nome, uf) VALUES (6,'Passo Fundo', 'RS');
INSERT INTO cidade (codigo,nome, uf) VALUES (7,'Casca', 'RS');
INSERT INTO cidade (codigo,nome, uf) VALUES (8,'Santa Maria', 'RS');
INSERT INTO cidade (codigo,nome, uf) VALUES (9,'Cruz Alta', 'RS');


/* tabela paciente*/

INSERT INTO paciente (codigo, nome, fone, codProf, codCid, idade) VALUES (1,'João Carlos','33303930', 1, 1, 20);
INSERT INTO paciente (codigo, nome, email, fone, codProf, codCid, idade) VALUES (2,'Maria Aparecida','MARIA@A.COM.BR','33303325', 2, 9, 10);
INSERT INTO paciente (codigo, nome, codProf, codCid, idade) VALUES (3,'Pedro Antonio', 3, 5, 22);
INSERT INTO paciente (codigo, nome, fone, codProf, codCid, idade) VALUES (4,'Marcos Vinicius', '33303930', 1, 7, 45);
INSERT INTO paciente (codigo, nome, email, fone, codProf, codCid, idade) VALUES (5,'Carolina Pereira', 'cp@a.bcd.efg', '9999.9999', 1, 9,44);
INSERT INTO paciente (codigo, nome, email, fone, codProf, codCid, idade) VALUES (6,'Antonio Carlos', 'ac@a.bcd.efg', '9999.9999', 5, 1, 12);
INSERT INTO paciente (codigo, nome, email, fone, codProf, codCid, idade) VALUES (7,'Mariana Faria', 'mf@a.bcd.efg', '9999.9999', 6, 2, 15);
INSERT INTO paciente (codigo, nome, fone, codProf, codCid, idade) VALUES (8,'Joaquim José Silva','33559630', 3, 1, 34);
INSERT INTO paciente (codigo, nome, fone, codProf, codCid, idade) VALUES (9,'Joana Darq','22304430', 2, 1, 67);
INSERT INTO paciente (codigo, nome, fone, codProf, codCid, idade) VALUES (10,'Pedro Avares','13239830', 6, 1, 20);

/* tabela laboratorio*/

INSERT INTO laboratorio (codigo, nome, codCid) VALUES (1,'Brasil', 3);
INSERT INTO laboratorio (codigo, nome, codCid) VALUES (2,'Delta', 8);
INSERT INTO laboratorio (codigo, nome, codCid) VALUES (3,'MedLab', 4);
INSERT INTO laboratorio (codigo, nome, codCid) VALUES (4,'LabMed', 5);


/* tabela medico*/

INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (1,'Paulo Rangel', 'pr@eee.ccc.br', '23453',1);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (2,'Ana Maria', 'am@eee.ccc.br', '555453',5);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (3,'José Paulo O', 'jpo@eee.ccc.br', '677755',6);
INSERT INTO medico (codigo, nome, CRM, codCid) VALUES (4,'Carla Ana', '987666',4);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (5,'Nena Lina', 'nena@eee.ccc.br', '245543',7);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (6,'Paulina Tirou', 'tirou@eee.ccc.br', '2564555',9);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (7,'Luara dos Santos', 'lua@eee.ccc.br', '983456',1);
INSERT INTO medico (codigo, nome, email, CRM, codCid) VALUES (8,'Luan dos Santos', 'lsantos@eee.ccc.br', '89898',1);

/* tabela especializacao*/

INSERT INTO especializacao (codigo, nome, area) VALUES (1,'Cardiologia','Pressao Arterial');
INSERT INTO especializacao (codigo, nome, area) VALUES (2,'Urologia','Enxaqueca');
INSERT INTO especializacao (codigo, nome, area) VALUES (3,'Psiquiatria','Problemas Mentais');
INSERT INTO especializacao (codigo, nome, area) VALUES (4,'Ginecologista','Obstetras');
INSERT INTO especializacao (codigo, nome, area) VALUES (5,'Fonoaudiologia','Surdez Temporária');
INSERT INTO especializacao (codigo, nome, area) VALUES (6,'Psicologia','Reabilitação');
INSERT INTO especializacao (codigo, nome, area) VALUES (7,'Fisioterapia','Reabilitação Mental');
INSERT INTO especializacao (codigo, nome, area) VALUES (8,'Clínico Geral','Problemas Renais');
INSERT INTO especializacao (codigo, nome, area) VALUES (9,'Clínico Geral','Problemas Pulmonares');
INSERT INTO especializacao (codigo, nome, area) VALUES (10,'Ortopedia','Coluna');
INSERT INTO especializacao (codigo, nome, area) VALUES (11,'Ortopedia','Joelho');
INSERT INTO especializacao (codigo, nome, area) VALUES (12,'Pediatria','Geral');

/* tabela medEsp*/

INSERT INTO medEsp(codEsp, codMed) VALUES (1,6);
INSERT INTO medEsp(codEsp, codMed) VALUES (1,3);
INSERT INTO medEsp(codEsp, codMed) VALUES (2,5);
INSERT INTO medEsp(codEsp, codMed) VALUES (3,2);
INSERT INTO medEsp(codEsp, codMed) VALUES (7,4);
INSERT INTO medEsp(codEsp, codMed) VALUES (10,5);
INSERT INTO medEsp(codEsp, codMed) VALUES (11,6);
INSERT INTO medEsp(codEsp, codMed) VALUES (6,1);
INSERT INTO medEsp(codEsp, codMed) VALUES (4,6);
INSERT INTO medEsp(codEsp, codMed) VALUES (2,1);
INSERT INTO medEsp(codEsp, codMed) VALUES (12,7);


/* tabela consulta*/

INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-02-20', '10:00:00', 1, 3, 100.00, 1);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-02-21', '11:00:00', 3, 2, 100.00, 1);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-02-22', '14:00:00', 4, 3, 100.00, 1);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-02-23', '13:00:00', 5, 4, 50.00, 2);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2005-02-20', '15:00:00', 6, 2, 100.00, 1);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2005-02-21', '16:00:00', 4, 1, 30.00, 3);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-03-20', '17:00:00', 7, 2, 44.00, 3);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-03-21', '09:00:00', 3, 1, 100.00, 1);

INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-03-21', '09:00:00', 2, 7, 100.00, 1);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2006-03-21', '09:00:00', 6, 7, 100.00, 1);

INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2002-03-21', '09:00:00', 6, 7, 122.00, NULL);
INSERT INTO consulta (data, hora, codPac, codMed, valor, codconv) VALUES ('2004-10-20', '15:00:00', 6, 3, 120.00, NULL);

/* tabela medicamento*/

INSERT INTO medicamento (codigo, descricao) VALUES (1, 'Diclofenaco');
INSERT INTO medicamento (codigo, descricao) VALUES (2, 'Olina');
INSERT INTO medicamento (codigo, descricao) VALUES (3, 'Aspirina');
INSERT INTO medicamento (codigo, descricao) VALUES (4, 'Tylenol');
INSERT INTO medicamento (codigo, descricao) VALUES (5, 'Hipoglos');
INSERT INTO medicamento (codigo, descricao) VALUES (6, 'Eno');
INSERT INTO medicamento (codigo, descricao) VALUES (7, 'Benzetacil');
INSERT INTO medicamento (codigo, descricao) VALUES (8, 'Sonrisal');
INSERT INTO medicamento (codigo, descricao) VALUES (9, 'Sorine');
INSERT INTO medicamento (codigo, descricao) VALUES (10,'Moura Brasil');
INSERT INTO medicamento (codigo, descricao) VALUES (11,'Engove');
 

/* tabela cons_medicame*/


INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-20', '10:00:00', 1, 2);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-20', '10:00:00', 1, 4);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-21', '11:00:00', 3, 2);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-21', '11:00:00', 3, 5);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-23', '13:00:00', 5, 1);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-23', '13:00:00', 5, 10);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-02-23', '13:00:00', 5, 11);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2005-02-20', '15:00:00', 6, 10);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2005-02-21', '16:00:00', 4, 10);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2005-02-21', '16:00:00', 4, 11);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-03-20', '17:00:00', 7, 10);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-03-21', '09:00:00', 3, 10);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-03-21', '09:00:00', 3, 8);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-03-21', '09:00:00', 3, 6);
INSERT INTO cons_medicame (data, hora, codPac, codMedica) VALUES ('2006-03-21', '09:00:00', 3, 3)








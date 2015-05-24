/* Organização das informações acadêmicas. ###################################
 * Nome, Curso de Formacao, Instituicao de formacao, Nome Completo do 
 * Orientador, Ano de Ingresso, Ano de Término, Referencias ... 
 * */

:- dynamic informacoesAcademicas/1.

/* Roger Rocker
 * */
informacoesAcademicas( ['Roger Rocker', 'Ciencias da Computacao', 
'ITA', 'Cashis Ferrari', 2010, 2015 ] ).

/* Stiven Stronger
 * */
informacoesAcademicas( ['Stiven Stronger', 'Ciencias da Computacao', 
'ITA', 'Loyd Banks', 2003, 2007, 'Avril Lavigne', 'Demi Lovato' ] ).
informacoesAcademicas( ['Stiven Stronger', 'Sexologia', 
'ITA', 'Caio Inteiro', 2007, 2012, 'Madonna' ] ).

/* Robson Troller
 * */
informacoesAcademicas( ['Robson Troller', 'Engenharia Mecanica', 
'UFSC', 'Bob Null', 2008, 2011, 'Marshall Mathers' ] ).

/* Juca Juquinha
 * */
informacoesAcademicas( ['Juca Juquinha', 'Ciencias da Computacao', 
'UFSC', 'Rui Normal', 2007, 2012, 'Bruno Mars', 'Rock Star' ] ).

/* Storn Front
 * */
informacoesAcademicas( ['Storn Front', 'Ciencias da Automacao', 
'UFMG', 'Elton Xavier', 2005, 2010 ] ).

/* Marcelo Cabrera
 * */
informacoesAcademicas( ['Marcelo Cabrera', 'Engenharia da Computacao', 
'USP', 'John Crazy', 2009, 2015, 'Silvio Santos' ] ).

/* Julian Still
 * */
informacoesAcademicas( ['Julian Still', 'Ciencias Biologicas', 
'UNICAMP', 'Sorocaba Sucuri', 2002, 2012, 'Silvio Santos', 'Bruno Mars' ] ).

/* Luiz Courier
 * */
informacoesAcademicas( ['Luiz Courier', 'Engenharia da Computacao', 
'UFSC', 'Vilmar Alegria', 2000, 2005, 'Roberto Carlos' ] ).

/* Wagner Nascimento
 * */
informacoesAcademicas( ['Wagner Nascimento', 'Ciencias Juridicas', 
'UFSC', 'Xander Astro', 2002, 2012 ] ).

/* Lucas Troll
 * */
informacoesAcademicas( ['Lucas Troll', 'Ciencias Sociais', 
'ITA', 'Tifany Funk', 2007, 2013 , 'Demi Lovato' ] ). 
informacoesAcademicas( ['Lucas Troll', 'Sexologia', 
'ITA', 'Zak Stranger', 2004, 2013 , 'Maroon 5', 'Clean Bandit' ] ). 
informacoesAcademicas( ['Lucas Troll', 'Medicina', 
'ITA', 'Alisson Soner', 2011, 2013 , 'Avenged Sevenfold', 'Eminem' ] ). 

/* Organização das informações pessoais. #####################################
 * Nome, data de nascimento, cidade, telefone 
 * */

:- dynamic informacoesPessoais/1.

/* Juca Juquinha
 * */
informacoesPessoais( ['Juca Juquinha', date( 1990, 1, 10 ), 
'Sao Paulo', 4837326424 ] ).

/* Storn Front
 * */
informacoesPessoais( ['Storn Front', date( 1977, 2, 15 ), 
'Florianopolis', 4737326422 ] ).

/* Marcelo Cabrera
 * */
informacoesPessoais( ['Marcelo Cabrera', date( 1971, 5, 13 ), 
'Sao Paulo', 4737326427 ] ).

/* Julian Still
 * */
informacoesPessoais( ['Julian Still', date( 1978, 12, 09 ), 
'Rio de Janeiro', 4887326455 ] ).

/* Luiz Courier
 * */
informacoesPessoais( ['Luiz Courier', date( 1982, 10, 30 ), 
'Rio Branco', 4887326465 ] ).

/* Wagner Nascimento
 * */
informacoesPessoais( ['Wagner Nascimento', date( 1990, 1, 10 ), 
'Florianopolis', 1177326475 ] ).

/* Lucas Troll
 * */
informacoesPessoais( ['Lucas Troll', date( 1981, 8, 20 ), 
'Cuiaba', 1177326825 ] ).

/* Robson Troller
 * */
informacoesPessoais( ['Robson Troller', date( 1982, 7, 04 ), 
'Joao Pessoa', 8197326625 ] ).

/* Stiven Stronger
 * */
informacoesPessoais( ['Stiven Stronger', date( 1986, 6, 12 ), 
'Brasilia', 8177326825 ] ).

/* Roger Rocker
 * */
informacoesPessoais( ['Roger Rocker', date( 1976, 11, 20 ), 
'Florianopolis', 4887347425 ] ).

/* Organização das informações profissionais. ################################
 * Nome, Nome da Empresa, Nome do Cargo, Ano de Ingresso, Ano de Término, 
 * Nome Completo de Colegas como Referências...
 * */

:- dynamic informacoesProfissionais/1.

/* Juca Juquinha
 * */
informacoesProfissionais( ['Juca Juquinha', 'Brasil Telecom', 
'Programador Junior', 2002, 2006, 'Storn Front', 'Marcelo Cabrera' ] ).

/* Storn Front
 * */
informacoesProfissionais( ['Storn Front', 'Microsoft', 
'Assistente Sexual', 2009, 2015 ] ).
informacoesProfissionais( ['Storn Front', 'Google', 
'Programador Senior', 2001, 2008, 'Marcelo Cabrera' ] ).
informacoesProfissionais( ['Storn Front', 'Apple', 
'Puxa Saco', 2001, 2008, 'Michael Jackson', 'Britney Spears' ] ).

/* Marcelo Cabrera
 * */
informacoesProfissionais( ['Marcelo Cabrera', 'GTV', 
'Enxedor De Linguica', 1995, 2015 ] ).

/* Julian Still
 * */
informacoesProfissionais( ['Julian Still', 'Google', 
'Programador', 1989, 2013, 'Alexandre Gonçalves Silva', 'Ruan Ramon' ] ).

/* Luiz Courier
 * */
informacoesProfissionais( ['Luiz Courier', 'Apple', 
'Desprogramador', 1990, 2015, 'Osvaldo Paulo Heiderscheidt Roberge Martins' ] ).

/* Wagner Nascimento
 * */
informacoesProfissionais( ['Wagner Nascimento', 'Microsoft', 
'Despuxador de Saco', 2005, 2011, 'Eminem', 'Demi Lovato', 'Lily Allen' ] ).

/* Lucas Troll
 * */
informacoesProfissionais( ['Lucas Troll', 'Google', 
'Repuxador de Saco', 2003, 2013 ] ).

/* Robson Troller
 * */
informacoesProfissionais( ['Robson Troller', 'Google', 
'Exorcista', 1979, 2015, 'Homer Simpson', 'Marge Simpson' ] ).

/* Stiven Stronger
 * */
informacoesProfissionais( ['Stiven Stronger', 'TIM', 
'Re-exorcista', 1980, 2015, 'Simpser Simpson', 'Bart Simpser' ] ).
informacoesProfissionais( ['Stiven Stronger', 'Google', 
'Repuxador de Saco', 2015, 2040, 'Demi Lovato', 'Avicii' ] ).

/* Roger Rocker
 * */
informacoesProfissionais( ['Roger Rocker', 'Apple', 
'Assitente Sexual', 1980, 2015, 'Madonna', 'Justin Bieber', 'Freddy Krueger' ] ).







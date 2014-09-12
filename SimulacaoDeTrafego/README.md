Semestre-2014-2
===============

Projeto I: Simula��o de Tr�fego

Semestre-2014-2
===============
Problema para o Projeto I: Simula��o de Tr�fego

Uma simula��o gr�fica, com alguns par�metros a mais na simula��o, como:
'o tempo de chegada de novos carros' de forma a existir a possibilidade 
de ocorrer "retrobloqueio" de um cruzamento em fun��o de ac�mulo de carros 
no trecho ap�s o sinal.


Requisitos Elicitados

Pistas

1. Representar cada pista por uma fila

1.1 O sistema possui uma lista de suas filas

1.2 Cada fila contem uma lista das filas de sa�da e uma velocidade

1.3 Cada fila possui umna velocidade

1.4 Carros saem de uma fila para ir para outra com a velocidade da fila 
destino

1.5 Cada fila tem um tamanho fixo em metros e comporta um n�mero limitado 
de carros.

1'.6 Quando uma pista enche, a entrada daquela pista � bloqueada e o 
sem�foro � marcado

1.7 Se o pr�ximo carro de uma pista estiver programado para entrar em uma 
pista cheia, ele n�o entra e bloqueia a pista.

1.8 Cada pista tem uma vari�vel randomica com distribui��o uniforme 
dividida em faixas de valores que modela para qual de suas pistas 
eferentes um carro vai ir.

1.9 Algumas filas s�o sumidouros e carros que nela entram, s�o eliminados 
ap�s a percorrerm.

1.10 Algumas filas s�o fontes e "recebem" carros a intervalos rand�micos 
dentro de uma faixa de tempos com m�dia e faixa de valores definida 
pelo professor em aula (PPT).


Sem�foro

2. O sistema possui uma lista dos sem�foros

2.1 Cada sem�foro possui uma lista das filas que fazem parte dele, 
dividindo-as em eferentes (sa�da) e aferentes (entrada).

2.2 Cada sem�foro asocia � lista de "pistas eferentes" (as suas filas 
de s�ida) uma lista de probabilidades de um carro dobrar em cada uma 
dessas pistas eferentes.


Ve�culos

3. Cada ve�culo possui um tamanho

3.1. O tamanho do ve�culo � dado pelo seu tamanho mais 1 metro � 
frente e 2 metros atr�s.


Rel�gio

4. O sistema possui uma lista de eventos que representa o "rel�gio do 
sistema".


4.1: S�o eventos: 

- chegada de um novo carro
- mudan�a de estado do sem�foro
- chegada de carro ao ao sem�foro
- troca de pista 

4.2 O rel�gio � uma lista ordenada por hora de ocorr�ncia do evento.


Gera��o de Valores Aleat�rios

Este � um trabalho de aula e por isso devemos fazer alguns compromissos 
para que o tamanho do trabalho fique dentro de limites fact�veis. 
Podemos imaginar que uma distribui��o realista para o intervalo de 
tempo de chegada de carros � uma vari�vel aleat�ria com distribui��o normal.

Ent�o n�o vamos complicar onde n�o h� necessidade. O importante � 
aprender a programar uma simula��o, e n�o obter dados absolutamente 
realistas. Para facilitar vamos ent�o utilizar vari�veis com 
distribui��o uniforme.

Toques de programa��o para gera��o de valores aleat�rios em um intervalo:

Gerar valores aleatorios com distribui��o uniforme no intervalo 0 a 1, 
utilize as fun��es rand e srand.

Lembre-se de inicializar o sempre gerador de numeros aleatorios, 
antes de usar, para garantir de que sejam usados valores diferentes 
em cada simula��o.

Para gerar um numero entre 0 e 1, voce precisa dividir o valor 
gerado por RAND_MAX, definido em stdlib.h.

Para gerar um numero aleatorio com distribui��o uniforme em um 
intervalo, pegue este resultado, multiplique pelo tamanho do 
intervalo e adicione a valor do limite inferior do intervalo. 
Por exemplo: para gerar um valor aleatorio de tempos de chegada 
entre 8 e 12 segundo (10 +/- 2), voc� pega o tamanho do intervalo, 
que � de 8 a 12 inclusive, logo 5 valores, e multiplica o seu n�mero 
aleatorio de 0 a 1 por 5. A seguir adiciona o limite inferior do 
intervalo, 8, ao resultado. Para que voce possa usar este numero 
ainda falta truncar, pegando so a parte inteira. Para isto basta 
fazer um typecasting: inteiro = (int) real;
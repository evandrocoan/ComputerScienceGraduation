/**
 * Projeto I: Simulação de Tráfego
 * 
 * Semestre-2014-2
 * ===============
 * Problema para o Projeto I: Simulação de Tráfego
 * 
 * Uma simulação gráfica, com alguns parâmetros a mais na simulação, como:
 * 'o tempo de chegada de novos carros' de forma a existir a possibilidade 
 * de ocorrer "retrobloqueio" de um cruzamento em função de acúmulo de carros 
 * no trecho após o sinal.
 * 
 * 
 * Requisitos Elicitados
 * 
 * Pistas
 * 
 * 1. Representar cada pista por uma fila
 * 
 * 1.1 O sistema possui uma lista de suas filas
 * 
 * 1.2 Cada fila contem uma lista das filas de saída e uma velocidade
 * 
 * 1.3 Cada fila possui umna velocidade
 * 
 * 1.4 Carros saem de uma fila para ir para outra com a velocidade da fila 
 * destino
 * 
 * 1.5 Cada fila tem um tamanho fixo em metros e comporta um número limitado 
 * de carros.
 * 
 * 1'.6 Quando uma pista enche, a entrada daquela pista é bloqueada e o 
 * semáforo é marcado
 * 
 * 1.7 Se o próximo carro de uma pista estiver programado para entrar em uma 
 * pista cheia, ele não entra e bloqueia a pista.
 * 
 * 1.8 Cada pista tem uma variável randomica com distribuição uniforme 
 * dividida em faixas de valores que modela para qual de suas pistas 
 * eferentes um carro vai ir.
 * 
 * 1.9 Algumas filas são sumidouros e carros que nela entram, são eliminados 
 * após a percorrerm.
 * 
 * 1.10 Algumas filas são fontes e "recebem" carros a intervalos randômicos 
 * dentro de uma faixa de tempos com média e faixa de valores definida 
 * pelo professor em aula (PPT).
 * 
 * 
 * Semáforo
 * 
 * 2. O sistema possui uma lista dos semáforos
 * 
 * 2.1 Cada semáforo possui uma lista das filas que fazem parte dele, 
 * dividindo-as em eferentes (saída) e aferentes (entrada).
 * 
 * 2.2 Cada semáforo asocia à lista de "pistas eferentes" (as suas filas 
 * de sáida) uma lista de probabilidades de um carro dobrar em cada uma 
 * dessas pistas eferentes.
 * 
 * 
 * Veículos
 * 
 * 3. Cada veículo possui um tamanho
 * 
 * 3.1. O tamanho do veículo é dado pelo seu tamanho mais 1 metro à 
 * frente e 2 metros atrás.
 * 
 * 
 * Relógio
 * 
 * 4. O sistema possui uma lista de eventos que representa o "relógio do 
 * sistema".
 * 
 * 
 * 4.1: São eventos: 
 * 
 * - chegada de um novo carro
 * - mudança de estado do semáforo
 * - chegada de carro ao ao semáforo
 * - troca de pista 
 * 
 * 4.2 O relógio é uma lista ordenada por hora de ocorrência do evento.
 * 
 * 
 * Geração de Valores Aleatórios
 * 
 * Este é um trabalho de aula e por isso devemos fazer alguns compromissos 
 * para que o tamanho do trabalho fique dentro de limites factíveis. 
 * Podemos imaginar que uma distribuição realista para o intervalo de 
 * tempo de chegada de carros é uma variável aleatória com distribuição normal.
 * 
 * Então não vamos complicar onde não há necessidade. O importante é 
 * aprender a programar uma simulação, e não obter dados absolutamente 
 * realistas. Para facilitar vamos então utilizar variáveis com 
 * distribuição uniforme.
 * 
 * Toques de programação para geração de valores aleatórios em um intervalo:
 * 
 * Gerar valores aleatorios com distribuição uniforme no intervalo 0 a 1, 
 * utilize as funções rand e srand.
 * 
 * Lembre-se de inicializar o sempre gerador de numeros aleatorios, 
 * antes de usar, para garantir de que sejam usados valores diferentes 
 * em cada simulação.
 * 
 * Para gerar um numero entre 0 e 1, voce precisa dividir o valor 
 * gerado por RAND_MAX, definido em stdlib.h.
 * 
 * Para gerar um numero aleatorio com distribuição uniforme em um 
 * intervalo, pegue este resultado, multiplique pelo tamanho do 
 * intervalo e adicione a valor do limite inferior do intervalo. 
 * Por exemplo: para gerar um valor aleatorio de tempos de chegada 
 * entre 8 e 12 segundo (10 +/- 2), você pega o tamanho do intervalo, 
 * que é de 8 a 12 inclusive, logo 5 valores, e multiplica o seu número 
 * aleatorio de 0 a 1 por 5. A seguir adiciona o limite inferior do 
 * intervalo, 8, ao resultado. Para que voce possa usar este numero 
 * ainda falta truncar, pegando so a parte inteira. Para isto basta 
 * fazer um typecasting: inteiro = (int) real;
 * 
 * @author Evandro  Coan, Charles Borges de Lima
 */

#include <iostream>
using namespace std;

int main() 
{
    cout << "!!!Hello World!!!" << endl; // prints !!!Hello World!!!
    return 0;
}

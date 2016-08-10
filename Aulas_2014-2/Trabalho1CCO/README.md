SistemasDeEquacoesLineares
==========================
Trabalho   (Métodos Diretos e Iterativos para Sistemas de equações lineares):

Dado o seguinte sistema de 10 equações:

           

Matriz Expandida para resolução por métodos diretos: Gauss e Crout

A = [

    [2.5 1.0  1.5  0.0   0.0   0.0   0.0   0.0   0.0   0.0   4];

    [0.0 0.52  0.51 0.0   0.1   0.0   0.0   0.0   0.0   0.0  -3];

    [0.9  1.0  2.9 1.0   0.0   0.0   0.0   0.0   0.0   0.0   1];

    [0.0  1.0  0.2  2.2   1.0   0.0   0.0   0.0   0.0   0.0  -1];

    [1.0  0.0  0.0  2.0  4.0  1.0   0.0   0.0   0.0   0.0  -1];

    [0.0  1.0  0.0  0.0  -2.0  4.0  -1.0   0.0   0.0   0.0   0];

    [1.0  0.0  0.0  0.0   0.0   2.0   4.0   1.0   0.0   0.0  -1];

    [0.0  1.0  0.0  0.0   0.0   0.0   1.0   3.0   1.0   0.0   1];

    [0.0  0.0  1.0  0.0   0.0   0.0   0.0  -1.0  -3.0  -1.0   3];

    [0.0  0.0  0.0  1.0   0.0   0.0   0.0   0.0   1.0   2.0 -2];

    ];

 

a). Crie uma função ou método que avalie se este sistema é Mal-Condicionado (pode ser efetuada dentro do algoritmo de escalonamento de Gauss ou de decomposição LU de Crout);

b). Crei uma função ou método que avalie se este sistema tem convergência garantida e se é recomendado testar o uso de fator de subrelaxação;

c). Determine a solução S={xi} do sistema acima, pelo método de Gauss com Pivotação Parcial a partir da sua matriz expandida [A b] do sistema acima, calcule o resíduo máximo das 10 equações do sistema       [A b] original, para a solução S={xi}, e avalie o tempo de processamento (tente isolar o sistema). Em uma versão 2 do método, calcule (via contador) o número total de operações em PONTO FLUTUANTE de adição/subtração, multiplicação e divisão utilizadas;

d). Determine a solução S={xi} do sistema acima, pelo método de Decomposição LU de Crout com Pivotação Parcial a partir da sua matriz [A] e vetor [b], calcule o resíduo máximo das 10 equações do sistema original, para a solução S={xi}, e avalie o tempo de processamento (tente isolar o sistema). Em uma versão 2 do método, calcule (via contador) o número total de operações em PONTO FLUTUANTE de adição/subtração, multiplicação e divisão utilizadas;

e). Determine a solução S={xi} do sistema acima, pelo método de diagonalização iterativa de Jacobi, com erro máximo 1.10-6, e avalie o tempo de processamento (tente isolar o sistema). Em uma versão 2 do método, calcule (via contador) o número total de operações em PONTO FLUTUANTE de adição/subtração, multiplicação e divisão utilizadas. Utilize fator de subrelaxação, somente se for mandatório;

f). Determine a solução S={xi} do sistema acima, pelo método de diagonalização iterativa de Gauss-Seidel, com erro máximo 1.10-6, e avalie o tempo de processamento (tente isolar o sistema). Em uma versão 2 do método, calcule (via contador) o número total de operações em PONTO FLUTUANTE de adição/subtração, multiplicação e divisão utilizadas. Utilize fator de subrelaxação, somente se for mandatório;

g). Compare, em uma planilha, os números de operações aritméticas de adição/subtração, multiplicação e divisão e total utilizadas nesses métodos, o tempo de processamento, e analise qual foi o método mais eficiente.  Calcule um fator de proporcionalidade para ver se o tempo de processamento é função do Nº total de operações aritméticas em ponto flutuante.

 

Faça um relatório claro e conciso com todos os resultados.

 

 

 

Equações iterativas (Jacobi)

                x(1) = (4-xi(2)-(1.5*xi(3)))/2.5;

                x(2) = (-3-(0.51*xi(3))-(0.1*xi(5)))/0.52;

                x(3) = (1-(0.9*xi(1))-(xi(2))-(xi(4)))/2.9;

                x(4) = (-1-xi(2)-(0.2*xi(3))-(xi(5)))/2.2;

                x(5) = (-1-xi(1)-(2*xi(4))-xi(6))/4;

                x(6) = (-xi(2)+(2*xi(5))+(xi(7)))/4;

                x(7) = (-1-xi(1)-(2*xi(6))-xi(8))/4;

                x(8) = (1-xi(2)-xi(7)-xi(9))/3;

                x(9) = (3-xi(3)+xi(8)+xi(10))/-3;

                x(10) = (-2-xi(4)-xi(9))/2;

 

Equações iterativas (Gauss-Seidel):

                x(1) = (4-x(2)-(1.5*x(3)))/2.5;

                x(2) = (-3-(0.51*x(3))-(0.1*x(5)))/0.52;

                x(3) = (1-(0.9*x(1))-(x(2))-(x(4)))/2.9;

                x(4) = (-1-x(2)-(0.2*x(3))-(x(5)))/2.2;

                x(5) = (-1-x(1)-(2*x(4))-x(6))/4;

                x(6) = (-x(2)+(2*x(5))+(x(7)))/4;

                x(7) = (-1-x(1)-(2*x(6))-x(8))/4;

                x(8) = (1-x(2)-x(7)-x(9))/3;

                x(9) = (3-x(3)+x(8)+x(10))/-3;

                x(10) = (-2-x(4)-x(9))/2;

 

 

Solução  = { 3.726141  -5.172945  -0.094939   3.094741  -2.616497  -0.449635  -1.73849  3.127102  -1.469868  -1.812436}

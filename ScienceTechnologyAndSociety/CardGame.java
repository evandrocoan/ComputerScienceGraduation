/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.awt.HeadlessException;
import javax.swing.JOptionPane;

/**
 *
 * @author decker
 */
public class CardGame {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        JOptionPane.showMessageDialog(null, "BEM VINDO AO JOGO DE CARTAS!\nVocê deverá escolher uma dentre as 13 cartas de um baralho.\nApós sua escolha, o carteador irá escolher uma carta, e para cada carta que ele\nescolher, você contará +1 na carta escolhida .\nQuando a contagem chegar a 20, se o carteador escolher a mesma carta que \n você escolheu, ele ganha, caso \n contrário, você ganha!");
        String cartas[] = {"As", "Dois", "Três", "Quatro", "Cinco", "Seis", "Sete", "Oito", "Nove", "Dez", "Valete", "Dama", "Rei"};
        Boolean digitou = false;
        int escolhida = -1;
        while (!digitou) {

            try {
                escolhida = Integer.parseInt(JOptionPane.showInputDialog("Digite a carta que você quer escolher! (1 a 12)"));
            } catch (NumberFormatException numberFormatException) {
                JOptionPane.showMessageDialog(null, "Você não digitou um número!");
            }
            if (escolhida < 1 || escolhida > 13) {
                JOptionPane.showMessageDialog(null, "Você digitou um número inválido");
            } else {
                digitou = true;
            }
        }
        int contador = escolhida;
        int sorteada = -1;
        do {
            sorteada = (int) (13 * Math.random());
            JOptionPane.showMessageDialog(null, "Sorteou o " + cartas[sorteada] + " ! \nO contador está em " + contador);
            contador++;
        } while (contador != 20);
        if (sorteada + 1 == escolhida) {
            JOptionPane.showMessageDialog(null, "Que chato, você perdeu! :( \nParou em: " + cartas[sorteada] + " e você escolheu " + cartas[escolhida-1]);
        } else {
            JOptionPane.showMessageDialog(null, "PARABÈNS! Você ganhou! \nParou em: " + cartas[sorteada] + " e você escolheu " + cartas[escolhida-1]);
        }
    }
}

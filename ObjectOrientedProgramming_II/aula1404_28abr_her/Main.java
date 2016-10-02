import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Main {
  public static void main(String[] _) {

    Ponto a;
    a = new Circulo(3,4,20);

    a.deslocarX(5);
  
    int diam;
    //diam = ((Circulo)a).diametro();//dowcasting
    
    System.out.println("diam(a) = " + diam);
  }

}//

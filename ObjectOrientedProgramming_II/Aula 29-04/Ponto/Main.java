

class Main {

  public static void main(String[] _) {
    Ponto p;
    p = new Ponto(3,4);

    String saida = "";
    saida += "p = " + p.toString() + "\n";

    //p.deslocarX(2);
    saida += "p = " + p.toString() + "\n";

    saida += "|p,(0,0)| = " + p.distancia(new Ponto(0,0)) + "\n";

    //p.x = 3;//erro!


    Retangulo r = new Retangulo(p, new Ponto(5,8));
    saida += "lar(r) = " + r.lar() + "\n";


    System.out.println(saida);
  }



}

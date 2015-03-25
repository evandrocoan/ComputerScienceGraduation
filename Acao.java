import java.util.*;

/**
 * Representa um ação do mercado de valores. Cada objeto desta classe representa
 * uma ação com um nome e valor de mercado. Cada objeto contém também o número
 * de ações que ele representa.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Acao
{
    ArrayList< String > listOfNames;
    String nome;
    double preco;
    
    public Acao( ArrayList< String > listOfNames )
    {
        Random rand = new Random();
        
        pickAName( rand, listOfNames );
        double truncar = (int) ( rand.nextDouble() * 1000000 );
        this.preco = truncar / 1000;
        
    }
    
    public String getNome()
    {
        return this.nome;
    }
    
    public double getPreco()
    {
        return this.preco;
    }
    
    public void newPrice( double preco )
    {
        this.preco = preco;
    }
    
    public void pickAName( Random rand, ArrayList< String > listOfNames )
    {
        this.nome = listOfNames.get( rand.nextInt( listOfNames.size() ) );
        listOfNames.remove( this.nome );
    }
}
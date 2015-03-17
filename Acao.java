/**
 * Representa um ação do mercado de valores. Cada objeto desta classe representa
 * uma ação com um nome e valor de mercado. Cada objeto contém também o número
 * de ações que ele representa.
 */
public class Acao
{
    private double preco;
    private int quantidade;
    private String nome;
    
    public Acao ()
    {
        this.preco = 0;
        this.quantidade = 0;
        this.nome = "generico";
    }
    
    public Acao ( double preco, int quantidade, String nome )
    {
        this.preco = preco;
        this.quantidade = quantidade;
        this.nome = nome;
    }
    
    public void setPrice( double preco )
    {
        this.preco = preco;
    }
    
    public boolean setQuantidade( int quantidade )
    {
        if ( this.quantidade >= Math.abs( quantidade ) )
        {
            this.quantidade += quantidade;
            return true;
        }
        return false;
    }
    
    public String acaoNames()
    {
        return this.nome;
    }
}
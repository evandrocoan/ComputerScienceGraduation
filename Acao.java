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
    
    /**
     * Construtor padrão.
     */
    public Acao()
    {
        this.preco = 0;
        this.quantidade = 0;
        this.nome = "generico";
    }
    
    /**
     * Construtor que cria uma ação completa.
     * @param preco o preço da ação
     * @param quantidade a quantidade de ações que este objeto representa
     * @param nome o nome da ação que este objeto representa
     */
    public Acao( double preco, int quantidade, String nome )
    {
        this.preco = preco;
        this.quantidade = quantidade;
        this.nome = nome;
    }
    
    /**
     * Define um preço para a ação.
     * @param preco
     */
    public void setPrice( double preco )
    {
        this.preco = preco;
    }
    
    /**
     * Retorna o preço da ação.
     * @return preco o preço da ação
     */
    public double getPrice()
    {
        return this.preco;
    }
    
    /**
     * Refine a quantidade de ações.
     * @param quantidade a quantidade de ações que este objeto representa
     * @return true caso tenha sucesso, false caso contrário
     */
    public boolean setQuantidade( int quantidade )
    {
        if ( this.quantidade >= Math.abs( quantidade ) )
        {
            this.quantidade += quantidade;
            return true;
        }
        return false;
    }
    
    /**
     * Retorna um String com o nome dessa ação.
     * @return
     */
    public String getAcaoNome()
    {
        return this.nome;
    }
}
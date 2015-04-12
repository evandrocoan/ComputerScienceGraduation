/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_dados;

/**
 * Representa uma conta de um usário/administrador em um homebroker.
 * 
 * @authors Evandro  Coan, Renan Pinho Assi
 */
public class Conta
{
    private String nome;
    private String senha;
    private double saldo;
    private boolean administrador;
    private Inventario inventario;
    
    /**
     * Cosntrutor padrão que cria um objeto da classe, sem reslizar nenhum tipo
     * de restrição com os parâmetros que ele recebe.
     * 
     * @param nome
     * @param senha
     * @param saldo
     * @param administrador
     * @param inventario
     */
    public Conta( String nome, String senha, double saldo,
            boolean administrador, Inventario inventario )
    {
        this.nome = nome;
        this.senha = senha;
        this.saldo = saldo;
        this.administrador = administrador;
        this.inventario = inventario;
    }
    
    // #################################### Administrador Access ##########
    /**
     * Define no nome do cliente. Somente o administrador tem acesso a essa
     * funcionanlidade.
     * 
     * @param nome o nome do cliente a ser definido
     */
    public void setNome( String nome )
    {
        if( this.administrador )
        {
            this.nome = nome;
        }
    }
    
    /**
     * Define a senha do cliente. Somente o administrador tem acesso a essa
     * funcionanlidade.
     * 
     * @param senha a senha do cliente a ser definida
     */
    public void setSenha( String senha )
    {
        if( this.administrador )
        {
            this.senha = senha;
        }
    }
    
    /**
     * Define um valor para o saldo. Tal comando é pertencente ao administrador.
     * 
     * @param saldo o saldo da conta do cliente
     */
    public void setSaldo( double saldo )
    {
        if( this.administrador )
        {
            this.saldo = saldo;
        }
    }
    
    /**
     * Retira dinheiro da conta.
     * 
     * @param quantidade a quantidade de saldo a ser retirada da conta. Caso o
     *            saldo seja insuficiente não realiza a operação
     * @return true caso seja realizada a transação, false caso contrário.
     */
    public boolean retirarDinheiro( double quantidade )
    {
        if( this.administrador )
        {
            if( this.saldo >= quantidade )
            {
                this.saldo = this.saldo - quantidade;
            }
        }
        
        return false;
    }
    
    /**
     * Deposit Money.
     * 
     * @param amount a quantidade de saldo a ser colocada na conta.
     * @return true caso seja realizada a transação, false caso contrário
     */
    public boolean depositMoney( double amount )
    {
        if( this.administrador )
        {
            this.saldo = this.saldo + amount;
        }
        
        return false;
    }
    
    // #################################### Client Access ##########
    
    /**
     * Retorna o inventário do cliente. Este contém todas as ações compradas por
     * ele.
     * 
     * @return inventario o inventário do cliente como objeto da classe
     *         inventário.
     */
    public Inventario getInventario()
    {
        return this.inventario;
    }
    
    /**
     * Retorna se a senha para esse usuário confere com a solicitada.
     * 
     * @param senha a senha para ser verificada com a conta.
     * @return true se a senha confere, false caso contrário.
     */
    public boolean checkSenha( String senha )
    {
        return this.senha.equals( senha );
    }
    
    /**
     * Retorna o nome do cliente.
     * 
     * @return o nome do cliente.
     */
    public String getNome()
    {
        return this.nome;
    }
    
    /**
     * Retorna no saldo da conta do cliente.
     * 
     * @return o saldo da conta do cliente.
     */
    public double getSaldo()
    {
        return this.saldo;
    }
    
    /**
     * Retorna o inventário representado como uma String. Essa String é composta
     * pelos nomes das ações no inventário.
     * 
     * 
     * @return inventario o inventário reprensentado como um String
     * @see #Inventario.inventarioToString()
     */
    public String inventarioToString()
    {
        return this.getInventario().inventarioToString();
    }
}
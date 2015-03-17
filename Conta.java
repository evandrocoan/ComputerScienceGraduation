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
    
    // #################################### Administrador Access ##########
    /**
     * Define no nome do cliente. Somente o administrador tem acesso a essa
     * funcionanlidade.
     * @param nome o nome do cliente a ser definido
     */
    public void setNome( String nome )
    {
        if( administrador )
        {
            this.nome = nome;
        }
    }
    
    /**
     * Define a senha do cliente. Somente o administrador tem acesso a essa
     * funcionanlidade.
     * @param nome o nome do cliente a ser definido
     */
    public void setSenha( String senha )
    {
        if( administrador )
        {
            this.senha = senha;
        }
    }
    
    /**
     * Define um valor para o saldo. Tal comando é pertencente ao administrador.
     * @param saldo o saldo da conta do cliente
     */
    public void setSaldo( double saldo )
    {
        if( administrador )
        {
            this.saldo = saldo;
        }
    }
    
    /**
     * Withdraw dinheiro da conta.
     * @param a quantidade de saldo a ser retirada da conta. Caso o saldo seja 
     * insuficiente não realiza a operação
     * @return true caso seja realizada a transação, false caso contrário.
     */
    public boolean withdraw( double amount )
    {
        if( administrador )
        {
            if( saldo >= amount )
            {
                this.saldo = this.saldo - amount;
            }
        }
        
        return false;
    }
    
    /**
     * Deposit Money.
     * @param amount a quantidade de saldo a ser colocada na conta.
     * @return true caso seja realizada a transação, false caso contrário
     */
        public boolean depositMoney( double amount )
    {
        if( administrador )
        {
            this.saldo = this.saldo + amount;
        }
        
        return false;
    }
    
    // #################################### Client Access ##########
    
    /**
     * Retorna o inventário do cliente. Este contém todas as ações compradas 
     * por ele.
     */
    public Inventario getInventario()
    {
        return this.inventario;
    }
    
    /**
     * Retorna se a senha para esse usuário confere com a solicitada.
     * @return true se a senha confere, false caso contrário.
     */
    public boolean checkSenha( String senha )
    {
        return this.senha.equals(senha);
    }
    
    /**
     * Retorna o nome do cliente.
     * @return o nome do cliente.
     */
    public String getNome()
    {
        return this.nome;
    }
    
    /**
     * Retorna no saldo da conta do cliente.
     * @return o saldo da conta do cliente.
     */
    public double getSaldo()
    {
        return this.saldo;
    }
}
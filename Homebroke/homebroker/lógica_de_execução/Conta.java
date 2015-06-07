/**
 * Pacote principal que contém o Homebroker.
 */
package homebroker.lógica_de_execução;

import util.Biblioteca;

/**
 * Representa uma conta de um usuário/administrador em um homebroker.
 * 
 * @author Professional
 */
public class Conta
{
   private String nome;
   private String senha;
   private double saldo;
   private boolean administrador;
   private final Inventario inventário;
   private boolean bloqueada;
   
   /**
    * Construtor padrão que cria um objeto da classe, sem realizar nenhum tipo de restrição com os
    * parâmetros que ele recebe.
    * 
    * @param nome o nome do usuário.
    * @param senha a senha do usuário.
    * @param saldo o saldo do usuário.
    * @param administrador se é um administrador ou não.
    */
   public Conta( final String nome, final String senha, final double saldo,
      final boolean administrador )
   {
      this.nome = nome;
      this.senha = senha;
      this.saldo = saldo;
      this.administrador = administrador;
      this.bloqueada = false;
      this.inventário = new Inventario();
   }
   
   /**
    * Acrescenta ou diminui o saldo.
    * 
    * @param ajuste o valor do ajuste.
    */
   public void ajustarSaldo( final double ajuste )
   {
      this.saldo = this.saldo + ajuste;
   }
   
   /**
    * Retorna se a senha para esse usuário confere com a solicitada.
    * 
    * @param senha a senha para ser verificada com a conta.
    * @return true se a senha confere, false caso contrário.
    */
   public boolean checkSenha( final String senha )
   {
      return this.senha.equals( senha );
   }
   
   /**
    * Cria um inventário fictício de ações contendo 5 ações fictícias.
    *
    * @param quantidade a quantidade de ações fictícias para se criar.
    */
   public void criarInventarioFicticio( final int quantidade )
   {
      for( int i = 0; i < ( quantidade / 5 ); i++ )
      {
         this.inventário.adicionarAoInventario( 2.2 + Biblioteca.gerarNumeroAleatorio(),
            10 + Biblioteca.gerarNumeroAleatorio(),
            "Tabajara SA" + Biblioteca.gerarNumeroAleatorio() );
         
         this.inventário.adicionarAoInventario( 22.2 + Biblioteca.gerarNumeroAleatorio(),
            100 + Biblioteca.gerarNumeroAleatorio(),
            "Tabajara SO" + Biblioteca.gerarNumeroAleatorio() );
         
         this.inventário.adicionarAoInventario( 200.2 + Biblioteca.gerarNumeroAleatorio(),
            1000 + Biblioteca.gerarNumeroAleatorio(),
            "Tabajara SP" + Biblioteca.gerarNumeroAleatorio() );
         
         this.inventário.adicionarAoInventario( 2000.2 + Biblioteca.gerarNumeroAleatorio(),
            10000 + Biblioteca.gerarNumeroAleatorio(),
            "Tabajara ST" + Biblioteca.gerarNumeroAleatorio() );
         
         this.inventário.adicionarAoInventario( 200006.2 + Biblioteca.gerarNumeroAleatorio(),
            10000 + Biblioteca.gerarNumeroAleatorio(),
            "Tabajara SS" + Biblioteca.gerarNumeroAleatorio() );
      }
   }
   
   /**
    * Deposit Money.
    * 
    * @param amount a quantidade de saldo a ser colocada na conta.
    */
   public void depositMoney( final double amount )
   {
      this.saldo = this.saldo + amount;
   }
   
   /**
    * @param nomeAção nome da ação.
    * @return true se ela existe false caso contrário.
    */
   public boolean existeNoInvetário( final String nomeAção )
   {
      return this.inventário.existeNoInvetário( nomeAção );
   }
   
   /**
    * @param quantidade a quantidade de ações
    * @param ação o nome da ação.
    * @return true se existe a quantidade especificada, false caso contrário.
    */
   public boolean existeQuantidade( final int quantidade, final String ação )
   {
      return this.inventário.existeQuantidade( quantidade, ação );
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
    * @param índice o índice da ação no inventário.
    * @return o nome da ação.
    */
   public String getNome( final int índice )
   {
      return this.inventário.getNome( 2 );
   }
   
   /**
    * @param nome o nome da ação a procurar o preço.
    * @return o preço da ação encontrada.
    */
   public double getPreço( final String nome )
   {
      return this.inventário.getPreço( nome );
   }
   
   /**
    * @param nome o nome da ação para encontrar a quantidade.
    * @return a quantidade de ação disponíveis.
    */
   public int getQuantidade( final String nome )
   {
      return this.inventário.getQuantidade( nome );
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
    * Retorna o inventário representado como uma String. Essa String é composta pelos nomes das
    * ações no inventário.
    * 
    * 
    * @return inventario o inventário representado como um String
    * @see #Inventario.inventarioToString()
    */
   public String inventarioToString()
   {
      return this.inventário.inventárioToString();
   }
   
   public boolean isAdministradora()
   {
      return this.administrador;
   }
   
   public boolean isBloqueada()
   {
      return this.bloqueada;
   }
   
   /**
    * Retira dinheiro da conta.
    * 
    * @param quantidade a quantidade de saldo a ser retirada da conta. Caso o saldo seja
    *           insuficiente não realiza a operação
    * @return true caso seja realizada a transação, false caso contrário.
    */
   public boolean retirarDinheiro( final double quantidade )
   {
      if( this.saldo >= quantidade )
      {
         this.saldo = this.saldo - quantidade;
         return true;
      }
      return false;
   }
   
   public void setAdministrador( final boolean privilégio )
   {
      this.administrador = privilégio;
   }
   
   public boolean setBloqueada( final boolean privilégio )
   {
      return this.bloqueada = privilégio;
   }
   
   /**
    * Define no nome do cliente. Somente o administrador tem acesso a essa funcionalidade.
    * 
    * @param nome o nome do cliente a ser definido
    */
   public void setNome( final String nome )
   {
      this.nome = nome;
   }
   
   /**
    * Define um valor para o saldo. Tal comando é pertencente ao administrador.
    * 
    * @param saldo o saldo da conta do cliente
    */
   public void setSaldo( final double saldo )
   {
      this.saldo = saldo;
   }
   
   /**
    * Define a senha do cliente. Somente o administrador tem acesso a essa funcionalidade.
    * 
    * @param senha a senha do cliente a ser definida
    */
   public void setSenha( final String senha )
   {
      this.senha = senha;
   }
}

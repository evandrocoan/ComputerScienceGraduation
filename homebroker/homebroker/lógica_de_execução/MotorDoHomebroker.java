/**
 * 
 */
package homebroker.lógica_de_execução;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import util.Biblioteca;

/**
 * @author Professional
 */
public final class MotorDoHomebroker
{
   /**
    * Responsável por realizar o debug do programa, quando ativado. Deve ser
    * instanciado antes que o construtor desta classe, pois este construtor
    * precisa de deste objeto já instanciado para ser monitorado pelo log.
    */
   private static final Logger LOG = Logger.getLogger( "MotorDoHomebroker" );
   
   /**
    * Único objeto desta classe.
    */
   private static final MotorDoHomebroker INSTÂNCIA = new MotorDoHomebroker();
   
   /**
    * As contasTeste que serão utilizadas para simular a adição de contas no
    * sistema, isto é, as contas criadas somente existirão temporariamente.
    */
   public transient List< Conta > contas;
   
   /**
    * A conta para qual se estará operando o inventário e no mercado de ações.
    */
   private transient Conta contaAutenticada;
   
   private final BookDeOfertas bookDeOfertas;
   
   /**
    * Construtor que inicializa a o motorDoHomebroker e implementa o padrão
    * singleton. O atributo JanelaPrincipal.janelaPrincipal não é inicializado
    * devido a sua construção necessitar de um objeto deste construtor.
    */
   private MotorDoHomebroker()
   {
      MotorDoHomebroker.LOG.setLevel( Level.OFF );
      this.bookDeOfertas = BookDeOfertas.getInstância();
      
      // Cria contas fictícias
      this.criarContasFicticia( 30, "123" );
   }
   
   /**
    * Retorna a única instância existe do MotorDoHomebroker.
    *
    * @return INSTANCE a única instância existe da JanelaPrincipal.
    */
   public static MotorDoHomebroker getInstância()
   {
      return MotorDoHomebroker.INSTÂNCIA;
   }
   
   /**
    * Encerra a execução do Homebroker.
    */
   public static void sairDoSistema()
   {
      System.exit( 0 );
   }
   
   public boolean adicionarConta( final double saldo, final int cpf, final String nome,
      final String senha )
   {
      return this.contas.add( new Conta( nome, senha, saldo, false, null ) );
   }
   
   /**
    * @param preço o preço da ação.
    * @param quantidade a quantidade de ações.
    * @param nome o nome a ação.
    *
    * @return true caso a operação tenha sucesso.
    */
   public boolean adicionarOfertaDeCompra( final double preço, final int quantidade,
      final String nome )
   {
      return this.bookDeOfertas.adicionarOfertaDeCompra( preço, quantidade, nome,
         this.contaAutenticada );
   }
   
   /**
    * @param preço o preço da ação.
    * @param quantidade a quantidade de ações.
    * @param nome o nome a ação.
    *
    * @return true caso a operação tenha sucesso.
    */
   public boolean adicionarOfertaDeVenda( final double preço, final int quantidade,
      final String nome )
   {
      return this.bookDeOfertas.adicionarOfertaDeVenda( preço, quantidade, nome,
         this.contaAutenticada );
   }
   
   public boolean bloquearConta( final String nome )
   {
      for( final Conta conta: this.contas )
      {
         if( conta.getNome().equals( nome ) )
         {
            return conta.definirBloqueada();
         }
      }
      return false;
   }
   
   /**
    * Transforma um ArrayList de contas e uma String
    *
    * @return texto um texto contendo os nomes das contas de teste criadas.
    */
   public String contasTesteToString()
   {
      final StringBuffer texto = new StringBuffer();
      
      int contador = 0;
      for( final Conta conta: this.contas )
      {
         texto.append( conta.getNome() ).append( ( conta.isBloqueada()? "(1)" : "(0)" ) );
         texto.append( ", " );
         if( Biblioteca.quebrarLinha( contador ) )
         {
            texto.append( '\n' );
         }
         contador = contador + 1;
      }
      return texto.append( "\n(1) = Bloqueada, (0) = Desbloqueada." ).toString();
   }
   
   /**
    * Cria contas teste para o sistema.
    *
    * @param quantidade a quantidade de contas teste para se criar
    * @param senha senha que as contas de teste terão
    */
   public void criarContasFicticia( final int quantidade, final String senha )
   {
      final ArrayList< Conta > contasTeste = new ArrayList<>();
      contasTeste.add( new Conta( "admin", "admin", 2000.5 * Biblioteca.gerarNumeroAleatorio(),
         true, new Inventario() ) );
      
      this.criarInventarioFicticio( contasTeste.get( 0 ), quantidade );
      
      Conta contaTeste;
      
      for( int i = 0; i < quantidade; i++ )
      {
         contaTeste =
            new Conta( "User" + Biblioteca.gerarNumeroAleatorio(), senha,
               2000.5 * Biblioteca.gerarNumeroAleatorio(), false, new Inventario() );
         this.criarInventarioFicticio( contaTeste, quantidade );
         
         contasTeste.add( contaTeste );
      }
      if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
      {
         MotorDoHomebroker.LOG.severe( "Estou em criarContasFictícias "
            + contasTeste.get( 0 ).getNome() );
      }
      this.contas = contasTeste;
   }
   
   /**
    * Cria um inventário fictício de ações contendo 5 ações fictícias.
    *
    * @param conta a conta que irá receber as ações fictícias.
    * @param quantidade a quantidade de ações fictícias para se criar.
    */
   public void criarInventarioFicticio( final Conta conta, final int quantidade )
   {
      Ação ação;
      
      for( int i = 0; i < ( quantidade / 5 ); i++ )
      {
         ação =
            new Ação( 2.2 + Biblioteca.gerarNumeroAleatorio(),
               10 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SA"
                  + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
            new Ação( 22.2 + Biblioteca.gerarNumeroAleatorio(),
               100 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SO"
                  + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
            new Ação( 200.2 + Biblioteca.gerarNumeroAleatorio(),
               1000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SP"
                  + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
            new Ação( 2000.2 + Biblioteca.gerarNumeroAleatorio(),
               10000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara ST"
                  + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
         
         ação =
            new Ação( 200006.2 + Biblioteca.gerarNumeroAleatorio(),
               10000 + Biblioteca.gerarNumeroAleatorio(), "Tabajara SS"
                  + Biblioteca.gerarNumeroAleatorio() );
         conta.getInventario().adicionarAoInventario( ação );
      }
   }
   
   /**
    * @param nome uma string contendo o nome da conta a ser excluída.
    */
   public void excluirConta( final String nome )
   {
      for( int index = 0; index < this.contas.size(); index++ )
      {
         final Conta conta = this.contas.get( index );
         
         if( conta.getNome().equals( nome ) )
         {
            this.bookDeOfertas.cancelarOfertas( conta );
            this.contas.remove( index );
         }
      }
   }
   
   public boolean existeAConta( final String nome )
   {
      for( final Conta conta: this.contas )
      {
         if( conta.getNome().equals( nome ) )
         {
            return true;
         }
      }
      return false;
   }
   
   /**
    * Dado o código de uma oferta, informa se existem novas ofertas lançadas no
    * mercado a partir da oferta informada.
    *
    * @param númeroDeOfertas a última oferta visualizada
    * @return true se existem novas ofertas, false caso contrário.
    * @see BookDeOfertas#existemNovasOfertas(int)
    */
   public boolean existemNovasOfertas( final int númeroDeOfertas )
   {
      return this.bookDeOfertas.existemNovasOfertas( númeroDeOfertas );
   }
   
   /**
    * {@link homebroker.lógica_de_execução.Conta#existeNoInvetário(String)}
    *
    * @param açãoParaVender o nome da ação.
    *
    * @return true caso ele exista, false caso contrário.
    */
   public boolean existeNoInventário( final String açãoParaVender )
   {
      return this.contaAutenticada.existeNoInvetário( açãoParaVender );
   }
   
   /**
    * {@link homebroker.lógica_de_execução.Conta#existeQuantidade(int, String)}
    *
    * @param quantidade a quantidade de ações.
    * @param ação o nome da ação.
    *
    * @return true caso exista, false caso contrário.
    */
   public boolean existeQuantidade( final int quantidade, final String ação )
   {
      return this.contaAutenticada.existeQuantidade( quantidade, ação );
   }
   
   /**
    * {@link homebroker.lógica_de_execução.Conta#getPreço(String)}
    *
    * @param açãoParaVender o nome da ação.
    *
    * @return preço o preço da ação.
    */
   public double getPreço( final String açãoParaVender )
   {
      return this.contaAutenticada.getPreço( açãoParaVender );
   }
   
   /**
    * {@link homebroker.lógica_de_execução.Conta#getQuantidade(String)}
    *
    * @param açãoParaVender o nome da ação para vender.
    *
    * @return a quantidade de ações.
    */
   public int getQuantidade( final String açãoParaVender )
   {
      return this.contaAutenticada.getQuantidade( açãoParaVender );
   }
   
   /**
    * @return @see
    *         {@link homebroker.lógica_de_execução.Conta#inventarioToString()}
    */
   public String inventarioToString()
   {
      return this.contaAutenticada.inventarioToString();
   }
   
   /**
    * @return true caso haja alguma conta que esteja autenticada tenha
    *         privilégio de administrador.
    */
   public boolean isAdministradora()
   {
      return this.contaAutenticada.isAdministradora();
   }
   
   /**
    * @return true caso haja alguma conta está autenticada, false caso
    *         contrário.
    */
   public boolean isAutenticada()
   {
      return this.contaAutenticada != null;
   }
   
   /**
    * Verifica se as informações de login são válidas.
    *
    * @param usuário o nome de usuário.
    * @param senha a senha ser verificada.
    *
    * @return true caso seja autenticado a conta
    */
   public boolean loginNoSistemaChecagem( final String usuário, final String senha )
   {
      for( final Conta conta: this.contas )
      {
         if( conta.getNome().equals( usuário ) && conta.checkSenha( senha ) )
         {
            this.contaAutenticada = conta;
            return true;
         }
      }
      return false;
   }
   
   /**
    * @param indice qual oferta buscar
    * @return açãoEmOferta uma String representando uma ação em oferta.
    * @see BookDeOfertas#ofertaToString(int)
    */
   public String ofertaToString( final int indice )
   {
      return this.bookDeOfertas.ofertaToString( indice );
   }
}

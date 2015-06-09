/**
 * Pacote principal que contém o Homebroker.
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
public final class Fachada
{
   private static final Logger LOG;
   private static final Fachada INSTÂNCIA;
   
   static
   {
      LOG = Logger.getLogger( Fachada.class.getName() );
      INSTÂNCIA = new Fachada();
   }
   
   /**
    * As contas que serão utilizadas para simular a adição de contas no sistema, isto é, as contas
    * criadas somente existirão temporariamente.
    */
   public List< Conta > contas;
   
   /**
    * A conta para qual se estará operando o inventário e no mercado de ações.
    */
   private Conta conta;
   
   private final Livros livros;
   
   private Fachada()
   {
      Fachada.LOG.setLevel( Level.OFF );
      this.livros = Livros.getInstância();
      
      this.criarContasFicticias( 30, "123" );
   }
   
   /**
    * Retorna a única instância existe da Fachada.
    *
    * @return INSTÂNCIA a única instância da Fachada. 
    */
   public static Fachada getInstância()
   {
      return Fachada.INSTÂNCIA;
   }
   
   /**
    * Encerra a execução do Homebroker.
    */
   public static void sairDoSistema()
   {
      System.exit( 0 );
   }
   
   public boolean adicionarConta( final double saldo, final int cpf, final String nome,
      final String senha ) throws Exception
   {
      if( !this.isAdministradora() )
      {
         throw new Exception( "Necessário privilégios de administrador!" );
      }
      return this.contas.add( new Conta( nome, senha, saldo, false ) );
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
      if( this.conta == null )
      {
         return this.livros.adicionarOfertaDeCompra( preço, quantidade, this.contas.get( 2 )
            .getNome( 2 ), this.contas.get( 1 ) );
      }
      return this.livros.adicionarOfertaDeCompra( preço, quantidade, nome, this.conta );
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
      if( this.conta == null )
      {
         return this.livros.adicionarOfertaDeVenda( preço, quantidade, this.contas.get( 2 )
            .getNome( 2 ), this.contas.get( 2 ) );
      }
      return this.livros.adicionarOfertaDeVenda( preço, quantidade, nome, this.conta );
   }
   
   /**
    * @param nome a conta a ter os privilégios de administrador removidos.
    * @param privilégio true se administrador, false caso contrário.
    * @throws Exception caso o usuário autenticado não seja administrador.
    */
   public void ajustarPrivilégios( final String nome, final boolean privilégio ) throws Exception
   {
      if( !this.isAdministradora() )
      {
         throw new Exception( "Necessário privilégios de administrador!" );
      }
      for( final Conta conta: this.contas )
      {
         if( conta.getNome().equals( nome ) )
         {
            conta.setAdministrador( privilégio );
         }
      }
   }
   
   public boolean bloquearConta( final String nome ) throws Exception
   {
      if( !this.isAdministradora() )
      {
         throw new Exception( "Necessário privilégios de administrador!" );
      }
      for( final Conta conta: this.contas )
      {
         if( conta.getNome().equals( nome ) )
         {
            return conta.setBloqueada( true );
         }
      }
      return false;
   }
   
   /**
    * @param senha a senha da conta atualmente autenticada.
    * @param nome o nome da conta que terá a senha checada, null para verificar a conta atualmente
    *           autenticada.
    * @return true caso a senha confira, false caso contrário.
    */
   public boolean checarSenha( final String senha, final String nome )
   {
      if( nome == null )
      {
         return this.conta.checkSenha( senha );
      }
      for( final Conta contaTemp: this.contas )
      {
         if( contaTemp.getNome().equals( nome ) )
         {
            return contaTemp.checkSenha( senha );
         }
      }
      return false;
   }
   
   /**
    * Transforma um ArrayList de contas e uma String
    *
    * @return texto um texto contendo os nomes das contas de teste criadas.
    */
   public String contasToString()
   {
      final StringBuffer texto = new StringBuffer();
      
      int contador = 0;
      for( final Conta conta: this.contas )
      {
         
         texto.append( conta.getNome() ).append( " (" )
            .append( ( conta.isAdministradora()? "1" : "0" ) );
         texto.append( ( conta.isBloqueada()? "1" : "0" ) );
         texto.append( "), " );
         if( Biblioteca.quebrarLinha( contador ) )
         {
            texto.append( '\n' );
         }
         contador = contador + 1;
      }
      return texto.append(
         "\n(01) = Bloqueada, (00) = Desbloqueada, (10) = Administrador, "
            + "(00) = Não administrador." ).toString();
   }
   
   /**
    * Cria contas teste para o sistema.
    *
    * @param quantidade a quantidade de contas teste para se criar
    * @param senha senha que as contas de teste terão
    */
   public void criarContasFicticias( final int quantidade, final String senha )
   {
      this.contas = new ArrayList<>();
      this.contas.add( new Conta( "admin", "admin", 2000.5 * Biblioteca.gerarNumeroAleatorio(),
         true ) );
      
      this.contas.get( 0 ).criarInventarioFicticio( quantidade );
      
      Conta contaTemp;
      
      for( int i = 0; i < quantidade; i++ )
      {
         contaTemp = new Conta( "user" + Biblioteca.gerarNumeroAleatorio(), senha,
            2000.5 * Biblioteca.gerarNumeroAleatorio(), false );
         contaTemp.criarInventarioFicticio( quantidade );
         
         if( ( Biblioteca.gerarNumeroAleatorio() % 10 ) == 7 )
         {
            contaTemp.setBloqueada( true );
         }
         if( ( Biblioteca.gerarNumeroAleatorio() % 10 ) > 7 )
         {
            contaTemp.setAdministrador( true );
         }
         this.contas.add( contaTemp );
      }
   }
   
   /**
    * @param nome o nome da conta a verificar se está atualmente logged no sistema.
    * @return true caso sim, false caso contrário.
    */
   public boolean estáLogadoAgora( final String nome )
   {
      return this.conta.getNome().equals( nome );
   }
   
   /**
    * @param nome uma string contendo o nome da conta a ser excluída.
    * @throws Exception caso o usuário autenticado não seja administrador.
    */
   public void excluirConta( final String nome ) throws Exception
   {
      if( !this.isAdministradora() )
      {
         throw new Exception( "Necessário privilégios de administrador!" );
      }
      for( int index = 0; index < this.contas.size(); index++ )
      {
         final Conta conta = this.contas.get( index );
         
         if( conta.getNome().equals( nome ) )
         {
            this.livros.cancelarOfertas( conta );
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
    * Dado o código de uma oferta, informa se existem novas ofertas lançadas no mercado a partir da
    * oferta informada.
    *
    * @param númeroDeOfertas a última oferta visualizada
    * @return true se existem novas ofertas, false caso contrário.
    * @see Livros#existemNovasOfertas(int)
    */
   public boolean existemNovasOfertas( final int númeroDeOfertas )
   {
      return this.livros.existemNovasOfertas( númeroDeOfertas );
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
      return this.conta.existeNoInvetário( açãoParaVender );
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
      return this.conta.existeQuantidade( quantidade, ação );
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
      return this.conta.getPreço( açãoParaVender );
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
      return this.conta.getQuantidade( açãoParaVender );
   }
   
   /**
    * @return @see {@link homebroker.lógica_de_execução.Conta#inventarioToString()}
    */
   public String inventarioToString()
   {
      return this.conta.inventarioToString();
   }
   
   /**
    * @return true caso haja alguma conta que esteja autenticada tenha privilégio de administrador.
    */
   public boolean isAdministradora()
   {
      return this.conta.isAdministradora();
   }
   
   /**
    * @return true caso haja alguma conta está autenticada, false caso contrário.
    */
   public boolean isAutenticada()
   {
      return this.conta != null;
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
         if( conta.getNome().equals( usuário ) && conta.checkSenha( senha ) && !conta.isBloqueada() )
         {
            this.conta = conta;
            return true;
         }
      }
      return false;
   }
   
   /**
    * @param indice qual oferta buscar
    * @return açãoEmOferta uma String representando uma ação em oferta.
    * @see Livros#ofertaToString(int)
    */
   public String ofertaToString( final int indice )
   {
      return this.livros.ofertaToString( indice );
   }
   
   /**
    * @param novaSenha a nova senha da conta atualmente autenticada.
    * @param nome o nome da conta que terá a senha alterada, null para alterar a conta atualmente
    *           autenticada.
    */
   public void setSenha( final String novaSenha, final String nome )
   {
      if( nome == null )
      {
         this.conta.setSenha( novaSenha );
         return;
      }
      for( final Conta contaTemp: this.contas )
      {
         if( contaTemp.getNome().equals( nome ) )
         {
            contaTemp.setSenha( novaSenha );
         }
      }
   }
   
   public String vendaToString( final int indice )
   {
      return this.livros.vendaToString( indice );
   }
}

/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Conta;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 * @author Professional
 */
public final class MotorDoHomebroker
{
    /**
     * Resposável por realizar o debug do programa, quando ativado. Deve ser
     * instânciado antes que o construtor desta classe, pois este construtor
     * precisa de deste objeto já instânciado para ser monitorado pelo log.
     */
    private static final Logger LOG = Logger.getLogger( "MotorDoHomebroker" );
    
    /**
     * Único objeto desta classe.
     */
    private static final MotorDoHomebroker INSTÂNCIA =
        new MotorDoHomebroker();
    
    /**
     * Classe responsável pelo controle da lógica de dados do BookDeOfertas.
     */
    private final MotorDoBook motorDoBook = MotorDoBook.getInstance();
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    public transient List< Conta > contasTeste;
    
    /**
     * A conta para qual se estará operando o inventário e no merdado de ações.
     */
    private transient Conta contaAutenticada;
    
    /**
     * Construtor que inicializa a o motorDoHomebroker e implementa o padrão
     * sigleton. O atributo JanelaPrincipal.janelaPricipal não é inicializado
     * devio a sua construção necessitar de um objeto deste construtor.
     */
    private MotorDoHomebroker()
    {
        MotorDoHomebroker.LOG.setLevel( Level.OFF );
        
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            MotorDoHomebroker.LOG.severe(
                "Estou no construtor de ProgramaPrincipal()" );
        }
        if( MotorDoHomebroker.INSTÂNCIA != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        // Liga o book de ofertas
        final Thread processoDoBook = new Thread( this.motorDoBook );
        processoDoBook.start();
        
        // Cria contas fictícias
        this.contasTeste = UtiliárioDeContas.criarContasFicticia( 30, "123" );
    }
    
    /**
     * @param preço o preço da ação.
     * @param quantidade a quantidade de ações.
     * @param nome o nome a ação.
     * @return true caso a operação tenha sucesso.
     */
    public boolean adicionarOfertaDeVenda( final double preço,
        final int quantidade, final String nome )
    {
        return this.motorDoBook.adicionarOfertaDeVenda( preço, quantidade, nome );
    }
    
    /**
     * @return true caso haja alguma conta está altenticada, false caso
     *         contrário.
     */
    public boolean contaEstáAutenticada()
    {
        return this.contaAutenticada != null;
    }
    
    /**
     * @return string uma string reprensentado as contas teste.
     */
    public String contasTesteToString()
    {
        return UtiliárioDeContas.contasTesteToString( this.contasTeste );
    }
    
    /**
     * Exibe o book de ofertas.
     */
    public void exibirBookDeOfertas()
    {
        if( MotorDoHomebroker.LOG.isLoggable( Level.SEVERE ) )
        {
            if( this.motorDoBook == null )
            {
                MotorDoHomebroker.LOG.severe( "motorDoBook é null!" );
            }
        }
        this.motorDoBook.exibirBookDeOfertas();
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#existeAçãoNoInvetário(String)}
     * 
     * @param açãoParaVender o nome da ação.
     * @return true caso ele exista, false caso contrário.
     */
    public boolean existeAçãoNoInvetário( final String açãoParaVender )
    {
        return this.contaAutenticada.existeAçãoNoInvetário( açãoParaVender );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#existeQuantidadeNoInvetário(int)}
     * 
     * @param quantidade a quantidade de ações.
     * @return true caso exista, false caso contrário.
     */
    public boolean existeQuantidadeNoInvetário( final int quantidade )
    {
        return this.contaAutenticada.existeQuantidadeNoInvetário( quantidade );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#getAçãoPreço(String)}
     * 
     * @param açãoParaVender o nome da ação.
     * @return preço o preço da ação.
     */
    public double getAçãoPreço( final String açãoParaVender )
    {
        return this.contaAutenticada.getAçãoPreço( açãoParaVender );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#getAçãoQuantidade(String)}
     * 
     * @param açãoParaVender o nome da ação para vender.
     * @return a quantidade de ações.
     */
    public int getAçãoQuantidade( final String açãoParaVender )
    {
        return this.contaAutenticada.getAçãoQuantidade( açãoParaVender );
    }
    
    /**
     * @return @see
     *         {@link homebroker.lógica_de_dados.Conta#inventarioToString()}
     */
    public String inventarioToString()
    {
        return this.contaAutenticada.inventarioToString();
    }
    
    /**
     * Verifica se as informações de login saõ válidas.
     * 
     * @param usuário o nome de usuário.
     * @param senha a senha ser verificada.
     * @return true caso seja autenticado a conta
     */
    public boolean loginNoSistemaChecagem( final String usuário,
        final String senha )
    {
        for( final Conta conta: this.contasTeste )
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
     * Retorna a única instancia existe do MotorDoHomebroker.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    public static MotorDoHomebroker getInstância()
    {
        return MotorDoHomebroker.INSTÂNCIA;
    }
    
    /**
     * Encerrra a execução do Homebroker.
     */
    public static void sairDoSistema()
    {
        System.exit( 0 );
    }
}

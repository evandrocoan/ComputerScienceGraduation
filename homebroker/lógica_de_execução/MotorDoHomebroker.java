/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Conta;
import homebroker.lógica_de_dados.Inventario;

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
    
    public boolean adicionarConta( final double saldo, final int cpf,
        final String nome, final String senha )
    {
        return this.contasTeste.add( new Conta( nome, senha, saldo,
            false, new Inventario() ) );
    }
    
    public boolean adicionarOfertaDeCompra( final double preço,
        final int quantidade, final String nome )
    {
        return this.motorDoBook.adicionarOfertaDeCompra( preço, quantidade,
            nome );
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
    
    public boolean bloquearConta( final String nome )
    {
        return UtiliárioDeContas.bloquearConta( nome, this.contasTeste );
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
     * @return true caso haja alguma conta está altenticada, false caso
     *         contrário.
     */
    public boolean isAutenticada()
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
     * {@link homebroker.lógica_de_dados.Conta#existeNoInvetário(String)}
     * 
     * @param açãoParaVender o nome da ação.
     * @return true caso ele exista, false caso contrário.
     */
    public boolean existeNoInvetário( final String açãoParaVender )
    {
        return this.contaAutenticada.existeNoInvetário( açãoParaVender );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#existeQuantidade(int)}
     * 
     * @param quantidade a quantidade de ações.
     * @return true caso exista, false caso contrário.
     */
    public boolean existeQuantidade( final int quantidade )
    {
        return this.contaAutenticada.existeQuantidade( quantidade );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#getPreço(String)}
     * 
     * @param açãoParaVender o nome da ação.
     * @return preço o preço da ação.
     */
    public double getPreço( final String açãoParaVender )
    {
        return this.contaAutenticada.getPreço( açãoParaVender );
    }
    
    /**
     * {@link homebroker.lógica_de_dados.Conta#getQuantidade(String)}
     * 
     * @param açãoParaVender o nome da ação para vender.
     * @return a quantidade de ações.
     */
    public int getQuantidade( final String açãoParaVender )
    {
        return this.contaAutenticada.getQuantidade( açãoParaVender );
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
}

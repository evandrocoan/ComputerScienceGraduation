/**
 * 
 */
package homebroker.lógica_de_execução;

import homebroker.lógica_de_dados.Ação;
import homebroker.lógica_de_dados.Conta;
import homebroker.lógica_de_dados.Inventario;

import java.util.ArrayList;

import javax.swing.JOptionPane;

import util.Biblioteca;

/**
 * 
 * @author Professional
 */
public class MotorDoHomebroker
{
    private static MotorDoHomebroker INSTÂNCIA_DO_MOTOR;
    private static final boolean DEBUG = false;
    
    private MotorDoBook motorDoBook = MotorDoBook.getInstance();
    
    /**
     * Processo que mantém o book de ofertas funcionando enquanto a interface
     * trabalha.
     */
    private Thread processoDoBook;
    
    /**
     * As contasTeste que serão utilizadas para simular a adição de contas no
     * sistema, isto é, as contas criadas somente existirão temporariamente.
     */
    private ArrayList< Conta > contasTeste;
    
    /**
     * A conta para qual se estará operando o inventário e no merdado de ações.
     */
    private Conta contaAutenticada;
    
    /**
     * Retorna a única instancia existe do MotorDoHomebroker.
     * 
     * @return INSTANCE a única instancia existe da JanelaPrincipal.
     */
    public static MotorDoHomebroker getInstance()
    {
        if( MotorDoHomebroker.INSTÂNCIA_DO_MOTOR == null )
        {
            synchronized( MotorDoHomebroker.class )
            {
                if( MotorDoHomebroker.INSTÂNCIA_DO_MOTOR == null )
                {
                    MotorDoHomebroker.INSTÂNCIA_DO_MOTOR =
                            new MotorDoHomebroker();
                }
            }
        }
        return MotorDoHomebroker.INSTÂNCIA_DO_MOTOR;
    }
    
    /**
     * Construtor que inicializa a o motorDoHomebroker e implementa o padrão
     * sigleton. O atributo JanelaPrincipal.janelaPricipal não é inicializado
     * devio a sua construção necessitar de um objeto deste construtor.
     */
    private MotorDoHomebroker()
    {
        if( MotorDoHomebroker.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou no construtor de ProgramaPrincipal()" );
        }
        if( INSTÂNCIA_DO_MOTOR != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        // Liga o book de ofertas
        this.motorDoBook = MotorDoBook.getInstance();
        this.processoDoBook = new Thread( this.motorDoBook );
        this.processoDoBook.start();
        
        // Cria contas fictícias
        this.contasTeste = MotorDoHomebroker.criarContasFicticia( 30, "123" );
        
        // Login temporário para testes.
        this.contaAutenticada = this.contasTeste.get( 0 );
        
        // Cria ofertas de compra e venda fictícias
        // DriverClass.testarBookDeOfertas( this.contasTeste );
    }
    
    /**
     * Inicia o processo de criação da conta de um usuário do sistema
     * 
     * //@return conta a conta criada
     */
    void criarUsuario()
    {
        // TODO
        // String nome = JOptionPane.showInputDialog( "Digite seu nome:" );
        // String senha = JOptionPane.showInputDialog( "Digite sua senha:" );
        // Conta conta = new Conta( nome, senha, 0, false, new Inventario() );
        // ( String nome, String senha, double saldo,boolean
        // administrador, Inventario inventario )
        // return conta;
    }
    
    /**
     * Menu principal que exibe as opções de operação no mercado e na carteira
     * de ações do cliente.
     * 
     * @param commando o comando inserido pelo usuário
     */
    public void menuPrincipal( String commando )
    {
        if( commando == null )
        {
            commando = "s";
        }
        
        switch( commando )
        {
        case "s":
            System.exit( 0 );
            break;
        case "v":
            if( this.contaAutenticada == null )
            {
                JOptionPane.showMessageDialog( null, "Não há "
                        + "nenhuma conta carregada no sistema!" );
                break;
            }
            JOptionPane.showMessageDialog( null,
                    this.contaAutenticada.inventarioToString() );
            break;
        // TODO
        // case "c":
        // Conta novaConta = Principal.criarUsuario();
        // contasTeste.add( novaConta );
        // break;
        case "ov":
            this.efetuarVendaDeAção();
            break;
        case "m":
            if( MotorDoHomebroker.DEBUG )
            {
                if( this.motorDoBook == null )
                {
                    JOptionPane.showMessageDialog( null, "motorDoBook é null!" );
                }
            }
            this.motorDoBook.exibirBookDeOfertas();
            break;
        default:
            JOptionPane.showMessageDialog( null, "Você digitou uma "
                    + "opção inválida!\n\n"
                    + "Digite 's' para fechar o programa.\n"
                    + "Digite 'v' para para ver o inventario\n"
                    // + "Digite 'c' para para criar uma conta!\n"
                    + "Digite 'm' para ver o mercado!\n" );
            break;
        }
    }
    
    private void efetuarVendaDeAção()
    {
        boolean sucesso = false;
        
        while( !sucesso )
        {
            String açãoAComprar =
                    JOptionPane.showInputDialog( this.contaAutenticada
                            .inventarioToString() );
            double preço =
                    Double.parseDouble( JOptionPane
                            .showInputDialog( "Insira o preço da ação:" ) );
            
            int quantidade =
                    (int) Double.parseDouble( JOptionPane
                            .showInputDialog( "Insira a quantidade da ação:" ) );
            
            sucesso =
                    this.motorDoBook.adicionarOfertaDeVenda( preço, quantidade,
                            açãoAComprar );
        }
        
    }
    
    /**
     * Método de realiza o login no sistema.
     * 
     * @param dica uma dica que será aprensetada no menu do login. Inicialmente
     *            ela serve para exibir quais contas estão disponiveis para
     *            login e sua senha
     */
    public void loginNoSistema( String dica )
    {
        Conta login = null;
        String command = " ", usuario = " ", senha = " ";
        boolean inputError = false;
        
        if( dica != null )
        {
            if( dica.equals( "" ) )
            {
                dica = "(" + dica + ")";
            }
        } else
        {
            dica = "";
        }
        
        while( !command.equals( "sair" ) && !usuario.equals( "sair" )
                && !senha.equals( "sair" ) )
        {
            usuario =
                    JOptionPane.showInputDialog( ( inputError
                            ? "Usuário ou senha inválidos\n\n" : "" )
                            + "Insira qual conta será feito login: " + dica );
            if( usuario == null )
            {
                break;
            }
            inputError = false;
            senha =
                    JOptionPane.showInputDialog( "Insira qual senha "
                            + "para a conta: " + usuario );
            
            if( senha == null )
            {
                break;
            }
            
            for( Conta conta: this.contasTeste )
            {
                if( conta.getNome().equals( usuario ) )
                {
                    command = "sa";
                }
                if( conta.checkSenha( senha ) )
                {
                    command += "ir";
                    
                    if( command.equalsIgnoreCase( "sair" ) )
                    {
                        login = conta;
                        break;
                    }
                }
            }
            inputError = true;
        }
        if( login == null )
        {
            System.exit( 0 );
        }
        this.contaAutenticada = login;
    }
    
    /**
     * Cria um inventário fictício de ações contendo 5 ações fictícias
     * 
     * @param conta a conta que irá receber as ações fictícioas
     * @param quantidade a quantidade de ações fictícias para se criar
     */
    public static void criarInventarioFicticio( Conta conta, int quantidade )
    {
        for( int i = 0; i < quantidade / 5; i++ )
        {
            conta.getInventario().adicionarAoInventario(
                    new Ação( 2.2 + util.Biblioteca.gerarNumeroAleatorio(),
                            10 + util.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara SA"
                                    + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 22.2 + util.Biblioteca.gerarNumeroAleatorio(),
                            100 + util.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara SO"
                                    + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 200.2 + util.Biblioteca.gerarNumeroAleatorio(),
                            1000 + util.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara SP"
                                    + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação( 2000.2 + util.Biblioteca.gerarNumeroAleatorio(),
                            10000 + util.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara ST"
                                    + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                    new Ação(
                            200006.2 + util.Biblioteca.gerarNumeroAleatorio(),
                            10000 + util.Biblioteca.gerarNumeroAleatorio(),
                            "Tabajara SS"
                                    + util.Biblioteca.gerarNumeroAleatorio() ) );
        }
    }
    
    /**
     * Cria contas teste para o sistema.
     * 
     * @param quantidade a quantidade de contas teste para se criar
     * @param senha senha que as contas de teste terão
     * @return conta uma nova conta teste com dados fictícios
     */
    public static ArrayList< Conta > criarContasFicticia( int quantidade,
            String senha )
    {
        ArrayList< Conta > contasTeste = new ArrayList<>();
        contasTeste.add( new Conta( "admin", "admin", 2000.5 * util.Biblioteca
                .gerarNumeroAleatorio(), true, new Inventario() ) );
        
        MotorDoHomebroker.criarInventarioFicticio( contasTeste.get( 0 ),
                quantidade );
        
        for( int i = 0; i < quantidade; i++ )
        {
            Conta contaTeste =
                    new Conta( "User" + Biblioteca.gerarNumeroAleatorio(),
                            senha,
                            2000.5 * util.Biblioteca.gerarNumeroAleatorio(),
                            false, new Inventario() );
            MotorDoHomebroker.criarInventarioFicticio( contaTeste, quantidade );
            
            contasTeste.add( contaTeste );
        }
        if( MotorDoHomebroker.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                    "Estou em criarContasFictícias "
                            + contasTeste.get( 0 ).getNome() );
        }
        return contasTeste;
    }
}

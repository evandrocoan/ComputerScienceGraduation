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
    
    /**
     * Transforma um ArrayList de contas e uma String
     * 
     * @param contas um ArrayList contendo as contas
     * @return texto um texto contendo os nomes das contas de teste criadas
     */
    public static String contasTesteToString( ArrayList< Conta > contas )
    {
        String texto = "";
        for( Conta conta: contas )
        {
            texto = texto + conta.getNome() + ", ";
        }
        return texto;
    }
    
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
     * Cria um inventário fictício de ações contendo 5 ações fictícias.
     * 
     * @param conta a conta que irá receber as ações fictícioas.
     * @param quantidade a quantidade de ações fictícias para se criar.
     */
    private static void criarInventarioFicticio( Conta conta, int quantidade )
    {
        for( int i = 0; i < ( quantidade / 5 ); i++ )
        {
            conta.getInventario().adicionarAoInventario(
                new Ação( 2.2 + util.Biblioteca.gerarNumeroAleatorio(),
                    10 + util.Biblioteca.gerarNumeroAleatorio(), "Tabajara SA"
                        + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                new Ação( 22.2 + util.Biblioteca.gerarNumeroAleatorio(),
                    100 + util.Biblioteca.gerarNumeroAleatorio(), "Tabajara SO"
                        + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                new Ação( 200.2 + util.Biblioteca.gerarNumeroAleatorio(),
                    1000 + util.Biblioteca.gerarNumeroAleatorio(),
                    "Tabajara SP" + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                new Ação( 2000.2 + util.Biblioteca.gerarNumeroAleatorio(),
                    10000 + util.Biblioteca.gerarNumeroAleatorio(),
                    "Tabajara ST" + util.Biblioteca.gerarNumeroAleatorio() ) );
            
            conta.getInventario().adicionarAoInventario(
                new Ação( 200006.2 + util.Biblioteca.gerarNumeroAleatorio(),
                    10000 + util.Biblioteca.gerarNumeroAleatorio(),
                    "Tabajara SS" + util.Biblioteca.gerarNumeroAleatorio() ) );
        }
    }
    
    private static void imputError()
    {
        JOptionPane.showMessageDialog( null, "Você digitou uma "
            + "opção inválida!\n\n" + "Digite 's' para fechar o programa.\n"
            + "Digite 'v' para para ver o inventario\n"
            // +
            // "Digite 'c' para para criar uma conta!\n"
            + "Digite 'm' para ver o mercado!\n" );
    }
    
    private static void sairDoSistema()
    {
        System.exit( 0 );
    }
    
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
        if( MotorDoHomebroker.INSTÂNCIA_DO_MOTOR != null )
        {
            throw new IllegalStateException( "Objeto já instânciado!" );
        }
        // Liga o book de ofertas
        this.motorDoBook = MotorDoBook.getInstance();
        this.processoDoBook = new Thread( this.motorDoBook );
        this.processoDoBook.start();
        
        // Cria contas fictícias
        this.criarContasFicticia( 30, "123" );
        
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
    public void criarUsuario()
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
            MotorDoHomebroker.sairDoSistema();
            break;
        case "v":
            this.mostrarInventário();
            break;
        // case "c":
        // TODO
        // this.criarUsuario();
        // break;
        case "ov":
            this.efetuarVendaDeAção();
            break;
        case "m":
            this.exibirBookDeOfertas();
            break;
        default:
            MotorDoHomebroker.imputError();
            break;
        }
    }
    
    /**
     * Cria contas teste para o sistema.
     * 
     * @param quantidade a quantidade de contas teste para se criar
     * @param senha senha que as contas de teste terão
     */
    private void criarContasFicticia( int quantidade, String senha )
    {
        ArrayList< Conta > contasTeste = new ArrayList<>();
        contasTeste.add( new Conta( "admin", "admin", 2000.5 * util.Biblioteca
            .gerarNumeroAleatorio(), true, new Inventario() ) );
        
        MotorDoHomebroker.criarInventarioFicticio( contasTeste.get( 0 ),
            quantidade );
        
        for( int i = 0; i < quantidade; i++ )
        {
            Conta contaTeste =
                new Conta( "User" + Biblioteca.gerarNumeroAleatorio(), senha,
                    2000.5 * util.Biblioteca.gerarNumeroAleatorio(), false,
                    new Inventario() );
            MotorDoHomebroker.criarInventarioFicticio( contaTeste, quantidade );
            
            contasTeste.add( contaTeste );
        }
        if( MotorDoHomebroker.DEBUG )
        {
            JOptionPane.showMessageDialog( null,
                "Estou em criarContasFictícias "
                    + contasTeste.get( 0 ).getNome() );
        }
        this.contasTeste = contasTeste;
    }
    
    private void efetuarVendaDeAção()
    {
        boolean sucesso = false;
        
        while( !sucesso )
        {
            String nome = this.getNomeAçãoParaVenda();
            if( nome == null )
            {
                return;
            }
            double preço = this.getPreçoAçãoParaVenda( nome );
            if( preço == 0 )
            {
                return;
            }
            int quantidade = this.getQuantidadeAçãoParaVenda( nome );
            if( quantidade == 0 )
            {
                return;
            }
            sucesso =
                this.motorDoBook.adicionarOfertaDeVenda( preço, quantidade,
                    nome );
        }
        
    }
    
    private void exibirBookDeOfertas()
    {
        if( MotorDoHomebroker.DEBUG )
        {
            if( this.motorDoBook == null )
            {
                JOptionPane.showMessageDialog( null, "motorDoBook é null!" );
            }
        }
        this.motorDoBook.exibirBookDeOfertas();
    }
    
    private String getNomeAçãoParaVenda()
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        String açãoParaVender = null;
        
        while( !sucesso )
        {
            açãoParaVender =
                JOptionPane.showInputDialog( ( nÉsimaVez
                    ? "Ação não existênte!\n\n" : "" )
                    + "Lista de ações disponíveis "
                    + "para venda: \n"
                    + this.contaAutenticada.inventarioToString() );
            if( açãoParaVender == null )
            {
                return null;
            }
            sucesso =
                this.contaAutenticada.existeAçãoNoInvetário( açãoParaVender );
            nÉsimaVez = true;
        }
        return açãoParaVender;
    }
    
    private double getPreçoAçãoParaVenda( String açãoParaVender )
    {
        double preço = 0;
        String imput =
            JOptionPane.showInputDialog( "Insira o preço da ação:",
                Double.toString( this.contaAutenticada
                    .getAçãoPreço( açãoParaVender ) ) );
        if( imput == null )
        {
            return 0;
        }
        preço = Double.parseDouble( imput );
        return preço;
    }
    
    private int getQuantidadeAçãoParaVenda( String açãoParaVender )
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        int quantidade = 0;
        
        while( !sucesso )
        {
            String imput =
                JOptionPane.showInputDialog( ( nÉsimaVez
                    ? "Quantidade não existênte!\n\n" : "" )
                    + "Insira a quantidade da ação:", Integer
                    .toString( this.contaAutenticada
                        .getAçãoQuantidade( açãoParaVender ) ) );
            if( imput == null )
            {
                return 0;
            }
            quantidade = (int) Double.parseDouble( imput );
            sucesso =
                this.contaAutenticada.existeQuantidadeNoInvetário( quantidade );
            nÉsimaVez = true;
        }
        return quantidade;
    }
    
    private void mostrarInventário()
    {
        if( this.contaAutenticada == null )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        JOptionPane.showMessageDialog( null, this.contaAutenticada
            .inventarioToString() );
    }
}

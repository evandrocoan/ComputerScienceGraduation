/**
 * 
 */
package homebroker.interface_gráfica;

import homebroker.lógica_de_execução.MotorDoHomebroker;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 * 
 * @author Professional
 */
public final class JanelaDeVendas extends JFrame
{
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -272784152689390567L;
    
    private static JanelaDeVendas instância;
    
    private final MotorDoHomebroker motor;
    
    private JanelaDeVendas( final MotorDoHomebroker motor )
    {
        this.motor = motor;
    }
    
    /**
     * Efetua a venda de ações.
     */
    public void efetuarVendaDeAção()
    {
        if( this.motor.contaAutenticada == null )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        boolean sucesso = false;
        
        while( !sucesso )
        {
            final String nome = this.getNomeAçãoParaVenda();
            if( nome == null )
            {
                return;
            }
            final double preço = this.getPreçoAçãoParaVenda( nome );
            if( preço == 0 )
            {
                return;
            }
            final int quantidade = this.getQuantidadeAçãoParaVenda( nome );
            if( quantidade == 0 )
            {
                return;
            }
            sucesso = this.motor.adicionarOfertaDeVenda( preço,
                quantidade, nome );
        }
        
    }
    
    private String getNomeAçãoParaVenda()
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        String açãoParaVender = null;
        
        while( !sucesso )
        {
            açãoParaVender = JOptionPane.showInputDialog(
                ( nÉsimaVez? "Ação não existênte!\n\n" : "" )
                + "Lista de ações disponíveis para venda: \n"
                + this.motor.contaAutenticada.inventarioToString() );
            if( açãoParaVender == null )
            {
                return null;
            }
            sucesso = this.motor.contaAutenticada
                .existeAçãoNoInvetário( açãoParaVender );
            nÉsimaVez = true;
        }
        return açãoParaVender;
    }
    
    private double getPreçoAçãoParaVenda( final String açãoParaVender )
    {
        final String imput = JOptionPane.showInputDialog(
            "Insira o preço da ação:", Double
            .toString( this.motor.contaAutenticada
                    .getAçãoPreço( açãoParaVender ) ) );
        if( imput == null )
        {
            return 0;
        }
        double preço;
        
        preço = Double.parseDouble( imput );
        return preço;
    }
    
    private int getQuantidadeAçãoParaVenda( final String açãoParaVender )
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        int quantidade = 0;
        
        while( !sucesso )
        {
            final String imput = JOptionPane.showInputDialog( ( nÉsimaVez
                ? "Quantidade não existênte!\n\n" : "" )
                + "Insira a quantidade da ação:", Integer
                .toString( this.motor.contaAutenticada
                    .getAçãoQuantidade( açãoParaVender ) ) );
            if( imput == null )
            {
                return 0;
            }
            quantidade = (int) Double.parseDouble( imput );
            sucesso = this.motor.contaAutenticada
                .existeQuantidadeNoInvetário( quantidade );
            nÉsimaVez = true;
        }
        return quantidade;
    }
    
    /**
     * @param motor o motor do Homebroker.
     * @return instância uma intância da janela de login.
     */
    public static JanelaDeVendas getInstância( final MotorDoHomebroker motor )
    {
        synchronized( JanelaDeVendas.class )
        {
            if( JanelaDeVendas.instância == null )
            {
                JanelaDeVendas.instância = new JanelaDeVendas( motor );
            }
        }
        return JanelaDeVendas.instância;
    }
}

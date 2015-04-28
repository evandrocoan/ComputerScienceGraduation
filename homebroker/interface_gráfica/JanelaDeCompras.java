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
public final class JanelaDeCompras extends JFrame
{
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -272784152689390567L;
    
    private static JanelaDeCompras instância;
    
    private final MotorDoHomebroker motor;
    
    private JanelaDeCompras( final MotorDoHomebroker motor )
    {
        this.motor = motor;
    }
    
    /**
     * Efetua a venda de ações.
     */
    public void efetuarCompra()
    {
        if( !this.motor.contaEstáAutenticada() )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        boolean sucesso = false;
        
        while( !sucesso )
        {
            final String nome = this.getNomeAção();
            if( nome == null )
            {
                return;
            }
            final double preço = this.getPreçoAção( nome );
            if( preço == 0 )
            {
                return;
            }
            final int quantidade = this.getQuantidadeAção( nome );
            if( quantidade == 0 )
            {
                return;
            }
            sucesso = this.motor.adicionarOfertaDeVenda( preço,
                quantidade, nome );
        }
        
    }
    
    private String getNomeAção()
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        String açãoParaVender = null;
        
        while( !sucesso )
        {
            açãoParaVender = JOptionPane.showInputDialog(
                ( nÉsimaVez? "Ação não existênte!\n\n" : "" )
                    + "Lista de ações disponíveis para venda: \n"
                    + this.motor.inventarioToString() );
            if( açãoParaVender == null )
            {
                return null;
            }
            sucesso = this.motor.existeAçãoNoInvetário( açãoParaVender );
            nÉsimaVez = true;
        }
        return açãoParaVender;
    }
    
    private double getPreçoAção( final String açãoParaVender )
    {
        final String imput = JOptionPane.showInputDialog(
            "Insira o preço da ação:",
            Double.toString( this.motor.getAçãoPreço( açãoParaVender ) ) );
        if( imput == null )
        {
            return 0;
        }
        double preço;
        
        preço = Double.parseDouble( imput );
        return preço;
    }
    
    private int getQuantidadeAção( final String açãoParaVender )
    {
        boolean sucesso = false;
        boolean nÉsimaVez = false;
        int quantidade = 0;
        
        while( !sucesso )
        {
            final String imput = JOptionPane.showInputDialog( ( nÉsimaVez
                ? "Quantidade não existênte!\n\n" : "" )
                + "Insira a quantidade da ação:", Integer
                .toString( this.motor.getAçãoQuantidade( açãoParaVender ) ) );
            if( imput == null )
            {
                return 0;
            }
            quantidade = (int) Double.parseDouble( imput );
            sucesso = this.motor.existeQuantidadeNoInvetário( quantidade );
            nÉsimaVez = true;
        }
        return quantidade;
    }
    
    /**
     * @param motor o motor do Homebroker.
     * @return instância uma intância da janela de login.
     */
    public static JanelaDeCompras getInstância( final MotorDoHomebroker motor )
    {
        synchronized( JanelaDeCompras.class )
        {
            if( JanelaDeCompras.instância == null )
            {
                JanelaDeCompras.instância = new JanelaDeCompras( motor );
            }
        }
        return JanelaDeCompras.instância;
    }
}

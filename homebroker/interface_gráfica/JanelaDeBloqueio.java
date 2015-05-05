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
public final class JanelaDeBloqueio extends JFrame
{
    /**
     * Implementa a serialização do swing.
     */
    private static final long serialVersionUID = -272784152689390567L;
    
    private static JanelaDeBloqueio instância;
    
    /**
     * @param motor o motor do Homebroker.
     * @return instância uma intância desta janela.
     */
    public static JanelaDeBloqueio getInstância( final MotorDoHomebroker motor )
    {
        synchronized( JanelaDeBloqueio.class )
        {
            if( JanelaDeBloqueio.instância == null )
            {
                JanelaDeBloqueio.instância = new JanelaDeBloqueio( motor );
            }
        }
        return JanelaDeBloqueio.instância;
    }
    
    private final MotorDoHomebroker motor;
    
    private final String dica;
    
    private JanelaDeBloqueio( final MotorDoHomebroker motor )
    {
        this.motor = motor;
        this.dica = motor.contasTesteToString();
    }
    
    /**
     * Efetua a venda de ações.
     */
    public void efetuarBloqueio()
    {
        if( !this.motor.isAutenticada() )
        {
            JOptionPane.showMessageDialog( null, "Não há "
                + "nenhuma conta carregada no sistema!" );
            return;
        }
        if( !this.motor.isAdministradora() )
        {
            JOptionPane.showMessageDialog( null, "Acesso negado! "
                + "Você precisa ter privilégio de administrador." );
            return;
        }
        this.solicitarConta();
    }
    
    private void solicitarConta()
    {
        String nome = null;
        boolean inputError = true;
        do
        {
            nome =
                JOptionPane.showInputDialog(
                    ( inputError? "" : "Usuário inválidos\n\n" )
                        + this.dica + "\n\nInsira qual conta será bloqueada: " );
            
            if( nome == null )
            {
                break;
            }
            inputError = this.motor.bloquearConta( nome );
            
        } while( !inputError );
    }
}

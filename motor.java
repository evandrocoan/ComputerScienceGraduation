import javax.swing.*;

import javax.swing.border.EmptyBorder;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

public class motor implements Runnable
{
    JPanel principalPanel;
    int i = 0;
    int a = 0;
    ArrayList< String > listOfNames;
    objectContainer[] acoes;
    JLabel[] precos;
    JLabel[] nomes;
    JLabel[] quantidades;
    
    public motor( JPanel p )
    {
        this.principalPanel = p;
    }
    
    public void run()
    {
        listOfNames =
                new ArrayList< String >( Arrays.asList( "PetroBol", "OGY",
                        "PineApple", "Doors", "Sea", "Mike", "Sadida",
                        "BadYear", "Kebab Hut", "Mundo" ) );
        acoes = new objectContainer[10];
        precos = new JLabel[10];
        quantidades = new JLabel[10];
        nomes = new JLabel[10];
        
        while( true )
        {
            if( i < 10 )
            {
                JPanel subPanel = new JPanel( new GridLayout( 4, 0, 2, 2 ) );
                JButton b = new JButton( "Vender A��o " + a );
                b.addActionListener( new ActionListener()
                {
                    int id = a;
                    
                    public void actionPerformed( ActionEvent e )
                    {
                        acoes[this.id].setQuantidade( acoes[this.id]
                                .getQuantidade() - 1 );
                        quantidades[this.id].setText( ""
                                + ( acoes[this.id].getQuantidade() ) );
                        // i--;
                        // subPanel.setVisible(false);
                        
                    }
                } );
                subPanel.setBackground( Color.black );
                subPanel.setBorder( new EmptyBorder( 4, 4, 4, 4 ) );
                
                acoes[i] = new objectContainer( new Acao( listOfNames ), 100 );
                
                subPanel.add( nomes[i] =
                        new JLabel( acoes[i].getContido().getNome(),
                                JLabel.CENTER ) );
                nomes[i].setOpaque( true );
                nomes[i].setBackground( Color.white );
                
                subPanel.add( quantidades[i] =
                        new JLabel( "QUANTIDADE: " + acoes[i].getQuantidade(),
                                JLabel.CENTER ) );
                quantidades[i].setOpaque( true );
                quantidades[i].setBackground( Color.white );
                
                subPanel.add( precos[i] =
                        new JLabel( "Pre�o: R$"
                                + acoes[i].getContido().getPreco(),
                                JLabel.CENTER ) );
                precos[i].setOpaque( true );
                precos[i].setBackground( Color.white );
                
                subPanel.add( b );
                
                principalPanel.add( subPanel );
                
                i++;
                a++;
            }
            try
            {
                Thread.sleep( 1000 );
            } catch( InterruptedException e )
            {
                
            }
        }
    }
    
    public static void main( String[] args )
    {
        
        GUI g = new GUI();
        g.add( new Canvas() );
        JPanel panel = new JPanel();
        Thread t = new Thread( new motor( panel ) );
        t.start();
        panel.setBackground( Color.WHITE );
        panel.setBounds( 300, 300, 300, 300 );
        g.add( panel, BorderLayout.CENTER );
        
        g.setVisible( true );
    }
}

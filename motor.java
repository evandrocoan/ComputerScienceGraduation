import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class motor  implements Runnable {
	JPanel p;
	int i = 0;
	int a = 0;
	public motor(JPanel p){
		this.p = p;
	}
	
	public void run(){
		
		while(true){
			if(i < 10){
				JButton b = new JButton("Ação " + a );
				b.addActionListener(new ActionListener(){
					public void actionPerformed(ActionEvent e)
		            {
							i--;
		                p.remove(b);                
		            }
				});
				p.add(b);
				i++;
				a++;
			}
			try{
				p.revalidate();
				Thread.sleep(1000);
			}catch(InterruptedException e){
				
			}
		}
	}
			
    public static void main(String[] args) {
    	
    	GUI g = new GUI();
    	g.add(new Canvas());
		JPanel panel = new JPanel();
		Thread t = new Thread(new motor(panel));
		t.start();
		panel.setBackground(Color.WHITE);
		panel.setBounds(300, 300, 300, 300);
		g.add(panel, BorderLayout.CENTER);
		
    	
    
    	g.setVisible(true);
    }

}


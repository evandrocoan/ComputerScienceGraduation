package bin;

import java.awt.Color;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import pilha.*;
import operacoes.*;
import operacoes.soma.*;
import java.io.*;




   
 

/**
 * @author Professional
 *
 */
public class Calculadora {

	private JFrame frmCalculadora;
	private JTextField textFieldLinha1;
	private JTextField textFieldLinha2;
	private JTextField textFieldLinha3;
	private JTextField textFieldLinha4;
	private JTextField textFieldAuxiliar;

	//declara aqui para poder trocar o nome dos botoes quando 2nd funcao for acionada
	private JButton btnPol = new JButton("P\u2194R");
	private JButton btnRet = new JButton("\u22BE");	//simbolo de angulo
	private JButton btnAb = new JButton("cmp?");
	private JButton btnYx = new JButton("y\u02E3");
	private JButton btnLog = new JButton("log");
	private JButton btnLn = new JButton("ln");
	private JButton btnSin = new JButton("sin");
	private JButton btnCos = new JButton("cos");
	private JButton btnTan = new JButton("tan");
	private JButton btnDeg = new JButton("DEG");
	private JButton btnDec = new JButton("10");
	private JButton btnSto = new JButton("STO");
	private JButton btnPi = new JButton("\u2328");	//teclado p/ relatorio, 2nd func = pi "\u03C0"

	ManipulaString valor = new ManipulaString();
	OperacoesMat   op = new OperacoesMat();
    DecimalFormat df = new DecimalFormat("#.##########");//define 10 digitos decimais de precisao com arredondamento

    
    
    

	private boolean SecFunc = true;
	private boolean hyp = true;
	private boolean deg = true;
	private int tipoBase = 0;
	private boolean PolRet = true;
	private double[] MEM = {0,0,0,0,0,0};
	private String base = " - [ base decimal ]";
	private String aux = "Mem [           ]";
	private boolean erro = false;
	private boolean entraDigito = false;


	private static ArrayList< Valor > pilha = new ArrayList<Valor>();
	private static ArrayList< Valor[] > desfazer = new ArrayList<Valor[]>();

	/**
	 * Launch the application.
	 */
	public static void main(String[] args)
	{

		
		
		
		
		
		
		Valor valor1 = new NumeroReal( new BigDecimal( 1 ) );
		Valor valor2 = new NumeroReal( new BigDecimal( 9 ) );

		pilha.add( valor1 );
		pilha.add( valor2 );

		Valor[] operandos = new Valor[2];
		pilha.subList( pilha.size() - 2, pilha.size() ).toArray( operandos );

		Operacao operacao = new Soma( new NumerosReais( operandos ) );

		System.out.println( operandos[0] + " " + operandos[1] );
		System.out.println( );
		System.out.println( "A soma é: " + operacao.operacao() );

		pilha.remove( pilha.size() - 1 );
		pilha.remove( pilha.size() - 1 );

		desfazer.add( operandos );



		EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					Calculadora window = new Calculadora();
					window.frmCalculadora.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public Calculadora() {
		initialize();
	}

	@SuppressWarnings("javadoc")
	public void printTela()//codigo para arredondamento e exclusao de zero final a direita do ponto
	{
		int index = valor.retornaIndexPilha();
	    df.setRoundingMode(RoundingMode.DOWN);//arredondamento para 10 casa decimais
	    String s ="";

		if(index>=1)
		{	s = df.format(valor.retornaValorPilha(0));
			textFieldLinha2.setText(s.replace(",", "."));
		}
		else
			textFieldLinha2.setText("");
		if(index>=2)
		{
			s = df.format(valor.retornaValorPilha(1));
			textFieldLinha3.setText(s.replace(",", "."));
		}
		else
			textFieldLinha3.setText("");
		if(index>=3)
		{
			s = df.format(valor.retornaValorPilha(2));
			textFieldLinha4.setText(s.replace(",", "."));
		}
		else
			textFieldLinha4.setText("");

		//para apagar .0
		if(valor.retornaStringDigitada().isEmpty())
			textFieldLinha1.setText("");
		else
		{
			s =valor.retornaStringDigitada();

			if(valor.contemCaractere(s, '.'))
			{
				if(s.endsWith("0"))
					s = (s.substring(0, s.length() - 2));
			}
			textFieldLinha1.setText(s); //trunca nao arredondo se resultado fica na linha1
		}


	}

	/*public void printStringTela()//para uso com os nrs complexos
	{
		int indexReais = valor.retornaIndexPilha();
		int indexImag = valor.retornaIndexStringPilha();

		if(indexImag>=1)
			textFieldLinha2.setText(valor.retornaStringPilha(0));
		else
			textFieldLinha2.setText("");
		if(indexImag>=2)
			textFieldLinha3.setText(valor.retornaStringPilha(1));
		else
			textFieldLinha3.setText("");
		if(indexImag>=3)
			textFieldLinha4.setText(valor.retornaStringPilha(2));
		else
			textFieldLinha4.setText("");

		textFieldLinha1.setText(valor.retornaStringDigitada());

		//*****ainda corrigir para os valores reais a impressao e ajuste da pilha



	}*/

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frmCalculadora = new JFrame();
		frmCalculadora.setResizable(false);
		frmCalculadora.getContentPane().setBackground(new Color(220, 220, 220));
		frmCalculadora.setTitle("Calculadora");
		frmCalculadora.getContentPane().setFont(new Font("Lucida Grande", Font.PLAIN, 10));
		frmCalculadora.setBounds(100, 100, 464, 457);
		frmCalculadora.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frmCalculadora.getContentPane().setLayout(null);

		JPanel panelTecladoPrincipal = new JPanel();
		panelTecladoPrincipal.setBackground(new Color(220, 220, 220));
		panelTecladoPrincipal.setBounds(14, 282, 430, 144);
		frmCalculadora.getContentPane().add(panelTecladoPrincipal);

		JButton btn7 = new JButton("7");
		btn7.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("7");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn7.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn8 = new JButton("8");
		btn8.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("8");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn8.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn9 = new JButton("9");
		btn9.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("9");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn9.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn4 = new JButton("4");
		btn4.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("4");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn4.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn5 = new JButton("5");
		btn5.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("5");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn5.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn6 = new JButton("6");
		btn6.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("6");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn6.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn1 = new JButton("1");
		btn1.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("1");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn1.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn2 = new JButton("2");
		btn2.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("2");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn2.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn3 = new JButton("3");
		btn3.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("3");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn3.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btn0 = new JButton("0");
		btn0.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("0");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btn0.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btnDot = new JButton(".");
		btnDot.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado(".");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btnDot.setVerticalAlignment(SwingConstants.TOP);
		btnDot.setFont(new Font("Arial", Font.PLAIN, 13));

		JButton btnExp = new JButton("EXP");
		btnExp.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("E");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btnExp.setFont(new Font("Arial", Font.PLAIN, 13));

		//---------------------------------------------------------------
		//DIVISAO --******TEM UM ERRO QUANDO SE DIGITA NUMEROS E ENTER NA MSG DE ERRO!
		//---------------------------------------------------------------
		JButton btnDivisao = new JButton("\u00F7");	//DIVISAO
		btnDivisao.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado=0;
				int index = valor.retornaIndexPilha();

				if(index!=0)//para nao operar se nao tiver dados na pilha
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(valor.retornaValorPilha(0)!=0)
						{
							resultado =  op.divisaoReais(valor.retornaValorPilha(1),valor.retornaValorPilha(0));

							valor.alteraValorResultado(""+resultado);
						}
						else //divisao por zero
						{
							valor.alteraValorResultado("ERRO - divis�o por zero!");

						}
						valor.corrige2PilhaOperacao();

					}
					else
					{
						if(valor.retornaValorNumerico()!=0)
						{
							resultado = op.divisaoReais(valor.retornaValorPilha(0),valor.retornaValorNumerico());

							valor.alteraValorResultado(""+resultado);

						}
						else
						{
							valor.alteraValorResultado("ERRO - divis�o por zero!");
						}
						valor.apagaValorPilha();

					}
					printTela();
				}
			}
		});
		btnDivisao.setFont(new Font("Arial", Font.PLAIN, 13));

		//---------------------------------------------------------------
		//MULTIPLICACAO
		//---------------------------------------------------------------
		JButton btnMultiplicacao = new JButton("\u00D7");
		btnMultiplicacao.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado;
				int index = valor.retornaIndexPilha();

				if(index!=0)//para nao operar se nao tiver dados na pilha
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						resultado =  op.multiplicacaoReais(valor.retornaValorPilha(1),valor.retornaValorPilha(0));
						valor.corrige2PilhaOperacao();
					}
					else
					{
						resultado = op.multiplicacaoReais(valor.retornaValorPilha(0),valor.retornaValorNumerico());
						valor.apagaValorPilha();
					}
					valor.alteraValorResultado(""+resultado);
					printTela();
				}

			}
		});
		btnMultiplicacao.setFont(new Font("Arial", Font.PLAIN, 13));

		//---------------------------------------------------------------
		//SUBTRACAO
		//---------------------------------------------------------------
		JButton btnSubtracao = new JButton("-");
		btnSubtracao.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado;
				int index = valor.retornaIndexPilha();

				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(index>1)
					{
						resultado =  op.subtracaoReais(valor.retornaValorPilha(1),valor.retornaValorPilha(0));
						valor.corrige2PilhaOperacao();
						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{	//se pilha zero vazia diminui com 0
					resultado = op.subtracaoReais(valor.retornaValorPilha(0),valor.retornaValorNumerico());
					valor.apagaValorPilha();
					valor.alteraValorResultado(""+resultado);
				}

				printTela();
			}
		});
		btnSubtracao.setFont(new Font("Arial", Font.PLAIN, 13));

		//---------------------------------------------------------------
		//SOMA
		//---------------------------------------------------------------
		JButton btnSoma = new JButton("+");
		btnSoma.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent arg0)
			{
				
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado;
				int index = valor.retornaIndexPilha();

				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(index>1)
					{
						resultado =  op.somaReais(valor.retornaValorPilha(0),valor.retornaValorPilha(1));
						valor.corrige2PilhaOperacao();
						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
						resultado = op.somaReais(valor.retornaValorNumerico(), valor.retornaValorPilha(0));
						valor.apagaValorPilha();
						valor.alteraValorResultado(""+resultado);
				}
				printTela();
			}
		});
		btnSoma.setFont(new Font("Arial", Font.PLAIN, 13));

		//---------------------------------------------------------------
		//+/-
		//--------------------------------------------------------------
		JButton btnMaisMenos = new JButton("\u00b1");
		btnMaisMenos.setVerticalAlignment(SwingConstants.TOP);
		btnMaisMenos.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				
				String s = valor.retornaStringDigitada();
				int tam = s.length();

				/*int index = valor.retornaIndexPilha();*/

				if(s.isEmpty())
				{
					valor.alteraValorDigitado("-");
				}
				else //logica para retirar '-', se ja estiver na tela
				{
					if(s.charAt(tam-1)=='-')//se tem '-' no final, apaga
						valor.alteraValorResultado(s.substring(0,tam-1));
					else
						valor.alteraValorDigitado("-");

					//if(index!=0)//s� valido para nrs reais
					//{
					//	valor.alteraValorPilha(0, -valor.retornaValorPilha(0));
					//}

				}
				printTela();
			}
		});
		btnMaisMenos.setFont(new Font("Arial", Font.PLAIN, 13));

		//--------------------------------------------------------------------
		//UNDO - não faz mais a rolagem da pilha
		//--------------------------------------------------------------------
		JButton btnUp = new JButton("\u21BA");
		btnUp.setToolTipText("Desfaz última ação.");
		btnUp.addActionListener(new ActionListener()
		{
			private double aux;

			@Override
			public void actionPerformed(ActionEvent e)
			{
				
				if(entraDigito)//se foi digitacao, apaga um digito
				{
					valor.apagaCaractere();
				}
				else
					valor.Copia2dadosPilha();
				
				printTela();
				
				
				/*int index = valor.retornaIndexPilha();

				if(index!=0)
				{
					aux = valor.retornaValorPilha(index-1);

					for(int i=index; i>0; i--)
						valor.alteraValorPilha(i, valor.retornaValorPilha(i-1));
					valor.alteraValorPilha(0, aux);

					printTela();

					/*if(index>=1)
						textFieldLinha2.setText(""+valor.retornaValorPilha(0));
					else
						textFieldLinha2.setText("");
					if(index>=2)
						textFieldLinha3.setText(""+valor.retornaValorPilha(1));
					else
						textFieldLinha3.setText("");
					if(index>=3)
						textFieldLinha4.setText(""+valor.retornaValorPilha(2));
					else
						textFieldLinha4.setText("");
				}*/
			}
		});
		btnUp.setFont(new Font("Arial", Font.PLAIN, 13));

		//--------------------------------------------------------------------
		//x <-> y
		//--------------------------------------------------------------------
		JButton btnSwap = new JButton("x\u2194y");
		btnSwap.addActionListener(new ActionListener()
		{
			private double aux;

			@Override
			public void actionPerformed(ActionEvent e)
			{
				int index = valor.retornaIndexPilha();
				aux = valor.retornaValorPilha(0);

				if(index!=0)	//s� executa se tiver algo na pilha
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))//se linha 1 =="", troca linha2 e linha3
					{
						if(index>1)//se houver dois ou mais elementos troca
						{
							valor.alteraValorPilha(0, valor.retornaValorPilha(1));
							valor.alteraValorPilha(1,aux);
							//textFieldLinha3.setText(""+valor.retornaValorPilha(1));
						}
					}
					else //troca linha1 e linha2
					{
						valor.alteraValorPilha(0, valor.retornaValorNumerico());
						valor.limpaValorDigitado();
						valor.alteraValorDigitado(""+aux);
						//textFieldLinha1.setText(""+valor.retornaStringDigitada());
					}
					//textFieldLinha2.setText(""+valor.retornaValorPilha(0));
				}
				printTela();
			}
		});
		btnSwap.setFont(new Font("Arial", Font.PLAIN, 13));

		//-----------------------------------------------------------------------------
		//INVERSO - 1/x
		//-----------------------------------------------------------------------------
		JButton btnInversao = new JButton("1/x");
		btnInversao.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado=0;
				int index = valor.retornaIndexPilha();

				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(valor.retornaValorPilha(0)!=0)
					{
						resultado =  op.divisaoReais(1,valor.retornaValorPilha(0));
						valor.alteraValorResultado(""+resultado);

					}
					else if(index!=0)
						valor.alteraValorResultado("ERRO - divis�o por zero!");
					valor.apagaValorPilha();
				}
				else
				{
					if(valor.retornaValorNumerico()!=0)
					{
						resultado = op.divisaoReais(1,valor.retornaValorNumerico());
						valor.alteraValorResultado(""+resultado);

					}
					else
					{
						valor.alteraValorResultado("ERRO - divis�o por zero!");
					}
				}

				printTela();
			}
		});
		btnInversao.setFont(new Font("Arial", Font.PLAIN, 13));

		//------------------------------------------------------------------------------
		//RAIZ QUADRADA
		//------------------------------------------------------------------------------
		JButton btnSqrt = new JButton("\u221A");
		btnSqrt.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				
				double resultado;
				int index = valor.retornaIndexPilha();

				
				
				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(index!=0)
					{
						resultado =  op.sqrtReais(valor.retornaValorPilha(0));
						valor.apagaValorPilha();
						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
					resultado = op.sqrtReais(valor.retornaValorNumerico());
					valor.alteraValorResultado(""+resultado);
				}

				printTela();
			}
		});
		btnSqrt.setFont(new Font("Arial", Font.PLAIN, 13));

		//------------------------------------------------------------------------------
		//ENTER - s� para n�meros na base decimal
		//------------------------------------------------------------------------------
		JButton btnEnter = new JButton("ENT");
		btnEnter.setToolTipText("Ativo somente na base 10!");
		btnEnter.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=false;
				
				double[] nrs = new double[2];//nr[0] = mod ou real, nr[1] = ang ou imag
				double real, imag, mod, ang;
				OperacoesMat op = new OperacoesMat();
				String[] s = {"",""};

				if(!erro)
				{
					valor.dadosPilha2Copia();
					try
					{
						int index = valor.retornaIndexPilha();

						if(valor.contemCaractere(valor.retornaStringDigitada(),'<'))//operacao com polar
						{
							if(!PolRet)//converte para retangular
							{
								nrs = valor.stringPolRet2Nrs(valor.retornaStringDigitada(), '<');

								if(deg)
									nrs[1] = op.deg2rad(nrs[1]);//converte o angulo para Radiano
								real = op.real(nrs[0],nrs[1]);
								imag = op.imaginario(nrs[0], nrs[1]);

								s[0] = df.format(real);  s[0] = s[0].replace(",", ".");
								s[1] = df.format(imag);  s[1] = s[1].replace(",", ".");
								valor.alteraValorResultado(s[0]+"j"+s[1]);//""+String.format("%.4f", real).replace(",", ".")  + "j" + String.format("%.4f", imag).replace(",", ".") );
							}
						}
						else if(valor.contemCaractere(valor.retornaStringDigitada(),'j'))//operacao com retangular
						{
							if(PolRet)//converte para polar
							{
								nrs = valor.stringPolRet2Nrs(valor.retornaStringDigitada(), 'j');

								mod = op.modulo(nrs[0], nrs[1]);
								if(deg)
									ang = op.anguloPolDeg(nrs[0], nrs[1]);
								else
									ang = op.anguloPolRad(nrs[0], nrs[1]);

								s[0] = df.format(mod);  s[0] = s[0].replace(",", ".");
								s[1] = df.format(ang);  s[1] = s[1].replace(",", ".");

								valor.alteraValorResultado(s[0]+"<"+s[1]);//""+String.format("%.4f", mod).replace(",", ".")  + "<" + String.format("%.4f", ang).replace(",", ".") );
							}
						}
						else //demais valores que n�o imaginario -
						{
							if(tipoBase==0)//s� armazena n�meros na base decimal
							{
								if(!valor.stringVazia(valor.retornaStringDigitada()))
								{
									valor.armazenaValorPilha();
									valor.limpaValorDigitado();
								}
								else if(index!=0)
								{
									valor.alteraValorResultado(""+ valor.retornaValorPilha(0));
									valor.armazenaValorPilha();
									valor.limpaValorDigitado();
								}
							}
							else//corrigir o problema da base que nao deixa o enter funcionar
							{
								tipoBase = 0;
								btnDec.setText("10");  btnDec.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
								base = " - [ base decimal ]";
								textFieldAuxiliar.setText(aux + base);

								valor.alteraValorResultado("Base trocada para DECIMAL");

							}
						}
					}catch(NumberFormatException exc){
						erro = true;
						valor.alteraValorResultado("Entrada Inválida!");

					}
					printTela();
				}
			}
		});
		btnEnter.setFont(new Font("Arial", Font.PLAIN, 13));
		//------------------------------------------------------------------------------

		//------------------------------------------------------------------------------
		//X�2
		//------------------------------------------------------------------------------
		JButton btnX2 = new JButton("x\uu00B2");
		btnX2.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent arg0)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado;
				int index = valor.retornaIndexPilha();

				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(index!=0)
					{
						resultado =  op.multiplicacaoReais(valor.retornaValorPilha(0),valor.retornaValorPilha(0));
						valor.apagaValorPilha();
						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
					resultado = op.multiplicacaoReais(valor.retornaValorNumerico(),valor.retornaValorNumerico());
					valor.alteraValorResultado(""+resultado);
				}
				printTela();
			}
		});
		btnX2.setFont(new Font("Arial", Font.PLAIN, 13));
		//------------------------------------------------------------------------------

		panelTecladoPrincipal.setLayout(new GridLayout(0, 5, 0, 0));
		panelTecladoPrincipal.add(btnMaisMenos);
		panelTecladoPrincipal.add(btnInversao);
		panelTecladoPrincipal.add(btnSqrt);
		panelTecladoPrincipal.add(btnX2);
		//-------------------------------------------------------------------------

		//Simbolo para a conversao de base - Prefixo
		//------------------------------------------------------------
		JButton btnUndo = new JButton("base");
		btnUndo.setToolTipText("Prefixo para a convers\u00E3o de base.");
		btnUndo.addActionListener(new ActionListener()
		{
			private int caract=0;
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				if(caract==0)
				{
					valor.alteraValorResultado("b");
					caract=1;
				}
				else if(caract==1)
				{
					valor.alteraValorResultado("x");
					caract=2;
				}
				else if(caract==2)	//numero na base decimal nao tem prefixo
				{
					valor.limpaValorDigitado();
					caract=0;
				}
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btnUndo.setFont(new Font("Arial", Font.PLAIN, 13));
		//-------------------------------------------------------------

		panelTecladoPrincipal.add(btnUndo);
		panelTecladoPrincipal.add(btn7);
		panelTecladoPrincipal.add(btn8);
		panelTecladoPrincipal.add(btn9);
		panelTecladoPrincipal.add(btnDivisao);
		//---------------------------------------------------------------------

		//---------------------------------------------------------------------
		//DELETE - apaga uma linha da pilha
		//---------------------------------------------------------------------
		JButton btnDel = new JButton("DEL");
		btnDel.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				@SuppressWarnings("unused")
				int index = valor.retornaIndexPilha();

				erro = false;
				//-------------------------------------------------------------
				//correcao para apagar primeiro valor sem ENTER e a pilha
				//-------------------------------------------------------------
				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					valor.apagaValorPilha();
					index--;
				}
				else
				{
					textFieldLinha1.setText("");
					valor.limpaValorDigitado();
				}
				//-------------------------------------------------------------
				printTela();
				/*if(index>=1)
					textFieldLinha2.setText(""+valor.retornaValorPilha(0));
				else
					textFieldLinha2.setText("");
				if(index>=2)
					textFieldLinha3.setText(""+valor.retornaValorPilha(1));
				else
					textFieldLinha3.setText("");
				if(index>=3)
					textFieldLinha4.setText(""+valor.retornaValorPilha(2));
				else
					textFieldLinha4.setText("");
					*/
			}
		});
		btnDel.setFont(new Font("Arial", Font.PLAIN, 13));
		panelTecladoPrincipal.add(btnDel);
		panelTecladoPrincipal.add(btn4);
		panelTecladoPrincipal.add(btn5);
		panelTecladoPrincipal.add(btn6);
		panelTecladoPrincipal.add(btnMultiplicacao);
		panelTecladoPrincipal.add(btnUp);
		panelTecladoPrincipal.add(btn1);
		panelTecladoPrincipal.add(btn2);
		panelTecladoPrincipal.add(btn3);
		panelTecladoPrincipal.add(btnSubtracao);
		panelTecladoPrincipal.add(btnSwap);
		panelTecladoPrincipal.add(btn0);
		panelTecladoPrincipal.add(btnDot);
		panelTecladoPrincipal.add(btnExp);
		panelTecladoPrincipal.add(btnSoma);
		panelTecladoPrincipal.add(btnEnter);

		JPanel panelTecladoSecundario = new JPanel();
		panelTecladoSecundario.setBackground(new Color(220, 220, 220));
		panelTecladoSecundario.setBounds(14, 201, 430, 83);
		frmCalculadora.getContentPane().add(panelTecladoSecundario);
		panelTecladoSecundario.setLayout(new GridLayout(0, 5, 0, 0));

		//---------------------------------------------------------------
		//2nd FUNCAO
		//---------------------------------------------------------------
		JButton btn2nd = new JButton("2nd");
		btn2nd.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				hyp=true;

				if(SecFunc)
				{
					btnAb.setText("MDC");  btnAb.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnAb.setToolTipText("M�ximo Divisor Comum entre dois n�meros.");
					btnSin.setText("asin"); btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnCos.setText("acos"); btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnTan.setText("atan"); btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnYx.setText("\u02E3\u221Ay"); btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnLog.setText("10\u02E3"); btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnLn.setText("e\u02E3"); btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnSto.setText("MEM"); btnSto.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					btnSto.setToolTipText("Carrega nr [<A><MEM>]");
					btnPi.setText("\u03C0"); btnPi.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnPi.setToolTipText("Nr. PI");

					SecFunc = false;
				}
				else
				{
					btnAb.setText("cmp?"); btnAb.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
					btnYx.setText("y\u02E3");btnYx.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnLog.setText("log");btnLog.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnLn.setText("ln");  btnLn.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnSin.setText("sin");btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnCos.setText("cos");btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnTan.setText("tan");btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnSto.setText("STO"); btnSto.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnSto.setToolTipText("Armazena nr [<nr> <ENT><A><STO>], apaga nr [<A><\u00B1><STO>]");
					btnPi.setText("\u2328"); btnPi.setFont(new Font("Dialog", Font.PLAIN, 19));
					btnPi.setToolTipText("Relatório");
					
					SecFunc = true;
				}
			}
		});
		btn2nd.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btn2nd);
		btnPol.setToolTipText("Coordenadas Polares para Retangulares e vice-versa.");
		//----------------------------------------------------------------

		//JButton btnPol = new JButton("|M|\u2220");
		btnPol.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				if(PolRet)
				{
					btnRet.setText("j");btnRet.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
					btnRet.setVerticalAlignment(SwingConstants.CENTER);
					btnRet.setToolTipText("Simbolo para a escrita de numeros retangulares: real \u00b1 jX");
					PolRet = false;
				}
				else
				{
					btnRet.setText("\u22BE");btnRet.setFont(new Font("Lucida Grande", Font.PLAIN, 20));
					btnRet.setVerticalAlignment(SwingConstants.BOTTOM);
					btnRet.setToolTipText("Simbolo para a escrita de numeros polares: m\u00F3dulo < angulo");
					PolRet = true;
				}
			}
		});
		btnPol.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnPol);
		btnRet.setToolTipText("Simbolo para a escrita de numeros polares: m\u00F3dulo < angulo");

		btnRet.setVerticalAlignment(SwingConstants.BOTTOM);
		//JButton btnRet = new JButton("ang");
		btnRet.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				
				valor.dadosPilha2Copia();
				if(PolRet)
				{
					valor.alteraValorDigitado("<");
					textFieldLinha1.setText(valor.retornaStringDigitada());
				}
				else
				{
					valor.alteraValorDigitado("j");
					textFieldLinha1.setText(valor.retornaStringDigitada());
				}
			}
		});
		btnRet.setFont(new Font("Dialog", Font.PLAIN, 20));
		panelTecladoSecundario.add(btnRet);

		//---------------------------------------------------------
		//HIPERBOLE - depende da 2nd funcao
		//---------------------------------------------------------
		JButton btnHyp = new JButton("hyp");
		btnHyp.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				if(hyp)
				{
					if(SecFunc)
					{
						btnSin.setText("sinh");btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
						btnCos.setText("conh");btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
						btnTan.setText("tanh");btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					}
					else
					{
						btnSin.setText("asinh");btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 9));
						btnCos.setText("acosh");btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 9));
						btnTan.setText("atanh");btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 9));
					}
					hyp=false;
				}
				else
				{
					if(SecFunc)
					{
						btnSin.setText("sin");btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
						btnCos.setText("cos");btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
						btnTan.setText("tan");btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 13));

					}
					else
					{
						btnSin.setText("asin"); btnSin.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
						btnCos.setText("acos"); btnCos.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
						btnTan.setText("atan"); btnTan.setFont(new Font("Lucida Grande", Font.PLAIN, 12));
					}
					hyp=true;
				}
			}
		});
		btnHyp.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnHyp);

		btnSto.setToolTipText("Armazena nr [<nr> <ENT><A><STO>], apaga nr [<A><\u00B1><STO>]");
		//---------------------------------------------------------

		//ARMAZENA DADO NA MEM�RIA - A B C D E F
		//---------------------------------------------------------
		//JButton btnSto = new JButton("STO");
		btnSto.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
/*				double resultado;
				int index = valor.retornaIndexPilha();
				int posMem = 0;*/

				if(SecFunc)
				{
					if(!valor.retornaStringDigitada().isEmpty())//nao tem para linha 2 e 3
					{

						switch(valor.retornaStringDigitada().charAt(0))
						{
							case 'A': MEM[0]=valor.retornaValorPilha(0);
									aux = aux.substring(0,5) + "A" + aux.substring(6,17) + base;
									break;
							case 'B': MEM[1]=valor.retornaValorPilha(0);
								aux = aux.substring(0,7) + "B" + aux.substring(8,17)+ base;
								break;
							case 'C': MEM[2]=valor.retornaValorPilha(0);
								aux = aux.substring(0,9) + "C" + aux.substring(10,17)+ base;
								break;
							case 'D': MEM[3]=valor.retornaValorPilha(0);
								aux = aux.substring(0,11) + "D" + aux.substring(12,17)+ base;
								break;
							case 'E': MEM[4]=valor.retornaValorPilha(0);
								aux = aux.substring(0,13) + "E" + aux.substring(14,17)+ base;
								break;
							case 'F': MEM[5]=valor.retornaValorPilha(0);
								aux = aux.substring(0,15) + "F" + aux.substring(16,17)+ base;
								break;

							case '-'://apaga memoria
								switch(valor.retornaStringDigitada().charAt(1))
								{
									case 'A': MEM[0]=0;
										aux = aux.substring(0,5) + " " + aux.substring(6,17)+ base;
										break;
									case 'B': MEM[1]=0;
										aux = aux.substring(0,7) + " " + aux.substring(8,17)+ base;
										break;
									case 'C': MEM[2]=0;
										aux = aux.substring(0,9) + " " + aux.substring(10,17)+ base;
										break;
									case 'D': MEM[3]=0;
										aux = aux.substring(0,11) + " " + aux.substring(12,17)+ base;
										break;
									case 'E': MEM[4]=0;
										aux = aux.substring(0,13) + " " + aux.substring(14,17)+ base;
										break;
									case 'F': MEM[5]=0;
										aux = aux.substring(0,15) + " " + aux.substring(16,17)+ base;
										break;
								}


							default:textFieldAuxiliar.setText("ERRO");
						}
						textFieldAuxiliar.setText(aux);
						valor.apagaValorPilha();
						valor.limpaValorDigitado();
					}

				}
				else
				{
					switch(valor.retornaStringDigitada().charAt(0))
					{
						case 'A': valor.alteraValorResultado("" + MEM[0]); break;
						case 'B': valor.alteraValorResultado("" + MEM[1]); break;
						case 'C': valor.alteraValorResultado("" + MEM[2]); break;
						case 'D': valor.alteraValorResultado("" + MEM[3]); break;
						case 'E': valor.alteraValorResultado("" + MEM[4]); break;
						case 'F': valor.alteraValorResultado("" + MEM[5]); break;

					}


				}

				printTela();
			}
		});
		btnSto.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnSto);
		btnPi.setToolTipText("Relatório");

		//---------------------------------------------------------
		//PI
		//---------------------------------------------------------
		//JButton btnPi = new JButton("\u03C0");
		btnPi.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				if(SecFunc)
				{
				
					valor.alteraValorResultado(""+ Math.PI);
					textFieldLinha1.setText(valor.retornaStringDigitada());
				}
				else//imprime relatorio
				{
					
					//relatorio	
					
	
					
				}
			}

		});
		btnPi.setFont(new Font("Dialog", Font.PLAIN, 19));
		panelTecladoSecundario.add(btnPi);
		//----------------------------------------------------------

		//HIPOTENUSA
		//----------------------------------------------------------
		JButton btnHms = new JButton("h \u22BF");
		btnHms.setToolTipText("hipotenusa");
		btnHms.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				double resultado;
				int index = valor.retornaIndexPilha();

				if(valor.stringVazia(valor.retornaStringDigitada()))
				{
					if(index>1)
					{
						resultado =  op.hipotenusa(valor.retornaValorPilha(0),valor.retornaValorPilha(1));
						valor.corrige2PilhaOperacao();
						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
						resultado = op.hipotenusa(valor.retornaValorNumerico(), valor.retornaValorPilha(0));
						valor.apagaValorPilha();
						valor.alteraValorResultado(""+resultado);
				}
				printTela();
			}
		});
		btnHms.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnHms);
		//------------------------------------------------------------

		//------------------------------------------------------------
		//SIN - ASIN - SINH - ASINH
		//------------------------------------------------------------
		//JButton btnSin = new JButton("sin");
		btnSin.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado, angulo;
				int index = valor.retornaIndexPilha();

				if(SecFunc)
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)//hyp true ainda nao foi pressionado - 2nd func sempre retorna hyp para true
								resultado =  op.seno(angulo);
							else
								resultado = op.senoh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);//String.format("%.5f", resultado).replace(",", "."));
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.seno(angulo);
						else
							resultado = op.senoh(angulo);

						valor.alteraValorResultado(""+resultado);//String.format("%.5f", resultado).replace(",", "."));
					}
				}
				else
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)
								resultado =  op.aseno(angulo);
							else
								resultado =  op.asenoh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);//String.format("%.5f", resultado).replace(",", "."));
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.aseno(angulo);
						else
							resultado = op.asenoh(angulo);

						valor.alteraValorResultado(""+resultado);//String.format("%.5f", resultado).replace(",", "."));
					}
				}
				printTela();
			}
		});
		btnSin.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnSin);
		//--------------------------------------------------------------

		//COS - ACOS  - COSH - ACOSH
		//--------------------------------------------------------------
		//JButton btnCos = new JButton("cos");
		btnCos.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado, angulo;
				int index = valor.retornaIndexPilha();

				if(SecFunc)
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)//hyp true ainda nao foi pressionado - 2nd func sempre retorna hyp para true
								resultado =  op.cos(angulo);
							else
								resultado = op.cosh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.cos(angulo);
						else
							resultado = op.cosh(angulo);

						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)
								resultado =  op.acos(angulo);
							else
								resultado =  op.acosh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.acos(angulo);
						else
							resultado = op.acosh(angulo);

						valor.alteraValorResultado(""+resultado);
					}
				}
				printTela();
			}
		});
		btnCos.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnCos);
		//---------------------------------------------------------------

		//TAN - ATAN - TANH - ATANH
		//JButton btnTan = new JButton("tan");
		btnTan.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado, angulo;
				int index = valor.retornaIndexPilha();

				if(SecFunc)
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)//hyp true ainda nao foi pressionado - 2nd func sempre retorna hyp para true
								resultado =  op.tan(angulo);
							else
								resultado = op.tanh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.tan(angulo);
						else
							resultado = op.tanh(angulo);

						valor.alteraValorResultado(""+resultado);
					}
				}
				else
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							angulo = valor.retornaValorPilha(0);

							if(deg)
								angulo = op.deg2rad(valor.retornaValorPilha(0));

							if(hyp)
								resultado =  op.atan(angulo);
							else
								resultado =  op.atanh(angulo);

							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);
						}
					}
					else
					{
						angulo = valor.retornaValorNumerico();

						if(deg)
							angulo = op.deg2rad(valor.retornaValorNumerico());

						if(hyp)
							resultado = op.atan(angulo);
						else
							resultado = op.atanh(angulo);

						valor.alteraValorResultado(""+resultado);
					}
				}
				printTela();
			}
		});
		btnTan.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnTan);

		//------------------------------------------------------------
		//RAND
		//------------------------------------------------------------
		JButton btnRand = new JButton("rand");
		btnRand.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				double x = Math.random();
				String s = String.format("%.3f", x).replace(",", ".") ;
				valor.alteraValorResultado(s);
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		btnRand.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnRand);
		btnAb.setToolTipText("Decomp\u00F5e um nr.  inteiro em seus fatores.");
		//-------------------------------------------------------------

		//COMPOSTO ou MDC
		//-------------------------------------------------------------
		//JButton btnAb = new JButton("cmp?);
		btnAb.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				if(SecFunc)//se composto de um nr inteiro
				{
					String resultado;
					int index = valor.retornaIndexPilha();

					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index!=0)
						{
							resultado =  op.fatores((int)valor.retornaValorPilha(0));
							valor.apagaValorPilha();
							valor.alteraValorResultado(""+resultado);
						}
					}
					else
					{
						resultado = op.fatores((int)valor.retornaValorNumerico());
						valor.alteraValorResultado(""+resultado);
					}
				}
				else //mdc
				{
					double result;
					int index = valor.retornaIndexPilha();

					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(index>1)
						{
							result =  op.mdc((int)valor.retornaValorPilha(0),(int)valor.retornaValorPilha(1));
							valor.corrige2PilhaOperacao();
							valor.alteraValorResultado(""+result);
						}
					}
					else
					{
							result= op.mdc((int)valor.retornaValorNumerico(), (int)valor.retornaValorPilha(0));
							valor.apagaValorPilha();
							valor.alteraValorResultado(""+result);
					}
				}
				printTela();
			}
		});
		btnAb.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnAb);

		//--------------------------------------------------------------------
		//Y^X ou x raiz Y
		//--------------------------------------------------------------------
		//JButton btnYx = new JButton("y\u02E3");
		btnYx.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado = 0;
				int index = valor.retornaIndexPilha();

				if(SecFunc)//2nd funcao sem ativacao
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))//linha 2 e 3
					{
						if(index>1)
						{
							resultado = op.powReais(valor.retornaValorPilha(0), valor.retornaValorPilha(1));
							valor.alteraValorResultado(""+ resultado);
							valor.corrige2PilhaOperacao();
						}
					}
					else if(index!=0)//linha 1 e 2
					{
						resultado = op.powReais(valor.retornaValorNumerico(), valor.retornaValorPilha(0));
						valor.alteraValorResultado(""+ resultado);
						valor.apagaValorPilha();
					}
				}
				else	//2nd funcao ativada
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))//linha 2 e 3
					{
						if(index>1)
						{
							resultado = op.rootYXReais(valor.retornaValorPilha(0), valor.retornaValorPilha(1));
							valor.alteraValorResultado(""+ resultado);
							valor.corrige2PilhaOperacao();
						}
					}
					else if(index!=0)//linha 1 e 2
					{
						resultado = op.rootYXReais(valor.retornaValorNumerico(), valor.retornaValorPilha(0));
						valor.alteraValorResultado(""+ resultado);
						valor.apagaValorPilha();
					}
				}
				printTela();
			}
		});
		btnYx.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnYx);
		//--------------------------------------------------------------------

		//--------------------------------------------------------------------
		//LOG ou 10�x
		//--------------------------------------------------------------------
		//JButton btnLog = new JButton("log");
		btnLog.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado = 0;
				int index = valor.retornaIndexPilha();

				if(SecFunc)//2nd funcao sem ativacao
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(valor.retornaValorPilha(0)!=0)
						{
							resultado =  op.log10(valor.retornaValorPilha(0));
							valor.alteraValorResultado(""+resultado);
						}
						else if(index!=0)
							valor.alteraValorResultado("ERRO - valor infinito!");
						valor.apagaValorPilha();
					}
					else
					{
						if(valor.retornaValorNumerico()!=0)
						{
							resultado = op.log10(valor.retornaValorNumerico());
							valor.alteraValorResultado(""+resultado);
						}
						else
						{
							valor.alteraValorResultado("ERRO - valor infinito!");
						}
					}
				}
				else	//2nd funcao ativada
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						resultado =  op.pow10(valor.retornaValorPilha(0));
						valor.alteraValorResultado(""+resultado);
						valor.apagaValorPilha();
					}
					else
					{
						resultado = op.pow10(valor.retornaValorNumerico());
						valor.alteraValorResultado(""+resultado);
					}
				}
				printTela();
			}
		});
		btnLog.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnLog);
		//--------------------------------------------------------------------

		//LN ou e�x
		//JButton btnLn = new JButton("ln");
		btnLn.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				valor.dadosPilha2Copia();
				entraDigito=false;
				
				double resultado = 0;
				int index = valor.retornaIndexPilha();

				if(SecFunc)//2nd funcao sem ativacao
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						if(valor.retornaValorPilha(0)!=0)
						{
							resultado =  op.ln(valor.retornaValorPilha(0));
							valor.alteraValorResultado(""+resultado);
						}
						else if(index!=0)
							valor.alteraValorResultado("ERRO - valor infinito!");
						valor.apagaValorPilha();
					}
					else
					{
						if(valor.retornaValorNumerico()!=0)
						{
							resultado = op.ln(valor.retornaValorNumerico());
							valor.alteraValorResultado(""+resultado);
						}
						else
						{
							valor.alteraValorResultado("ERRO - valor infinito!");
						}
					}
				}
				else	//2nd funcao ativada
				{
					if(valor.stringVazia(valor.retornaStringDigitada()))
					{
						resultado =  op.powe(valor.retornaValorPilha(0));
						valor.alteraValorResultado(""+resultado);
						valor.apagaValorPilha();
					}
					else
					{
						resultado = op.powe(valor.retornaValorNumerico());
						valor.alteraValorResultado(""+resultado);
					}
				}
				printTela();
			}
		});
		btnLn.setFont(new Font("Dialog", Font.PLAIN, 13));
		panelTecladoSecundario.add(btnLn);
		//---------------------------------------------------------------------

		JPanel panelTecladoHexa = new JPanel();
		panelTecladoHexa.setBounds(14, 172, 430, 30);
		frmCalculadora.getContentPane().add(panelTecladoHexa);
		panelTecladoHexa.setLayout(new GridLayout(0, 6, 0, 0));

		//---------------------------------------------------------
		JButton btnA = new JButton("A");
		btnA.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("A");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		panelTecladoHexa.add(btnA);
		//---------------------------------------------------------

		JButton btnB = new JButton("B");
		btnB.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("B");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		panelTecladoHexa.add(btnB);
		//----------------------------------------------------------

		JButton btnC = new JButton("C");
		btnC.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent arg0)
			{
				entraDigito=true;
				valor.alteraValorDigitado("C");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		panelTecladoHexa.add(btnC);
		//----------------------------------------------------------
		JButton btnD = new JButton("D");
		btnD.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("D");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		panelTecladoHexa.add(btnD);

		//---------------------------------------------------------
		JButton btnE = new JButton("E");
		btnE.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("E");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});
		panelTecladoHexa.add(btnE);
		//----------------------------------------------------------
		JButton btnF = new JButton("F");
		panelTecladoHexa.add(btnF);
		btnF.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				entraDigito=true;
				valor.alteraValorDigitado("F");
				textFieldLinha1.setText(valor.retornaStringDigitada());
			}
		});

		textFieldLinha1 = new JTextField();
		textFieldLinha1.setBounds(14, 138, 430, 32);
		textFieldLinha1.setHorizontalAlignment(SwingConstants.RIGHT);
		textFieldLinha1.setFont(new Font("Lucida Grande", Font.PLAIN, 14));
		frmCalculadora.getContentPane().add(textFieldLinha1);
		textFieldLinha1.setColumns(10);

		textFieldLinha2 = new JTextField();
		textFieldLinha2.setHorizontalAlignment(SwingConstants.RIGHT);
		textFieldLinha2.setFont(new Font("Lucida Grande", Font.PLAIN, 14));
		textFieldLinha2.setColumns(10);
		textFieldLinha2.setBounds(14, 111, 430, 32);
		frmCalculadora.getContentPane().add(textFieldLinha2);

		textFieldLinha3 = new JTextField();
		textFieldLinha3.setHorizontalAlignment(SwingConstants.RIGHT);
		textFieldLinha3.setFont(new Font("Lucida Grande", Font.PLAIN, 14));
		textFieldLinha3.setColumns(10);
		textFieldLinha3.setBounds(14, 84, 430, 32);
		frmCalculadora.getContentPane().add(textFieldLinha3);

		textFieldLinha4 = new JTextField();
		textFieldLinha4.setHorizontalAlignment(SwingConstants.RIGHT);
		textFieldLinha4.setFont(new Font("Lucida Grande", Font.PLAIN, 14));
		textFieldLinha4.setColumns(10);
		textFieldLinha4.setBounds(14, 57, 430, 32);
		frmCalculadora.getContentPane().add(textFieldLinha4);

		textFieldAuxiliar = new JTextField();
		textFieldAuxiliar.setText("Mem [           ] - [ base decimal ]");
		textFieldAuxiliar.setEditable(false);
		textFieldAuxiliar.setHorizontalAlignment(SwingConstants.LEFT);
		textFieldAuxiliar.setFont(new Font("Lucida Grande", Font.PLAIN, 9));
		textFieldAuxiliar.setColumns(10);
		textFieldAuxiliar.setBounds(14, 39, 430, 16);
		frmCalculadora.getContentPane().add(textFieldAuxiliar);

		//------------------------------------------------------------------
		//DEG-RAD
		//------------------------------------------------------------------
		//JButton btnDeg = new JButton("DEG");
		btnDeg.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent arg0)
			{
				if(deg)
				{
					btnDeg.setText("RAD");  btnDeg.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
					deg = false;
				}
				else
				{
					btnDeg.setText("DEG");  btnDeg.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
					deg = true;
				}

			}
		});
		btnDeg.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
		btnDeg.setBounds(292, 12, 69, 18);
		frmCalculadora.getContentPane().add(btnDeg);
		//-------------------------------------------------------------------

		//DEC - BIN - HEX
		//-------------------------------------------------------------------
		//JButton btnDec = new JButton("dec");
		btnDec.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				try{
					if(tipoBase==2)
						tipoBase=0;
					else
						tipoBase++;

					if(tipoBase==0)
					{
						btnDec.setText("10");  btnDec.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
						base = " - [ base decimal ]";
					}
					else if(tipoBase==1)
					{
						btnDec.setText("2");  btnDec.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
						base = " - [ base bin�ria ]";
					}
					else
					{
						btnDec.setText("16");  btnDec.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
						base = " - [ base hexadecimal ]";
					}
					textFieldAuxiliar.setText(aux + base);

					if(!valor.retornaStringDigitada().isEmpty())
					{
						switch(tipoBase)
						{
							case 0: valor.alteraValorResultado(valor.trocaBase(valor.retornaStringDigitada(), 0)); break;//converte para dec

							case 1: valor.alteraValorResultado(valor.trocaBase(valor.retornaStringDigitada(), 1)); break;//converte para bin

							case 2: valor.alteraValorResultado(valor.trocaBase(valor.retornaStringDigitada(), 2)); break;//converte para hex

							default: valor.alteraValorResultado("Erro convers�o");
						}
						textFieldLinha1.setText(valor.retornaStringDigitada());
					}
				}catch(NumberFormatException exc){
					erro = true;
					valor.alteraValorResultado("Entrada Inv�lida!");

				}



			}
		});
		btnDec.setFont(new Font("Lucida Grande", Font.PLAIN, 10));
		btnDec.setBounds(365, 12, 69, 18);
		frmCalculadora.getContentPane().add(btnDec);
		//-------------------------------------------------------------------

		JLabel lblRpn = new JLabel("RPN");
		lblRpn.setFont(new Font("Lucida Grande", Font.PLAIN, 13));
		lblRpn.setBounds(180, 12, 32, 16);
		frmCalculadora.getContentPane().add(lblRpn);

		JLabel lblCaveira = new JLabel("\u2620");
		lblCaveira.setFont(new Font("Lucida Grande", Font.PLAIN, 36));
		lblCaveira.setBounds(18, 4, 27, 32);
		frmCalculadora.getContentPane().add(lblCaveira);

	}
}



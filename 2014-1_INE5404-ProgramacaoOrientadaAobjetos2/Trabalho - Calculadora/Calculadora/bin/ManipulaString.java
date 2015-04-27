package bin;
public class ManipulaString
{
	private String stringValor="";
	private double[] valorPilha = new double[100];
	private double[] copiaValorPilha = new double[100];
	private String copiaStringValor="";
	private int indexPilha=0;
	private int copiaIndexPilha=0;
	
	private String[] stringPilha = new String[100];	
	private int indexStringPilha=0;
	
	public ManipulaString(){ }
	
	//---------------------------------------------
	boolean contemCaractere(String s, char c)
	{
		int tam = s.length();
		
		for(int i=0; i< tam; i++)
		{
			char a = s.charAt(i);
			if(a==c)
				return true;
		}
		return false;
	}	
	//----------------------------------------------
	void alteraValorDigitado(String s)
	{
		stringValor = stringValor + s;
	}
	//----------------------------------------------
	void limpaValorDigitado()
	{
			stringValor = "";
	}
	//----------------------------------------------
	void alteraValorResultado(String s)//altera o valor digitado com limpeza
	{
		this.limpaValorDigitado();
		stringValor = s;
	}
	//-----------------------------------------------
	String retornaStringDigitada()
	{
		return stringValor;
	}
	//----------------------------------------------
	double retornaValorNumerico()
	{
		if(this.stringVazia(stringValor))
			return 0;
		return Double.parseDouble(stringValor);
	}
	//---------------------------------------------
	void armazenaValorPilha()
	{
		for(int i=indexPilha; i>=0; i--)
			valorPilha[i+1]=valorPilha[i];		
		
		valorPilha[0] = this.retornaValorNumerico();
		indexPilha++;
	}
	//--------------------------------------------
	void apagaValorPilha()
	{
		for(int i=0; i<= indexPilha; i++)
			valorPilha[i]=valorPilha[i+1];
		
		if(indexPilha!=0)
			indexPilha--;
	}
	//--------------------------------------------
	void alteraValorPilha(int i, double valor)
	{
		valorPilha[i]=valor;
	}
	//---------------------------------------------
	double retornaValorPilha(int i)
	{
		return valorPilha[i];
	}
	//--------------------------------------------
	int retornaIndexPilha()
	{
		return indexPilha;
	}
	//--------------------------------------------
	void corrige2PilhaOperacao()
	{
		this.apagaValorPilha();
		this.apagaValorPilha();
	}
	//--------------------------------------------
	void dadosPilha2Copia()
	{
		copiaStringValor = stringValor;
		copiaIndexPilha = indexPilha;
		
		for(int i=0; i<100; i++)
			copiaValorPilha[i]=valorPilha[i];
	}
	//--------------------------------------------
	void Copia2dadosPilha()
	{
		stringValor=copiaStringValor;
		indexPilha = copiaIndexPilha;
		
		for(int i=0; i<100; i++)
			valorPilha[i]=copiaValorPilha[i];
	}	
	//--------------------------------------------
	void apagaCaractere()//exclui ultimo caractere da stringValor
	{
		int tam = stringValor.length() ;
		
		if(tam>0)
			stringValor = (stringValor.substring(0, stringValor.length() - 1));
	}
	
	//---------------------------------------------
	//s� para trabalho com nrs complexos - a pilha armazena String
	//---------------------------------------------
	/*void armazenaStringPilha()
	{
		for(int i=indexStringPilha; i>=0; i--)
			stringPilha[i+1]=stringPilha[i];		
		
		stringPilha[0] = stringValor;
		indexStringPilha++;
	}
	//--------------------------------------------
	void apagaStringPilha()
	{
		for(int i=0; i<= indexStringPilha; i++)
			stringPilha[i]=stringPilha[i+1];
		
		if(indexStringPilha!=0)
			indexStringPilha--;
	}
	//--------------------------------------------
	void alteraStringPilha(int i, String valor)
	{
		stringPilha[i]=valor;
	}
	//---------------------------------------------
	String retornaStringPilha(int i)
	{
		return stringPilha[i];
	}
	//--------------------------------------------
	int retornaIndexStringPilha()
	{
		return indexStringPilha;
	}
	//--------------------------------------------
	void corrige2StringPilhaOperacao()
	{
		this.apagaStringPilha();
		this.apagaStringPilha();
	}
	*/
	//--------------------------------------------
	//--------------------------------------------
	public boolean stringVazia(String texto) //S� USAR objeto.isEmpty() no lugar desta fun��o doida.
	{
	      try // pega a primeira letra da palavra. se conseguir, eh porque a string n�o tah vazia.
	      {   // nesse caso retorna false.
	         texto.charAt(0);
	         return false;
	      }catch (NullPointerException e){ // se o texto eh null, retorna true
	    	  return true;
	      }catch (StringIndexOutOfBoundsException e){// se o texto eh "", retorna true
	    	  return true;
	      }
	  }
	//--------------------------------------------------------------
	public int nrCaracteresIguais(String s, char caract)	//retorna o nr de carecteres iguais em uma String
	{
		int cont=0;
		
		for(int j=0; j<s.length(); j++)
		{
			
			if(s.charAt(j)==caract)
				cont++;
		}
		return cont;
	}
	
	//---------------------------------------------------------------
	String retNrsComExp(String nrs)//coloca expoentes nos numeros repetidos em um sequencia continua de numeros
	{							  //S� FUNCIONA PARA NUMEROS COM 1 DIGITO!!!
		int tam = nrs.length();
		int k=0;
		String result = "";
		
    	for(int i=0; i<tam;)
    	{
    		char a = nrs.charAt(i);
    		k = this.nrCaracteresIguais(nrs,a);
    		
    		if(k>1)
    		{
    			if(result.isEmpty())//so para nao imprimir o x no primeiro valor
    				result = result +  a + "\u02C4" + k;
    			else
    				result = result +  "x" + a + "\u02C4" + k;
    			i = i+k;
    			
    		}
    		else
    		{
    			if(result.isEmpty())//so para nao imprimir o x no primeiro valor
    				result = result + nrs.charAt(i);
    			else
    				result = result + "x"+ nrs.charAt(i);
    			i++;
    			
    		}k=0;
    	}
				
		return result;
	}
	//------------------------------------------------------------------------
	public String toBase10(String s)
	{
		char firstCaract = s.charAt(0);
		OperacoesMat op = new OperacoesMat();
		String aux = s.substring(1,s.length());//elimina o primeiro caractere
		
		if(!Character.isLetter(firstCaract))//se esta na base decimal nao altera
			return s;
					
		switch(firstCaract)
		{
			case 'b': return "" + op.bin2dec(aux);
			case 'x': return "" + op.hex2dec(aux);
			default: return "Erro conv. toBase10";
		}	
	}
	//------------------------------------------------------------------------
	public String toBase2(String s)
	{
		char firstCaract = s.charAt(0);
		String aux = s.substring(1,s.length());//elimina o primeiro caractere
		
		OperacoesMat op = new OperacoesMat();
		
		if(!Character.isLetter(s.charAt(0)))
			return "b" + op.dec2bin(Integer.parseInt(s));
				
		switch(firstCaract)
		{
			case 'b': return s;			//binario para binario
			case 'x': return "b" + op.hex2dec(aux);
			default: return "Erro conv. toBase2";
		}	
	}
	//------------------------------------------------------------------------
	public String toBase16(String s)
	{
		char firstCaract = s.charAt(0);
		String aux = s.substring(1,s.length());//elimina o primeiro caractere
		
		OperacoesMat op = new OperacoesMat();
		
		if(!Character.isLetter(s.charAt(0)))
			return "x"+ op.dec2hex(Integer.parseInt(s));
				
		switch(firstCaract)
		{
			case 'b': return "x" + op.bin2hex(aux);			//binario para binario
			case 'x': return s;
			default: return "Erro conv. toBase16";
		}	
	}
	//-------------------------------------------------------------------------
	public String trocaBase(String nr, int base)//base = 0 dec, 1 = bin e 2 = hex
	{
		switch(base)
		{
			case 0: return this.toBase10(nr);
				
			case 1: return this.toBase2(nr);
				
			case 2: return this.toBase16(nr);
			
			default: return "Erro troca de base";
		}
	}
	//--------------------------------------------------------------------------
	public double[] stringPolRet2Nrs(String s, char x)//separa os dois nrs separados pelo simbolo x
	{
		String[] nrs = {"", ""};
		double[] NRS = new double[2];
		int tam = s.length();
		int i=0;
		
		do
		{
			nrs[0] = nrs[0] + s.charAt(i);
			i++;
			
		}while(s.charAt(i)!=x);
			
		for(int k=i+1; k<tam; k++)
		{
			nrs[1]=nrs[1]+s.charAt(k);
		}
		NRS[0] = Double.parseDouble(nrs[0]);
		NRS[1] = Double.parseDouble(nrs[1]);
		
		return NRS;
	}
	//-----------------------------------------------------------------------------------
	
	
	/*public boolean stringValida(String s)
	{
		//regras
		//
		
		return true;
	}*/
}
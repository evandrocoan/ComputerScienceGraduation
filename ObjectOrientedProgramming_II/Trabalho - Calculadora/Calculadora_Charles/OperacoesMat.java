import java.util.Scanner;

public class OperacoesMat 
{
	OperacoesMat(){}
	//------------------------------------------------
	double somaReais(double x, double y)
	{
		return x+y;
	}
	//------------------------------------------------
	double subtracaoReais(double x, double y)
	{
		return x-y;
	}
	//------------------------------------------------
	double multiplicacaoReais(double x, double y)
	{
		return x*y;
	}
	//------------------------------------------------
	double divisaoReais(double x, double y)
	{
		if(y!=0)
			return x/y;
		return 0;
	}
	//------------------------------------------------
	double sqrtReais(double x)
	{
		return Math.sqrt(x);
	}
	//------------------------------------------------
	double powReais(double y, double x)//yöx
	{
		return Math.pow(y,x);
	}
	//------------------------------------------------
	double rootYXReais(double y, double x)//yö(1/x) = x raiz y
	{
		if(x==0)
			x=1;
		return Math.pow(y, 1/x);
	}
	//------------------------------------------------
	double log10(double x)
	{
		//if(x<0)
		//	x = -x;
		return Math.log10(x);
	}
	//------------------------------------------------
	double ln(double x)
	{
		return Math.log(x);
	}
	//------------------------------------------------
	double pow10(double x)
	{
		return Math.pow(10,x);
	}
	//------------------------------------------------
	double powe(double x)
	{
		return Math.pow(Math.E,x);
	}
	//------------------------------------------------

    double deg2rad(double deg)	//graus em Radianos 0-2Pi
    {
    	return deg*Math.PI/180;
    }
	//------------------------------------------------
    double rad2deg(double rad)
    {
    	return rad*180/Math.PI;
    }
    //------------------------------------------------
    double seno(double ang)
    {
    	return Math.sin(ang);
    }
    //-------------------------------------------------
    double aseno(double ang)
    {
    	return Math.asin(ang);
    }
    //------------------------------------------------
    double senoh(double ang)
    {
    	return Math.sinh(ang);
    }
    //------------------------------------------------
    double asenoh(double x) 
    { 
    	return Math.log(x + Math.sqrt(x*x + 1.0)); 
    } 
    //------------------------------------------------
    //------------------------------------------------
    double cos(double ang)
    {
    	return Math.cos(ang);
    }
    //-------------------------------------------------
    double acos(double ang)
    {
    	return Math.acos(ang);
    }
    //------------------------------------------------
    double cosh(double ang)
    {
    	return Math.cosh(ang);
    }
    //-------------------------------------------------
    double acosh(double x) 
    { 
    	return Math.log(x + Math.sqrt(x*x - 1.0)); 
    } 
    //------------------------------------------------
    //------------------------------------------------
    double tan(double ang)
    {
    	return Math.tan(ang);
    }
    //-------------------------------------------------
    double atan(double ang)
    {
    	return Math.atan(ang);
    }
    //------------------------------------------------
    double tanh(double ang)
    {
    	return Math.tanh(ang);
    }
    //------------------------------------------------
    double atanh(double x) 
    { 
    	return 0.5*Math.log( (x + 1.0) / (x - 1.0) ); 
    } 
    //------------------------------------------------
    //Determina se Ž primo
    String fatores(int nr)	//fatores de um determinado nr.
    {
    	String result = "";
    	int nr0 = nr/2;
    	int d = 2;
    	int r=0;


    	if (nr==1)
    	    return "primo "+ nr;
    	else
    	{ 
    	    while (d<= nr0)	//so divide ate a metade
    	    {    
    	        r = nr%d;
    	    	if (r!=0)
    	        {
    	            if(d<3)  //para o inicio com 2 e 3 depois nao divide pelos impares
    	                d = d+1;
    	            else
    	                d = d+2; //so divide pelos impares se resto !=0
    	        }
    	        else
    	        {
    	        	if(result.isEmpty())//so para nao imprimir o x no primeiro valor
        				result = result +  d;
        			else
        				result = result + "x" + d;
    	            nr = nr/d;
    	        }
    	    }
    	    if(nr>nr0)
    	    	return "primo "+ nr;   	  
    	}
     	
    	return result;
    }
    //-------------------------------------------------------------------
    int mdc(int a, int b)
    {
    	while(a!=b)
    	{
    		if(a>b)
    			a = a - b;
    		else
    			b = b - a;
    	}
    	return a;
    }
    //--------------------------------------------------------------------
    //CONVERSAO DEC - BIN
    public String dec2bin(int nr)
    {  	
    	return Integer.toBinaryString(nr);
    } 
    //CONVERSAO BIN - DEC
    public int bin2dec(String nr)
    {
    	return Integer.parseInt(nr, 2);
    }
    //--------------------------------------------------------------------
    //CONVERSAO DEC - HEX
    public String dec2hex(int nr)
    {  	
    	return Integer.toHexString(nr);
    }    
    //CONVERSAO HEX - DEC
    public int hex2dec(String nr)
    {  	
    	return Integer.parseInt(nr, 16);
    } 
    //--------------------------------------------------------------------
    //CONVERSAO HEX - BIN
    public String hex2bin(String s)
    {
    	return this.dec2bin(hex2dec(s));
    }
    //--------------------------------------------------------------------
    //CONVERSAO BIN - HEX
    public String bin2hex(String s)
    {
    	return  this.dec2hex(bin2dec(s));
    }
    //-------------------------------------------------------------------
    public double hipotenusa(double x, double y)
    {
    	return Math.hypot(x, y);
    }
    //------------------------------------------------------------------
    //NR. Complexos
    public double anguloPolRad(double real, double imag)
    {
    	return Math.atan2(imag,real);
    }
    public double anguloPolDeg(double real, double imag)
    {
    	return this.rad2deg(this.anguloPolRad(real,imag));
    }
    public double modulo(double real, double imag)
    {
    	return Math.hypot(real, imag);
    }
    public double real(double mod, double ang)
    {
		return mod*Math.cos(ang);//real

    }
    public double imaginario(double mod, double ang)
    {
		return mod*Math.sin(ang);//imaginario

    }
    //---------------------------------------------------------------------
}		

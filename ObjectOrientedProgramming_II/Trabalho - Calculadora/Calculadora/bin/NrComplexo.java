package bin;
public class NrComplexo extends OperacoesMat
{
	double[] nrCR = new double[2];//nrC[0] = real, nrC[1]=imaginario
	double[] nrCP = new double[2];//nrCP[0] =  m�dulo, nrCP[1] = angulo

	public NrComplexo(double vR, double vI)
	{
		nrCR[0] = vR;
		nrCR[1] = vI;

		nrCP = this.converteRet2Polar(nrCR);

	}
	//----------------------------------------------------
	//M�todos de Acesso
	//----------------------------------------------------
	public void altereReal(double vR)
	{
		nrCR[0] = vR;
	}
	//----------------------------------------------------
	public void altereImaginario(double vI)
	{
		nrCR[1] = vI;
	}
	//----------------------------------------------------
	public double fornecaReal()
	{
		return nrCR[0];
	}
	//----------------------------------------------------
	public double fornecaImaginario()
	{
		return nrCR[1];
	}
	//----------------------------------------------------
	public double fornecaModulo()
	{
		return nrCP[0];
	}
	//----------------------------------------------------
	public double fornecaAngulo()
	{
		return nrCP[1];
	}
	//----------------------------------------------------
	public NrComplexo fornecaConjugado()
	{
		return new NrComplexo(nrCR[0], -1*nrCR[1]);
	}
	//****************************************************
	public double[] somaRetangular(NrComplexo x)
	{
		double[] y = new double[2];

		y[0] = nrCR[0] + x.fornecaReal();
		y[1] = nrCR[1] + x.fornecaImaginario();

		return y;
	}
	//----------------------------------------------------
	public double[] subtraiRetangular(NrComplexo x)
	{
		double[] y = new double[2];

		y[0] = nrCR[0] - x.fornecaReal();
		y[1] = nrCR[1] - x.fornecaImaginario();

		return y;
	}
	//----------------------------------------------------
	public double[] converteRet2Polar(double[] x)//x[0] = real, x[1]= imaginario
	{
		double[] xP = new double[2];

		xP[0] = Math.sqrt((x[0]*x[0]) + (x[1]*x[1]));//m�dulo

		if(xP[0]==0)								//para evitar a divis�o por zero
			xP[0]=0.0000000001;

		//�ngulo
		if(x[0]>=0) 							 	//quadrantes I e IV
			xP[1] = Math.asin(x[1]/xP[0]);			//arco seno (CO/H)
		else
		{										 	//quadrante II
			xP[1] = Math.acos(x[0]/xP[0]);			//arco cosseno (CA/H)

			if(x[1]<0) 						 		//quadrante III
				xP[1] = -1*xP[1];			 		//inverte angulo
		}
		return xP;
	}
	//----------------------------------------------------
	public double[] convertePolar2Ret(double[] x)
	{
		double[] xR = new double[2];

		xR[0]= x[0]*Math.cos(x[1]);//real
		xR[1]= x[0]*Math.sin(x[1]);//imaginario

		return xR;
	}
	//-----------------------------------------------------
	public double[] multiplicaPolar(NrComplexo x)
	{
		double[] y = new double[2];

		y[0] = nrCP[0] * x.fornecaModulo();
		y[1] = nrCP[1] + x.fornecaAngulo();

		return y;
	}
	//-----------------------------------------------------
	public double[] dividePolar(NrComplexo x)
	{
		double[] y = new double[2];

		y[0] = nrCP[0]/ x.fornecaModulo();
		y[1] = nrCP[1] - x.fornecaAngulo();

		return y;
	}
	//-----------------------------------------------------
	public String toString()
    {
    	String aux = "";
    	double x = nrCR[1];

		if(nrCR[1]>=0)
    		aux=" +j";
		else
		{	aux=" -j";
			x = -1*x;//tirar o sinal, pois j� esta em j
		}

    	return "RETANGULAR: " + String.format("%.2f", nrCR[0]) + aux + String.format("%.2f", x)+
    		   "\nPOLAR: " + String.format("%.2f", nrCP[0]) + " < " + String.format("%.2f", nrCP[1]);
    }
	//---------------------------------------------------------
}
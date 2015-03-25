
public class objectContainer {
	Acao contido;
	int quantidade;
	
	public objectContainer(Acao o, int q){
		this.contido = o;
		this.quantidade = q;
	}
	
	public Acao getContido(){
		return this.contido;
	}
	
	public int getQuantidade(){
		return this.quantidade;
	}
	
	public void setQuantidade(int quantidade){
		this.quantidade = quantidade;
	}
}

/*
* Protótipos utilizados
*/
int mdc( int u , int v);
int[] amigos( int argc, int argv );

int main( int argc, char** argv)
{
if( argc != 3)
{
printf("\nPARÂMETROS ERRADOS (insira dois parâmetros.\n" );
return 1;
}

int a = atoi(argv[1]);
int b = atoi(argv[2]);

int[] c = amigos( a, b );
imprimirAmigos( c );

return 0;
}

int mdc( int u , int v) 
{
if (v == 0)
return u ;
return mdc(v , u % v );
}

/*
* Determina os pares de amigos. Vai ter um for e um for dentro. O for de 
* dentro vai ficar comparando com for de fora para ver se ele é amigo.
* @param arg1 o inicio da área de busca
* @param arg2 o final da faixa de busca
* @return um array de inteiros contendos os pares dos amigos 
*/
int[] amigos( int arg1, int arg2 )
{

}

/*
* Imprime os amigos
* @param amigos um array que contém os amigos aos pares começando em zero.
*/
void imprimirAmigos( int[] amigos )
{

}
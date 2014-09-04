/*
 * Pilha.hpp
 *
 *  Created on: 26/08/2014
 *      Author: Charles B. L. e Evandro Coan -  Grupo 4
 */

#ifndef PILHA_HPP_
#define PILHA_HPP_

#include <iostream>
using namespace std;

#define TAM_PILHA 30				/// máximo tamanho da estrutura definido em 30
#define ERROPILHACHEIA -1
#define ERROPILHAVAZIA -2

template<typename T>
class Pilha
{
	private:
		int sp;					/// ponteiro de endereço para escrita e leitura
		int tamPilha;			/// variável que armazena o tamanho da pilha
		T *stack;

	public:
		Pilha();				/// construtor da pilha com tamanho definido por TAM_PILHA
		Pilha<T>(int t);		/// construtor da pilha com tamanho definido por variavel externa
		void empilha(T dado);
		T desempilha();
		T topo();
		int getPosTopo();
		void limparPilha();
		bool PilhaVazia();
		bool PilhaCheia();
};

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
/**
 * O construtor ajusta o topo da pilha para o valor -1 (vazia)
 * cria a pilha conforme tamanho definido
 */
template <typename T>
Pilha<T>::Pilha(){
	tamPilha = TAM_PILHA;
	stack = new T[tamPilha];
	sp = -1;			/// inicia o ponteiro de pilha
}
//----------------------------------------------------------------------------
/**
 *  O construtor ajusta o topo da pilha para o valor -1 (vazia)
 *  cria a pilha conforme tamanho definido por t
 */
template <typename T>
Pilha<T>::Pilha(int t){
	tamPilha = t;
	stack = new T[tamPilha];
	sp = -1;			/// inicia o ponteiro de pilha
}
//----------------------------------------------------------------------------
/**
 * emiplha(T dado) coloca um novo dado na pilha, incrementa o stack pointer
 */
template <typename T>
void Pilha<T>::empilha(T dado)
{
	try{
		if(this -> PilhaCheia())
			throw ERROPILHACHEIA;
		else
		{
			sp++;
			stack[sp] = dado;
		}
	}
	catch(int nr){
		cout << "Erro empilha() - pilha cheia: " << nr << endl;
		throw ERROPILHACHEIA;
	}

}
//----------------------------------------------------------------------------
/**
 * desempilha() retorna o dado do topo da pilha e corrige o stack pointer
 */
template <typename T>
T Pilha<T>::desempilha(){
	try	{
		if(this -> PilhaVazia())
			throw ERROPILHAVAZIA;
		else
		{
			sp--;
			return stack[sp+1];
		}
	}
	catch(int nr){
		cout << "Erro desempilha() - pilha vazia: " << nr << endl;
		throw ERROPILHAVAZIA;
	}

}
//----------------------------------------------------------------------------
/**
 * topo() retorna o valor armazenado no topo da pilha e não altera o stack pointer
 */
template <typename T>
T Pilha<T>::topo(){

	try	{
		if(this -> PilhaVazia())
			throw ERROPILHAVAZIA;
		else
			return stack[sp];
	}
	catch(int nr)	{
		cout << "Erro topo() - pilha vazia: " << nr << endl;
		throw ERROPILHAVAZIA;
	}


}
//----------------------------------------------------------------------------
/**
 * getPosTopo() retorna o valor do stack pointer (topo)
 */
template <typename T>
int Pilha<T>::getPosTopo(){

	try	{
		if(this -> PilhaVazia())
			throw ERROPILHAVAZIA;
		else
			return sp;
	}
	catch(int nr){
		cout << "Erro getPosTopo() - pilha vazia: " << nr << endl;
		throw ERROPILHAVAZIA;
	}
}
//----------------------------------------------------------------------------
/**
 * limparPilha() volta o stack pointer ao valor de pilha vazia
 */
template <typename T>
void Pilha<T>::limparPilha(){
	sp = -1;
}
//----------------------------------------------------------------------------
/**
 * PilhaVazia() retorna verdadeiro ou falso
 */
template <typename T>
bool Pilha<T>::PilhaVazia(){
	if(sp == -1)
		return true;
	return false;
}
//----------------------------------------------------------------------------
/**
 * PilhaCheia() retorna verdadeiro ou falso
 */
template <typename T>
bool Pilha<T>::PilhaCheia(){
	if(sp == (tamPilha-1))
		return true;
	return false;
}
//----------------------------------------------------------------------------


#endif /* PILHA_HPP_ */

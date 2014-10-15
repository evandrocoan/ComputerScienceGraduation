//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * Arquivo que contém as assinaturas do métodos que manipulam uma árvore 
 * binária de busca que utiliza templates.
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */

#include <iostream>
#include <cctype>
#include <stdlib.h>
#include "NodeArvore.cpp"

using namespace std;

template< typename T >
class ArvoreBinaria
{
public:
    void insert( int, NodeArvore< T > & );
    void del( int, NodeArvore< T > & );
    int deletemin( NodeArvore< T > & );
    void find( int, NodeArvore< T > & );
    NodeArvore< T > findmin( NodeArvore< T > );
    NodeArvore< T > findmax( NodeArvore< T > );
    void makeempty( NodeArvore< T > & );
    void copy( NodeArvore< T > &, NodeArvore< T > & );
    NodeArvore< T > nodecopy( NodeArvore< T > & );
    void preorder( NodeArvore< T > );
    void inorder( NodeArvore< T > );
    void postorder( NodeArvore< T > );
    int bsheight( NodeArvore< T > );
    NodeArvore< T > srl( NodeArvore< T > & );
    NodeArvore< T > drl( NodeArvore< T > & );
    NodeArvore< T > srr( NodeArvore< T > & );
    NodeArvore< T > drr( NodeArvore< T > & );
    int max( int, int );
    int nonodes( NodeArvore< T > );
};

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
#include "arvoreBinaria.cpp"

using namespace std;

class ArvoreBinaria
{
public:
    void insert( int, nodeptr & );
    void del( int, nodeptr & );
    int deletemin( nodeptr & );
    void find( int, nodeptr & );
    nodeptr findmin( nodeptr );
    nodeptr findmax( nodeptr );
    void makeempty( nodeptr & );
    void copy( nodeptr &, nodeptr & );
    nodeptr nodecopy( nodeptr & );
    void preorder( nodeptr );
    void inorder( nodeptr );
    void postorder( nodeptr );
    int bsheight( nodeptr );
    nodeptr srl( nodeptr & );
    nodeptr drl( nodeptr & );
    nodeptr srr( nodeptr & );
    nodeptr drr( nodeptr & );
    int max( int, int );
    int nonodes( nodeptr );
};

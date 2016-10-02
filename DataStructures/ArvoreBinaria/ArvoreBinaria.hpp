//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * Arquivo que contém as assinaturas do métodos que manipulam uma árvore 
 * binária de busca que utiliza templates.
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */

#ifndef ARVOREBINARIA_HPP_
#define ARVOREBINARIA_HPP_

#include <iostream>
#include <cctype>
#include <stdlib.h>

using namespace std;

template< typename T >
class ArvoreBinaria
{
public:
    /**
     *  Inteiro que armazena a altura (nível do nodo)
     */
    int height;
    
    /**
     * Retorna a altura no nodo com um inteiro
     * 
     * @return um inteiro representando a altura do nó
     */
    int getHeight() const
    {
        return height;
    }
    
    /**
     * Ajusta a altura do nó.
     * 
     * @param um inteiro informando qual a altura do nó
     */
    void setHeight( int height )
    {
        this->height = height;
    }
    
    // Inserting a node
    void insert( T informacao, NodeArvore< T > & elementoPai )
    {
        if( elementoPai.getInfo( ) == NULL )
        {
            elementoPai = new NodeArvore< T >( informacao, NULL, NULL, 0 );
            if( elementoPai == NULL )
            {
                cout << "Out of Space\n" << endl;
            }
        } else
        {
            if( informacao < elementoPai->element )
            {
                insert( informacao, elementoPai->left );
                if( ( bsheight( elementoPai->left ) - bsheight(
                        elementoPai->right ) )
                    == 2 )
                {
                    if( informacao < elementoPai->left->element )
                    {
                        elementoPai = srl( elementoPai );
                    } else
                    {
                        elementoPai = drl( elementoPai );
                    }
                }
            } else
                if( informacao > elementoPai->element )
                {
                    insert( informacao, elementoPai->right );
                    if( ( bsheight( elementoPai->right ) - bsheight(
                            elementoPai->left ) )
                        == 2 )
                    {
                        if( informacao > elementoPai->right->element )
                        {
                            elementoPai = srr( elementoPai );
                        } else
                        {
                            elementoPai = drr( elementoPai );
                        }
                    }
                } else
                {
                    cout << "Element Exists\n" << endl;
                }
        }
        int m, n, d;
        m = bsheight( elementoPai->left );
        n = bsheight( elementoPai->right );
        d = max( m, n );
        elementoPai->height = d + 1;
    }
    
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
    int nonodes( NodeArvore< T > );*/
};
// Finding the Smallest
template< typename T >
NodeArvore< T > ArvoreBinaria< T >::findmin( NodeArvore< T > p )
{
    if( p == NULL )
    {
        cout << "The tree is empty\n" << endl;
        return p;
    } else
    {
        while( p->left != NULL )
        {
            p = p->left;
            //return p;
        }
        return p;
    }
}

// Finding the Largest node
template< typename T >
NodeArvore< T > ArvoreBinaria< T >::findmax( NodeArvore< T > p )
{
    if( p == NULL )
    {
        cout << "The tree is empty\n" << endl;
        return p;
    } else
    {
        while( p->right != NULL )
        {
            p = p->right;
            //return p;
        }
        return p;
    }
}

// Finding an element
template< typename T >
void ArvoreBinaria< T >::find( int x, NodeArvore< T > &p )
{
    if( p == NULL )
    {
        cout << "Sorry! element not found\n" << endl;
    } else
    {
        if( x < p->element )
        {
            find( x, p->left );
        } else
        {
            if( x > p->element )
            {
                find( x, p->right );
            } else
            {
                cout << "Element found!\n" << endl;
            }
        }
    }
}

// Copy a tree
template< typename T >
void ArvoreBinaria< T >::copy( NodeArvore< T > &p, NodeArvore< T > &p1 )
{
    makeempty( p1 );
    p1 = nodecopy( p );
}

// Make a tree empty
template< typename T >
void ArvoreBinaria< T >::makeempty( NodeArvore< T > &p )
{
    NodeArvore< T > d;
    if( p != NULL )
    {
        makeempty( p->left );
        makeempty( p->right );
        d = p;
        free( d );
        p = NULL;
    }
}
// Copy the nodes
template< typename T >
NodeArvore< T > ArvoreBinaria< T >::nodecopy( NodeArvore< T > &p )
{
    NodeArvore< T > temp;
    if( p == NULL )
    {
        return p;
    } else
    {
        NodeArvore< T > temp2;
        temp = temp2;
        temp->element = p->element;
        temp->left = nodecopy( p->left );
        temp->right = nodecopy( p->right );
        return temp;
    }
}

// Deleting a node
template< typename T >
void ArvoreBinaria< T >::del( int x, NodeArvore< T > &p )
{
    NodeArvore< T > d;
    if( p == NULL )
    {
        cout << "Sorry! element not found\n" << endl;
    } else
        if( x < p->element )
        {
            del( x, p->left );
        } else
            if( x > p->element )
            {
                del( x, p->right );
            } else
                if( ( p->left == NULL ) && ( p->right == NULL ) )
                {
                    d = p;
                    free( d );
                    p = NULL;
                    cout << "Element deleted successfully\n" << endl;
                } else
                    if( p->left == NULL )
                    {
                        d = p;
                        free( d );
                        p = p->right;
                        cout << "Element deleted successfully\n" << endl;
                    } else
                        if( p->right == NULL )
                        {
                            d = p;
                            p = p->left;
                            free( d );
                            cout << "Element deleted successfully\n" << endl;
                        } else
                        {
                            p->element = deletemin( p->right );
                        }
}

template< typename T >
int ArvoreBinaria< T >::deletemin( NodeArvore< T > &p )
{
    int c;
    cout << "inside deltemin\n" << endl;
    if( p->left == NULL )
    {
        c = p->element;
        p = p->right;
        return c;
    } else
    {
        c = deletemin( p->left );
        return c;
    }
}

template< typename T >
void ArvoreBinaria< T >::preorder( NodeArvore< T > p )
{
    if( p != NULL )
    {
        cout << p->element << "\t";
        preorder( p->left );
        preorder( p->right );
    }
}

// Inorder Printing
template< typename T >
void ArvoreBinaria< T >::inorder( NodeArvore< T > p )
{
    if( p != NULL )
    {
        inorder( p->left );
        cout << p->element << "\t";
        inorder( p->right );
    }
}

// PostOrder Printing
template< typename T >
void ArvoreBinaria< T >::postorder( NodeArvore< T > p )
{
    if( p != NULL )
    {
        postorder( p->left );
        postorder( p->right );
        cout << p->element << "\t";
    }
}

template< typename T >
int ArvoreBinaria< T >::max( int value1, int value2 )
{
    return ( ( value1 > value2 ) ? value1 : value2 );
}

template< typename T >
int ArvoreBinaria< T >::bsheight( NodeArvore< T > p )
{
    int t;
    if( p == NULL )
    {
        return -1;
    } else
    {
        t = p->height;
        return t;
    }
}

template< typename T >
NodeArvore< T > ArvoreBinaria< T >::srl( NodeArvore< T > &p1 )
{
    NodeArvore< T > p2;
    p2 = p1->left;
    p1->left = p2->right;
    p2->right = p1;
    p1->height = max( bsheight( p1->left ), bsheight( p1->right ) ) + 1;
    p2->height = max( bsheight( p2->left ), p1->height ) + 1;
    return p2;
}

template< typename T >
NodeArvore< T > ArvoreBinaria< T >::srr( NodeArvore< T > &p1 )
{
    NodeArvore< T > p2;
    p2 = p1->right;
    p1->right = p2->left;
    p2->left = p1;
    p1->height = max( bsheight( p1->left ), bsheight( p1->right ) ) + 1;
    p2->height = max( p1->height, bsheight( p2->right ) ) + 1;
    return p2;
}

template< typename T >
NodeArvore< T > ArvoreBinaria< T >::drl( NodeArvore< T > &p1 )
{
    p1->left = srr( p1->left );
    return srl( p1 );
}

template< typename T >
NodeArvore< T > ArvoreBinaria< T >::drr( NodeArvore< T > &p1 )
{
    p1->right = srl( p1->right );
    return srr( p1 );
}

template< typename T >
int ArvoreBinaria< T >::nonodes( NodeArvore< T > p )
{
    int count = 0;
    if( p != NULL )
    {
        nonodes( p->left );
        nonodes( p->right );
        count++;
    }
    return count;
}

#endif /* ARVOREBINARIA_HPP */

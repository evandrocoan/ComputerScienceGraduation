//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * Arquivo que contém as implementações das assinaturas dos métodos de uma 
 * árvore binária utilizando templates.
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */
#include "ArvoreBinaria.hpp"

// Inserting a node
template< typename T >
void ArvoreBinaria< T >::insert( int x, NodeArvore< T > &p )
{
    if( p == NULL )
    {
        p = NodeArvore;
        p->element = x;
        p->left = NULL;
        p->right = NULL;
        p->height = 0;
        if( p == NULL )
        {
            cout << "Out of Space\n" << endl;
        }
    } else
    {
        if( x < p->element )
        {
            insert( x, p->left );
            if( ( bsheight( p->left ) - bsheight( p->right ) ) == 2 )
            {
                if( x < p->left->element )
                {
                    p = srl( p );
                } else
                {
                    p = drl( p );
                }
            }
        } else
            if( x > p->element )
            {
                insert( x, p->right );
                if( ( bsheight( p->right ) - bsheight( p->left ) ) == 2 )
                {
                    if( x > p->right->element )
                    {
                        p = srr( p );
                    } else
                    {
                        p = drr( p );
                    }
                }
            } else
            {
                cout << "Element Exists\n" << endl;
            }
    }
    int m, n, d;
    m = bsheight( p->left );
    n = bsheight( p->right );
    d = max( m, n );
    p->height = d + 1;
}

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
        temp = new node;
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

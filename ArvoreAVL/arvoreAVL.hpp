#include <iostream>
#include <cctype>
#include <stdlib.h>

using namespace std;

struct node
{
    int element;
    node *left;
    node *right;
    int height;
};
typedef struct node *nodeptr;
class bstree
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

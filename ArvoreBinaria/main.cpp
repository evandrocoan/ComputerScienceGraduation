/**
 * Arquivo que contém testes aplicados à uma árvore bináira.
 */

/**
 * Inclui as assinaturas dos métodos utilizados nos testes
 */
#include "ArvoreBinaria.cpp"
#include "ArvoreBinaria.hpp"

/**
 * Função principal que realiza a chamada dos testes a serem executados.
 * 
 * @return o código de retorno do estado da execuçãodo programa
 */
int main()
{
    testesCout();
    
    return 0;
}

/**
 * Função que realiza teste utilizando cout
 */
void testesCout()
{
    
    //clrscr();
    NodeArvore< int > root, root1, min, max; //,flag;
    int a, choice, findele, delele;
    char ch = 'y';
    ArvoreBinaria bst;
    
    //system("clear");
    root = 0;
    root1 = NULL;
    cout << "\n\t\t\t\tWELCOME TO AVL TREE" << endl;
    cout << "\t\t\t\t:::::::::::::::::::\n" << endl;

    cout << "\n\t\tADDING NEW NODE" << endl;
    cout << "\t\t:::::::::::::\n" << endl;
    cout << "Enter a new value: ";
    cin >> a;
    bst.insert( a, root );
    cout << "\nThe new value have been added to your tree successfully\n"
         << endl;

    if( root != NULL )
    {
        min = bst.findmin( root );
        cout << "\nThe minimum element in the tree is: "
             << min->element << endl;
    }

    if( root != NULL )
    {
        max = bst.findmax( root );
        cout << "\nThe maximum element in the tree is: "
             << max->element << endl;
    }

    cout << "\nEnter node to search: ";
    cin >> findele;
    if( root != NULL )
    {
        bst.find( findele, root );
    }

    cout << "\nEnter node to delete: ";
    cin >> delele;
    bst.del( delele, root );
    bst.inorder( root );
    cout << endl;

    cout << "\n\t\tPRE-ORDER TRAVERSAL" << endl;
    bst.preorder( root );
    cout << endl;

    cout << "\n\t\tIN-ORDER TRAVERSAL" << endl;
    bst.inorder( root );
    cout << endl;

    cout << "\n\t\tPOST ORDER TRAVERSAL" << endl;
    bst.postorder( root );
    cout << endl;

    cout << "\n\t\tHEIGHT\n" << endl;
    cout << "The height of the tree is: " << bst.bsheight( root )
         << endl;

    cout << "\n\tThank your for using AVL tree program\n" << endl;

    cout << "Sorry! wrong input\n" << endl;

}

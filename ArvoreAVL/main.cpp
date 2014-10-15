int main()
{
   //clrscr();
   nodeptr root, root1, min, max;//,flag;
   int a, choice, findele, delele;
   char ch = 'y';
   bstree bst;

   //system("clear");
   root = NULL;
   root1 = NULL;
   cout << "\n\t\t\t\tWELCOME TO AVL TREE" << endl;
   cout << "\t\t\t\t:::::::::::::::::::\n" << endl;

   do
   {
      cout << "\t\t::::::::::::::::::::::::::::::::::::::::::::::::" << endl;
      cout << "\t\t::::Enter 1 to insert a new node::::::::::::::::" << endl;
      cout << "\t\t::::Enter 2 to find the minimum value:::::::::::" << endl;
      cout << "\t\t::::Enter 3 to find the max value:::::::::::::::" << endl;
      cout << "\t\t::::Enter 4 to search a value:::::::::::::::::::" << endl;
      cout << "\t\t::::Enter 5 to delete a value:::::::::::::::::::" << endl;
      cout << "\t\t::::Enter 6 to display Preorder:::::::::::::::::" << endl;
      cout << "\t\t::::Enter 7 to display Inorder::::::::::::::::::" << endl;
      cout << "\t\t::::Enter 8 to display Postorder::::::::::::::::" << endl;
      cout << "\t\t::::Enter 9 to display the height of the tree:::" << endl;
      cout << "\t\t::::Enter 0 to exit:::::::::::::::::::::::::::::" << endl;
      cout << "\t\t::::::::::::::::::::::::::::::::::::::::::::::::\n" << endl;

      cout << "\nEnter the choice: ";
      cin >> choice;

      switch(choice)
      {
         case 1:
            cout << "\n\t\tADDING NEW NODE" << endl;
            cout << "\t\t:::::::::::::\n" << endl;
            cout << "Enter a new value: ";
            cin >> a;
            bst.insert( a, root );
            cout << "\nThe new value have been added to your tree successfully\n" << endl;
            break;
         case 2:
            if(root != NULL)
            {
               min = bst.findmin( root );
               cout << "\nThe minimum element in the tree is: " << min->element << endl;
            }
            break;
         case 3:
            if(root != NULL)
            {
               max = bst.findmax( root );
               cout << "\nThe maximum element in the tree is: " << max->element << endl;
            }
            break;
         case 4:
            cout << "\nEnter node to search: ";
            cin >> findele;
            if(root != NULL)
            {
               bst.find( findele, root );
            }
            break;
         case 5:
            cout << "\nEnter node to delete: ";
            cin >> delele;
            bst.del( delele, root );
            bst.inorder( root );
            cout << endl;
            break;
         case 6:
            cout << "\n\t\tPRE-ORDER TRAVERSAL" << endl;
            bst.preorder( root );
            cout << endl;
            break;
         case 7:
            cout << "\n\t\tIN-ORDER TRAVERSAL" << endl;
            bst.inorder( root );
            cout << endl;
            break;
         case 8:
            cout << "\n\t\tPOST ORDER TRAVERSAL" << endl;
            bst.postorder( root );
            cout << endl;
            break;
         case 9:
            cout << "\n\t\tHEIGHT\n" << endl;
            cout << "The height of the tree is: " << bst.bsheight( root ) << endl;

            break;
         case 0:
            cout << "\n\tThank your for using AVL tree program\n" << endl;
            break;
         default:
            cout << "Sorry! wrong input\n" << endl;
            break;
      }
      system( "pause" );
      system( "cls" );
   } while(choice != 0);
   return 0;
}

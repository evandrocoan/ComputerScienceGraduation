// COMPLETAR COM O NOME DA DUPLA
// ACADÊMICO 1:
// ACADÊMICO 2:

#include <cstdlib>
#include <iostream>
#include <iterator> 
#include <sstream>
#include <string>
#include <vector>

// COMPLETAR COM OS INCLUDES PARA AS CHAMADAS DE SISTEMA
// #include ...

using namespace std;

class CommandInterpreter
{
    
public:
    void run()
    {
        while( true )
        {
            //Get a command with parameters
            //and parse it into a vector
            string input;
            vector < string > commands;
            getline( cin, input );
            istringstream iss( input );
            copy( istream_iterator < string > ( iss ),
                  istream_iterator< string >(), back_inserter( commands ) );
            string command( commands[ 0 ] );
            //Verify the command
            int result;
            if ( command.compare( "exit" ) == 0 )
            {
                return;
            } else
                if ( command.compare( "creat" ) == 0 )
                {
                    if ( commands.size() != 2 )
                    {
                        cout << "Incorrect number of arguments!" << endl;
                        continue;
                    }
                    string aux( commands[ 1 ] );
                    const char * file_name = aux.c_str();
                    // COMPLETAR COM A CHAMADA DE SISTEMA
                    cout << result << endl;
                    
                } else
                    if ( command.compare( "cd" ) == 0 )
                    {
                        if ( commands.size() != 2 )
                        {
                            cout << "Incorrect number of arguments!" << endl;
                            continue;
                        }
                        string aux( commands[ 1 ] );
                        const char * path = aux.c_str();
                        // COMPLETAR COM A CHAMADA DE SISTEMA
                        cout << result << endl;
                        
                    } else
                        if ( command.compare( "open" ) == 0 )
                        {
                            if ( commands.size() != 2 )
                            {
                                cout << "Incorrect number of arguments!"
                                << endl;
                                continue;
                            }
                            string aux( commands[ 1 ] );
                            // COMPLETAR COM A ADAPTAÇÃO DO PARÂMETRO
                            // COMPLETAR COM A CHAMADA DE SISTEMA
                            cout << result << endl;
                            
                        } else
                            if ( command.compare( "link" ) == 0 )
                            {
                                if ( commands.size() != 3 )
                                {
                                    cout << "Incorrect number of arguments!"
                                    << endl;
                                    continue;
                                }
                                // COMPLETAR COM A ADAPTAÇÃO DOS PARÂMETROS
                                // COMPLETAR COM A CHAMADA DE SISTEMA
                                cout << result << endl;
                                
                            } else
                                if ( command.compare( "write" ) == 0 )
                                {
                                    if ( commands.size() != 2 )
                                    {
                                        cout << "Incorrect number of arguments!"
                                        << endl;
                                        continue;
                                    }
                                    // COMPLETAR COM A ADAPTAÇÃO DO PARÂMETRO
                                    // COMPLETAR COM A CHAMADA DE SISTEMA
                                    cout << result << endl;
                                    
                                } else
                                    if ( command.compare( "mkdir" ) == 0 )
                                    {
                                        if ( commands.size() != 2 )
                                        {
                                            cout << "Incorrect number of arguments!"
                                            << endl;
                                            continue;
                                        }
                                        // COMPLETAR COM A ADAPTAÇÃO DO PARÂMETRO
                                        // COMPLETAR COM A CHAMADA DE SISTEMA
                                        cout << result << endl;
                                        
                                    } else
                                        if ( command.compare( "unlink" ) == 0 )
                                        {
                                            if ( commands.size() != 2 )
                                            {
                                                cout << "Incorrect number of arguments!"
                                                << endl;
                                                continue;
                                            }
                                            // COMPLETAR COM A ADAPTAÇÃO DO PARÂMETRO
                                            // COMPLETAR COM A CHAMADA DE SISTEMA
                                            cout << result << endl;
                                            
                                        } else
                                            if ( command.compare( "uname" ) == 0 )
                                            {
                                                struct utsname u;
                                                // COMPLETAR COM A CHAMADA DE SISTEMA
                                                cout << u.sysname << " "
                                                << u.release << "" << u.version
                                                << endl;
                                                
                                            } else
                                                if ( command.compare( "getuid" ) == 0 )
                                                {
                                                    // COMPLETAR COM A CHAMADA DE SISTEMA
                                                    cout << result << endl;
                                                    
                                                } else
                                                {
                                                    cout << "Command not found!"
                                                    << endl;
                                                }
        }
    }
};

int main()
{
    CommandInterpreter ci;
    ci.run();
}

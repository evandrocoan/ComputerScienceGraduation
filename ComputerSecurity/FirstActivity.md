

1. Criar certificado pgp. Obs.: SALVAR A CHAVE PRIVADA E NÃO ESQUECER SENHA;<br>
    Faça um backup da sua chave privada;<br>
    Publicar a chave pública em um repositório PGP. Exemplos:<br>
    Keyserver da RNP ( use o Google para encontrar o site )<br>
    MIT PGP Public Key Server<br>
    Keyserver PGP.com<br>
    Referências: gpg --help e https://help.ubuntu.com/community/GnuPrivacyGuardHowto

    https://www.gnupg.org/documentation/manuals/gnupg/

    https://www.google.com.br/search?q=backup+private+key+gpg
    https://gist.github.com/chrisroos/1205934

    https://www.google.com.br/search?q=publish+public+key+gpg
    https://www.gnupg.org/gph/en/manual/x457.html


2. Crie um novo certificado PGP para este trabalho individual (Não use o teu certificado pois ele terá que ser revogado). Coloque esse certificado de testes no servidor PGP. Depois verifique seu status. Então, crie um certificado de revogação e revogue o certificado de testes. <br>
É importante lembrar que a base de dados local ( anéis de chaves privadas e públicas ) precisam ser atualizadas com o conteúdo dos servidores PGP. Utilize a opção "reflesh" periodicamente para fazer isso. Faça um relatório do que você fez, incluindo o KeyID do certificado revogado.



3. Pratique a revogação de certificados PGP. Assine um certificado qualquer PGP ( de outra pessoa ). E envie esse certificado para o servidor PGP. Depois verifique o status do certificado. E então, revogue a assinatura que você fez. Confira o resultado no servidor PGP. Faça um relatório do que você fez, incluindo o KeyID do certificado cuja assinatura você revogou.

4. O que é o anel de chaves privadas? Como este está estruturado? Na sua aplicação GPG onde este anel de chaves é armazenado? Quem pode ser acesso a esse porta chaves?

5. Qual a diferença entre assinar uma chave local e assinar no servidor?

6. O que é e como é organizado o banco de dados de confiabilidade?

7. O que são e para que servem as sub-chaves?

8. Coloque sua foto ( ou uma figura qualquer ) que represente você em seu certificado GPG.

9. O que é preciso para criar e manter um servidor de chaves GPG, sincronizado com os demais servidores existentes?

10. Dê um exemplo de como tornar sigiloso um arquivo usando o GPG. Envie esse arquivo para um colega e que enviar para você outro arquivo cifrado. Você deve decifrar e recuperar o conteúdo original.

11. Dê um exemplo de como assinar um arquivo ( assinatura anexada e outro com assinatura separada ), usando o GPG. Envie uma mensagem assinada para um colega. Esse colega deve enviar para você outra mensagem assinada. Verifique se a assinatura está correta.


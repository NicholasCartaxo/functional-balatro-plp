# functional-balatro-plp
## EXECUÇÃO
Para rodar, eu utilizei o comando no terminal, já dentro do diretorio principal:
> ghc -isrc app/Main.hs

Depois de compilar, acessem o diretório app e executem no terminal:
> .\Main.exe

Importante mencionar que descobrimos que dependendo de qual terminal/SO utilizar, a UTF-8 não funciona, podendo aparecer caracteres corrompidos ao executar o Main.exe, logo, antes de executar qualquer coisa, definam a UTF-8 como formatação padrão de caracteres do terminal de vocês. No meu (Windows PowerShell) eu utilizei:
> [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

Ainda não vi como alterar o encoding do projeto ainda, então, por hora, deixei os imports necessários dentro do arquivo Main e a dica de cima para rodarem com tranquilidade.

> [!NOTE]
> Pesquisei também sobre apagar os arquivos de objeto (.o) e os de interface compilada (.hi) que surgem após "ghc -isrc app/Main.hs", e vi que com eles no diretório recompila mais rápido depois de um tempo de uso, o que me ajudou a ir mais rápido do desenvolvimento, por isso não apaguei. Se quiserem apagar todos eles de uma vez, pelo menos no Windows PowerShell pode-se usar:
> *.o
> *.hi

> [!CAUTION]
> Não apagar o Main.exe

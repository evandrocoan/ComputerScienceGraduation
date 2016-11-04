# 
# Se ao somar 1º resto com o 2º, e isso for menor the 'restoLimite', significa que
# este o grau desse polinômio é pelo meno 2.
# 
# O resto 1 sozinho não diz nada de interessante. Se ele for menor, significa que ele convergiu,
# então que bom. Se ele não for, significa que não convergiu.
# 
# @param R     vetor com os restos do polinômio.
# 
function multiplicidade = calcularNumeroDeMultiplicides( R )
    
    # Depende de várias coisas, é um estudo em aberto ainda. Mas quando há duas raízes muito
    # próximas entre si, o valor de restoLimite deve ser pequeno e, não deve ser tão grande
    # quanto '0.1'.
    restoLimite = 1e-4
    
    soma           = 0
    multiplicidade = 1
    
    # O que garante que esse processo irá parar, é que o último resto não será zero.
    # Isso acontece por que a última derivada de um polinômio de grau n, nunca é zero.
    #
    # P(n)   = x^2 + 1
    # P(n)'  = 2*x
    # P(n)'' = 2
    # 
    while( soma < restoLimite )
        
        multiplicidade = multiplicidade + 1
        soma           = soma + R( multiplicidade ) 
        
    end
    
    multiplicidade = multiplicidade - 2
    
end

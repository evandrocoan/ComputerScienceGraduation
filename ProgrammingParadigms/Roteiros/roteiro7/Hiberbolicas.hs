module Hiberbolicas ( senoHiperbolico, cossenoHiperbolico, tangenteHiperbolico, 
									cotangenteHiperbolico
									) where

senoHiperbolico :: Float -> Float
senoHiperbolico u = ( (exp 1)**u - (exp 1)**(-u) ) / 2 

cossenoHiperbolico :: Float -> Float
cossenoHiperbolico u = ( (exp 1)**u + (exp 1)**(-u) ) / 2 

tangenteHiperbolico :: Float -> Float
tangenteHiperbolico u = ( (exp 1)**u - (exp 1)**(-u) ) / ( (exp 1)**u + (exp 1)**(-u) )

cotangenteHiperbolico :: Float -> Float
cotangenteHiperbolico u = ( (exp 1)**u + (exp 1)**(-u) ) / 
																						( (exp 1)**u - (exp 1)**(-u) ) 

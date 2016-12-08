Ie = 2/3;
n=3500
Tn = fTn(n, 0, 1)
erroTn = abs(Tn - fTn(2*n, 0, 1));
erroExatoTn = abs(Tn - Ie)

n = 1900
Sn = fSn(n, 0, 1)
erroTn = abs(Sn - fSn(2*n, 0, 1))
erroExatoSn = abs(Sn - Ie)

m = 10
Gm = fGm(m, 0, 1)
erroExatoGm = abs(Gm - Ie)
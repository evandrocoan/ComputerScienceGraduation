

function gaussLegendreIntegral = problem1GaussLegendreIntegral( m, a, b )

    t = [  0        , 0        , 0        ;
          -1/sqrt(3), 1/sqrt(3), 0        ;
          -sqrt(3/5), 0        , sqrt(3/5); ...
        ];

    C = [ 2  , 0  , 0   ;
          1  , 1  , 0   ;
          5/9, 8/9, 5/9 ; ...
        ];

    # C=[
    #     [2                      0                      0                      0                      0                      0                      0                      ];
    #     [1                      1                      0                      0                      0                      0                      0                      ];
    #     [5/9                    8/9                    5/9                    0                      0                      0                      0                      ];
    #     [0.34785484513745385737 0.65214515486254614263 0.65214515486254614263 0.34785484513745385737 0                      0                      0                      ];
    #     [0.23692688505618908751 0.47862867049936646804 128/225                0.47862867049936646804 0.23692688505618908751 0                      0                      ];
    #     [0.17132449237917034504 0.36076157304813860757 0.46791393457269104739 0.46791393457269104739 0.36076157304813860757 0.17132449237917034504 0                      ];
    #     [0.129484966168869693271 0.27970539148927666790 0.38183005050511894495 512/1225              0.38183005050511894495 0.27970539148927666790 0.129484966168869693271];
    #   ];

    # t=[
    #     [ 0                       0                       0                      0                      0                       0                      0                     ];
    #     [-1/sqrt(3)               1/sqrt(3)               0                      0                      0                       0                      0                     ];
    #     [-sqrt(3/5)               0                       sqrt(3/5)              0                      0                       0                      0                     ];
    #     [-0.8611363115940525752  -0.3399810435848562648   0.3399810435848562648  0.8611363115940525752  0                       0                      0                     ];
    #     [-0.90617984593866399280 -0.53846931010568309104  0                      0.53846931010568309104 0.90617984593866399280  0                      0                     ];
    #     [-0.93246951420315202781 -0.66120938646626451366 -0.23861918608319690863 0.23861918608319690863 0.66120938646626451366  0.93246951420315202781 0                     ];
    #     [-0.94910791234275852453 -0.74153118559939443986 -0.40584515137739716691 0                      0.40584515137739716691  0.74153118559939443986 0.94910791234275852453];
    #   ];

    t = calculateLegendreWeightCoefficients( m );
    C = calculateLegendreNodeCoefficients( m, t );

    summation = 0;

    for i = 1 : m

        x(i)      = ChebyshevDomainLinearTransformationOut( t(m,i), a, b );
        summation = summation + C(m,i)*problem1Function( x(i) );

    end

    gaussLegendreIntegral = (b-a)*summation / 2;

end


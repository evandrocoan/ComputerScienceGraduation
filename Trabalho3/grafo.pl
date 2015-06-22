:- dynamic aresta/2.

aresta([0, 0, 4], [1, 0, 30]).
aresta([1, 0, 30], [0, 0, 4]).
aresta([0, 0, 4], [0, 1, 1]).
aresta([0, 1, 1], [0, 0, 4]).
aresta([1, 0, 30], [2, 0, 2]).
aresta([2, 0, 2], [1, 0, 30]).
aresta([1, 0, 30], [1, 1, 2]).
aresta([1, 1, 2], [1, 0, 30]).
aresta([2, 0, 2], [3, 0, 1]).
aresta([3, 0, 1], [2, 0, 2]).
aresta([4, 0, 0], [3, 0, 1]).
aresta([3, 0, 1], [4, 0, 0]).
aresta([0, 1, 1], [1, 1, 2]).
aresta([1, 1, 2], [0, 1, 1]).
aresta([1, 1, 2], [2, 1, 3]).
aresta([2, 1, 3], [1, 1, 2]).
aresta([2, 1, 3], [3, 1, 9]).
aresta([3, 1, 9], [2, 1, 3]).
aresta([2, 1, 3], [2, 0, 2]).
aresta([2, 0, 2], [2, 1, 3]).
aresta([3, 1, 9], [4, 1, 1]).
aresta([4, 1, 1], [3, 1, 9]).
aresta([3, 1, 9], [3, 0, 1]).
aresta([3, 0, 1], [3, 1, 9]).
aresta([3, 1, 9], [3, 2, 7]).
aresta([3, 2, 7], [3, 1, 9]).
aresta([4, 1, 1], [5, 1, 9]).
aresta([5, 1, 9], [4, 1, 1]).
aresta([5, 1, 9], [5, 2, 5]).
aresta([5, 2, 5], [5, 1, 9]).
aresta([6, 1, 0], [5, 1, 9]).
aresta([5, 1, 9], [6, 1, 0]).
aresta([0, 2, 3], [1, 2, 9]).
aresta([1, 2, 9], [0, 2, 3]).
aresta([0, 2, 3], [0, 1, 1]).
aresta([0, 1, 1], [0, 2, 3]).
aresta([0, 2, 3], [0, 3, 2]).
aresta([0, 3, 2], [0, 2, 3]).
aresta([1, 2, 9], [2, 2, 8]).
aresta([2, 2, 8], [1, 2, 9]).
aresta([1, 2, 9], [1, 1, 2]).
aresta([1, 1, 2], [1, 2, 9]).
aresta([1, 2, 9], [1, 3, 5]).
aresta([1, 3, 5], [1, 2, 9]).
aresta([2, 2, 8], [3, 2, 7]).
aresta([3, 2, 7], [2, 2, 8]).
aresta([2, 2, 8], [2, 1, 3]).
aresta([2, 1, 3], [2, 2, 8]).
aresta([2, 2, 8], [2, 3, 0]).
aresta([2, 3, 0], [2, 2, 8]).
aresta([3, 2, 7], [4, 2, 6]).
aresta([4, 2, 6], [3, 2, 7]).
aresta([3, 2, 7], [3, 3, 0]).
aresta([3, 3, 0], [3, 2, 7]).
aresta([4, 2, 6], [5, 2, 5]).
aresta([5, 2, 5], [4, 2, 6]).
aresta([4, 2, 6], [4, 1, 1]).
aresta([4, 1, 1], [4, 2, 6]).
aresta([4, 2, 6], [4, 3, 0]).
aresta([4, 3, 0], [4, 2, 6]).
aresta([5, 2, 5], [5, 3, 0]).
aresta([5, 3, 0], [5, 2, 5]).
aresta([6, 2, 0], [5, 2, 5]).
aresta([5, 2, 5], [6, 2, 0]).
aresta([0, 3, 2], [1, 3, 5]).
aresta([1, 3, 5], [0, 3, 2]).
aresta([0, 3, 2], [0, 4, 1]).
aresta([0, 4, 1], [0, 3, 2]).
aresta([1, 3, 5], [1, 4, 0]).
aresta([1, 4, 0], [1, 3, 5]).
aresta([2, 3, 0], [1, 3, 5]).
aresta([1, 3, 5], [2, 3, 0]).
aresta([0, 4, 1], [0, 5, 0]).
aresta([0, 5, 0], [0, 4, 1]).
aresta([1, 4, 0], [0, 4, 1]).
aresta([0, 4, 1], [1, 4, 0]).
aresta([1, 4, 0], [2, 4, 3]).
aresta([2, 4, 3], [1, 4, 0]).
aresta([2, 4, 3], [2, 5, 1]).
aresta([2, 5, 1], [2, 4, 3]).
aresta([3, 4, 0], [2, 4, 3]).
aresta([2, 4, 3], [3, 4, 0]).
aresta([5, 4, 0], [6, 4, 1]).
aresta([6, 4, 1], [5, 4, 0]).
aresta([6, 4, 1], [6, 5, 0]).
aresta([6, 5, 0], [6, 4, 1]).
aresta([7, 4, 0], [6, 4, 1]).
aresta([6, 4, 1], [7, 4, 0]).
aresta([0, 5, 0], [1, 5, 7]).
aresta([1, 5, 7], [0, 5, 0]).
aresta([1, 5, 7], [2, 5, 1]).
aresta([2, 5, 1], [1, 5, 7]).
aresta([1, 5, 7], [1, 6, 0]).
aresta([1, 6, 0], [1, 5, 7]).
aresta([2, 5, 1], [2, 6, 0]).
aresta([2, 6, 0], [2, 5, 1]).
aresta([3, 5, 0], [2, 5, 1]).
aresta([2, 5, 1], [3, 5, 0]).
aresta([6, 6, 0], [7, 6, 5]).
aresta([7, 6, 5], [6, 6, 0]).
aresta([7, 6, 5], [8, 6, 1]).
aresta([8, 6, 1], [7, 6, 5]).
aresta([7, 6, 5], [7, 7, 0]).
aresta([7, 7, 0], [7, 6, 5]).
aresta([8, 6, 1], [8, 7, 0]).
aresta([8, 7, 0], [8, 6, 1]).
aresta([9, 6, 0], [8, 6, 1]).
aresta([8, 6, 1], [9, 6, 0]).
aresta([8, 9, 0], [9, 9, 100]).
aresta([9, 9, 100], [8, 9, 0]).

:- dynamic vertice/1.

vertice([0, 0, 4]).
vertice([1, 0, 30]).
vertice([0, 1, 1]).
vertice([2, 0, 2]).
vertice([1, 1, 2]).
vertice([3, 0, 1]).
vertice([4, 0, 0]).
vertice([5, 0, 0]).
vertice([6, 0, 0]).
vertice([7, 0, 0]).
vertice([8, 0, 0]).
vertice([9, 0, 0]).
vertice([2, 1, 3]).
vertice([3, 1, 9]).
vertice([4, 1, 1]).
vertice([3, 2, 7]).
vertice([5, 1, 9]).
vertice([5, 2, 5]).
vertice([6, 1, 0]).
vertice([7, 1, 0]).
vertice([8, 1, 0]).
vertice([9, 1, 0]).
vertice([0, 2, 3]).
vertice([1, 2, 9]).
vertice([0, 3, 2]).
vertice([2, 2, 8]).
vertice([1, 3, 5]).
vertice([2, 3, 0]).
vertice([4, 2, 6]).
vertice([3, 3, 0]).
vertice([4, 3, 0]).
vertice([5, 3, 0]).
vertice([6, 2, 0]).
vertice([7, 2, 0]).
vertice([8, 2, 0]).
vertice([9, 2, 0]).
vertice([0, 4, 1]).
vertice([1, 4, 0]).
vertice([6, 3, 0]).
vertice([7, 3, 0]).
vertice([8, 3, 0]).
vertice([9, 3, 0]).
vertice([0, 5, 0]).
vertice([2, 4, 3]).
vertice([2, 5, 1]).
vertice([3, 4, 0]).
vertice([4, 4, 0]).
vertice([5, 4, 0]).
vertice([6, 4, 1]).
vertice([6, 5, 0]).
vertice([7, 4, 0]).
vertice([8, 4, 0]).
vertice([9, 4, 0]).
vertice([1, 5, 7]).
vertice([1, 6, 0]).
vertice([2, 6, 0]).
vertice([3, 5, 0]).
vertice([4, 5, 0]).
vertice([5, 5, 0]).
vertice([7, 5, 0]).
vertice([8, 5, 0]).
vertice([9, 5, 0]).
vertice([0, 6, 0]).
vertice([3, 6, 0]).
vertice([4, 6, 0]).
vertice([5, 6, 0]).
vertice([6, 6, 0]).
vertice([7, 6, 5]).
vertice([8, 6, 1]).
vertice([7, 7, 0]).
vertice([8, 7, 0]).
vertice([9, 6, 0]).
vertice([0, 7, 0]).
vertice([1, 7, 0]).
vertice([2, 7, 0]).
vertice([3, 7, 0]).
vertice([4, 7, 0]).
vertice([5, 7, 0]).
vertice([6, 7, 0]).
vertice([9, 7, 0]).
vertice([0, 8, 0]).
vertice([1, 8, 0]).
vertice([2, 8, 0]).
vertice([3, 8, 0]).
vertice([4, 8, 0]).
vertice([5, 8, 0]).
vertice([6, 8, 0]).
vertice([7, 8, 0]).
vertice([8, 8, 0]).
vertice([9, 8, 0]).
vertice([0, 9, 0]).
vertice([1, 9, 0]).
vertice([2, 9, 0]).
vertice([3, 9, 0]).
vertice([4, 9, 0]).
vertice([5, 9, 0]).
vertice([6, 9, 0]).
vertice([7, 9, 0]).
vertice([8, 9, 0]).
vertice([9, 9, 100]).


# 2017-1 Modeling and Simulation



Repository for Modeling and Simulation Works at the subject `INE5425 - UFSC`.

The chart should be generated only when you request a replication. When more than one Replication.
It is necessary to perform a histogram instead of the graph.


### Installing

To run the program you need installed:

1. Python 2.7
1. PyQt4 (library)
1. matplotlib (library)


Usually the easiest way to install them is by installing [ANACONDA](https://www.continuum.io/downloads).
And using its package manager with the commands:

1. `conda remove pyqt`
1. `conda install pyqt=4`
1. `conda install matplotlib`


### Running

To run this application you just need to type from this folder:
```
python drunk_walk_simulation/main.py
```



### Exporting an executable

To export an executable you can use http://www.pyinstaller.org/ and run the command:
```
pyinstaller main.py
```
But you first need to copy the debug tools module from `../python_debug/debug_tools.py` to the
`main.py` the source folder. This is because `pyinstaller` does not support external include
folder using `sys.path.insert(0,'../python_debug')`. See the issue:

1. [pyinstaller#2640](https://github.com/pyinstaller/pyinstaller/issues/2640) Python files included by `sys.path.insert()` are missing on the final executable



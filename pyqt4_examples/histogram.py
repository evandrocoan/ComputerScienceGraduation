"""

How to make a histogram from a list of data
https://stackoverflow.com/questions/24809757/how-to-make-a-histogram-from-a-list-of-data


How does numpy.histogram() work?
https://stackoverflow.com/questions/9141732/how-does-numpy-histogram-work

"""

import numpy as np
import random
import math
from matplotlib import c

data = np.random.normal(0, 20, 1000)

bins = np.linspace(math.ceil(min(data)),
                   math.floor(max(data)),
                   20) # fixed number of bins

pyplot.xlim([min(data)-5, max(data)+5])

pyplot.hist(data, bins=bins, alpha=0.5)
pyplot.title('Random Gaussian data (fixed number of bins)')
pyplot.xlabel('variable X (20 evenly spaced bins)')
pyplot.ylabel('count')

pyplot.show()

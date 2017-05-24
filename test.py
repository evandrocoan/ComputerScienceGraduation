

import random
import matplotlib.pyplot as plt


x = random.sample(range(1000), 30)
xbins = [0, len(x)]
plt.bar(range(0,30), x)
plt.show()






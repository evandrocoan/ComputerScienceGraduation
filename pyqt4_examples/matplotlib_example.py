"""

Histogram Matplotlib
https://stackoverflow.com/questions/5328556/histogram-matplotlib

"""



import matplotlib.pyplot as plt
import numpy as np

mu, sigma = 100, 15
x = mu + sigma * np.random.randn(10000)

hist, bins = np.histogram(x, bins=50)

width = 0.7 * (bins[1] - bins[0])
center = (bins[:-1] + bins[1:]) / 2

plt.bar(center, hist, align='center', width=width)
plt.show()



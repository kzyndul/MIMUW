import numpy as np
import matplotlib.pyplot as plt

dane = np.array([[1.0, 1.3, 2.6, 0], [2.2, 1.1, 1.2, 1], [2.0, 2.4, 3.8, 1 ], [1.5, 3.2, 2.1, 0], [3.2, 1.2, 4.2, 1]])

x_ab = dane[:, [0, 1]]
x_ac = dane[:, [0, 2]]
y = dane[:, 3]

w = np.mgrid[0:1.1:0.1, 2:3.1:0.1]

def mean_Squared_Error(x, w):
    MSE = np.zeros(shape=w[0].shape)
    for i in range(len(x)):
        MSE += (y[i] - 1 / (1 + np.exp(-(w[0,:,:]*x[i][0] + w[1,:,:]*x[i][1]))))**2
    MSE /= len(x)
    return MSE


y_ab = mean_Squared_Error(x_ab, w)
y_ac = mean_Squared_Error(x_ac, w)
print(y_ab)

print(y_ac)


fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(10, 5), subplot_kw={'projection': '3d'})

axes[0].plot_surface(w[0,:,:], w[1,:,:], y_ab, cmap='viridis')
axes[0].set_title('A_B')

axes[1].plot_surface(w[0,:,:], w[1,:,:], y_ac, cmap='viridis')
axes[1].set_title('A_C')

plt.show()
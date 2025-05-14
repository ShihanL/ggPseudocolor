# ggPsueudocolor

ggPsueudocolor is a ggplot2 implementation of a bivariate density plot commonly used for visulising flow cytometry datasets, where scatter points are colored based on their relative densities. Base methods in ggplot2 such as geom_density_2d are unable to show individual datapoints making it harder to interpret data distributions. 

<img width="288" alt="image" src="https://github.com/user-attachments/assets/1ab0e80e-8818-463c-8c44-ad7897c7ffd8" />

Four density calculations are currently implemented, 
* pseudocolor: Continuous 2D KDE using `MASS::kde2d`
* pseudocolor_binned: Binned 2D KDE
* pseudocolor_nn: Density estimation via average distance to `k` nearest neighbors unsing (`FNN::get.knn`).
*  pseudocolor_nn_count: Counts the number of neighbors within a radius `r`

<img width="256" alt="image" src="https://github.com/user-attachments/assets/298048a7-583e-48e6-a326-078cdf10c82f" />


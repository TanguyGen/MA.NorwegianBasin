open3d() # Open a new RGL device

x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width
points3d(x, y, z, color ="lightgray") # Scatter plot

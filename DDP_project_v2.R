
library(ggplot2)
library(grid)
library(manipulate)

myPlot <- function(sigma, sd) {
    mu = 0  # mean of the distribution
    # sd = 1  # sigma, this will need to vary in the future
    
    # establish the tail length
    min <- mu - sd*7
    max <- mu + sd*7
    
    # create the x vector
    x <- seq(min, max, 0.001)
    
    # generate the normal distribution and move it to a data frame
    y <- dnorm(x, mu, sd)
    df <- data.frame("x" = x, "y" = y)
    
    # create min and max z values for a selected alpha
    zmin <- mu - sigma
    zmax <- mu + sigma
    
    # calculate the percentage of good under the curve
    percent <- pnorm(zmax, mean = mu, sd = sd) - 
                         pnorm(zmin, mean = mu, sd = sd)
    
    # calculate incorrect ppm
    ppm <- 1e6 - percent*1e6
    
    # create a polygon for the good area
    poly1 <- data.frame(x = x, y = pmin(y))
    poly1 <- poly1[poly1$x >= (zmin), ]
    poly1 <- poly1[poly1$x <= (zmax), ]
    poly1 <- rbind(poly1, c(zmax, 0))
    poly1 <- rbind(c(zmin,0), poly1)
    poly1$id <- 1
    poly1$value <- "good"
    
    #create the lower tail polygon
    poly2 <- df
    poly2 <- poly2[poly2$x <= zmin, ]
    poly2 <- rbind(poly2, c(zmin, 0))
    poly2 <- rbind(poly2, c(-7, 0))
    poly2$id <- 2
    poly2$value <- "not good"
    
    # create the upper tail polygon
    poly3 <- df
    poly3 <- poly3[poly3$x >= zmax, ]
    poly3 <- rbind(c(zmax, 0), poly3)
    poly3 <- rbind(c(7, 0), poly3)
    poly3$id <- 3
    poly3$value <- "not good"
    
    #combine the polygons into the same data frame
    poly <- rbind(poly1, poly2, poly3)
    poly$id <- factor(poly$id)
    poly$value <- factor(poly$value)
    
    plot2 <- ggplot(data = poly, aes(x=x, y=y)) +
        scale_fill_manual(values = c("good" = "olivedrab3", "not good" = "lightsteelblue4")) +
        geom_polygon(aes(fill = value, group = id)) +
        geom_line(data = df, aes(x, y)) +
        labs(x = "sigma", y = NULL) + 
        scale_x_continuous(limits = c(-7, 7), breaks = -7:7) +
        guides(fill = FALSE) +
        # green arrow
        annotate("segment", x=-3, y=0.2, xend=-0.5, yend=0.15, 
                 arrow = arrow(length = unit(0.3, "cm")), size=1) +
        # percent good
        annotate("text", label = percent, x=-4, y=0.25, parse=T, size=6) +
        # ppm
        annotate("text", label = ppm, x=5, y=0.3, parse=T, size=6) +
        # sigma level
        annotate("text", label = sigma, x=5, y=0.35, parse=T, size=6)
    
    plot2
}

manipulate(myPlot(sigma, sd), sigma = slider(1, 6, step = 1),
           sd = slider(1, 3, step = 0.5))





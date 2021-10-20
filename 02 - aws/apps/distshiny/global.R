library(ggplot2)

# Continuas
normal <- function(mu,sig,x1,x2) {
  x <- seq(from = mu - 6 * sig, to = mu + 6 * sig,length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dnorm(x,mu,sig))
  
  df2 <- data.frame(x = c(x1,seq(x1,x2,length.out = 1000),x2),
                    y = c(0,dnorm(seq(x1,x2,length.out = 1000),mu,sig),0))
  
  sumy <- df$y[which.max(df$y)] / 4
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  z1 <- (x1 - mu) / sig
  
  z2 <- (x2 - mu) / sig
  
  g <- qplot(df$x,df$y, geom = "line", xlab = "x", ylab = "f(x)") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1, aes(x = x, y = y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed")) + 
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.1, label = z1) +
    geom_line(data = arrow2,aes(x,y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed",angle = 45)) + 
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.1, label = z2)
  
  list(g,z1,z2,r = pnorm(z2) - pnorm(z1))
}

exponencial <- function(lambda,x1,x2){
 to <- if(x2 <= (10/lambda)){10 / lambda} else{x2}
 
 x <-  seq(from = 0, to = to, length.out = 1000)
 
 df <- data.frame(x = x,
                  y = dexp(x,lambda))
 
 df2 <- data.frame(x = c(x1,seq(from = x1, to = x2,length.out = 1000),x2),
                   y = c(0,dexp(seq(x1,x2,length.out = 1000),rate = lambda),0))
 
 sumy <- mean(df$y) * 1.2
 
 arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
 
 arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                      df2[nrow(df2)-1,2] + sumy))
 
 
 g <- qplot(x = x,y = y,data = df,geom = "line") + 
   geom_polygon(data = df2, aes(x,y), fill = "blue") + 
   labs(x = "x", y = "f(x)") + 
   geom_line(data = arrow1,aes(x = x,y = y),arrow = arrow(length = unit(0.3,"cm"),
                                           ends = "first",type = "closed")) + 
   geom_line(data = arrow2,aes(x = x,y = y),arrow = arrow(length = unit(0.3,"cm"),
                                           ends = "first",type = "closed")) + 
   annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
   annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
   
 list(g,r = pexp(x2,lambda) - pexp(x1,lambda))
}

uniforme_cont <- function(a,b,x1,x2){
  x <- seq(from = a, to = b, length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dunif(x,min = a,max = b))
  
  
  df2 <- data.frame(x = c(x1,seq(from = x1, to = x2, length.out = 1000),x2),
                    y = c(0,dunif(seq(x1,x2,length.out = 1000),a,b),0))
  
  sumy <- (1/(b-a)) * 0.25
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  
  g <- qplot(x = x, y = y, data = df, geom = "line",xlab = "x", ylab = "f(x)") + 
    geom_segment(aes(x = a, xend = a, y = 0, yend = 1 / (b - a)),
                 linetype = "dashed") + 
    geom_segment(aes(x = b, xend = b, y = 0, yend = 1 / (b - a)),
                 linetype = "dashed") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1,aes(x = x,y = y),
        arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) + 
    geom_line(data = arrow2,aes(x = x,y = y),
        arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) + 
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
  
  list(g,r = punif(x2,a,b) - punif(x1,a,b))
}

beta <- function(a,b,x1,x2) {
  x <- seq(0,1, length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dbeta(x,a,b))
  
  
  df2 <- data.frame(x = c(x1,seq(from = x1, to = x2, length.out = 1000),x2),
                    y = c(0,dbeta(seq(x1,x2,length.out = 1000),a,b),0))
  
  sumy <- max(df$y)*0.25
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  
  g <- qplot(x = x, y = y, data = df, geom = "line",xlab = "x", ylab = "f(x)") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1,aes(x = x,y = y),
              arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) +
    geom_line(data = arrow2,aes(x = x,y = y),
              arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) +
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
  
  list(g,r = pbeta(x2,a,b) - pbeta(x1,a,b)) 
}

gamma <- function(a,b,x1,x2) {
  x <- seq(0,((a/b) * 30),length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dgamma(x,shape = a,scale = b))
  
  df <- subset(df,df$y >= 0.0001)
  
  
  df2 <- data.frame(x = c(x1,seq(from = x1, to = x2, length.out = 1000),x2),
                    y = c(0,dgamma(seq(x1,x2,length.out = 1000),shape = a,
                                   scale = b),0))
  
  sumy <- max(df$y)*0.25
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  
  g <- qplot(x = x, y = y, data = df, geom = "line",xlab = "x", ylab = "f(x)") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1,aes(x = x,y = y),
              arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) +
    geom_line(data = arrow2,aes(x = x,y = y),
              arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) +
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
  
  list(g,r = pgamma(x2,shape = a,scale = b) - pgamma(x1,shape = a,scale = b)) 
}



# Discretas
uniforme_dis <- function(k,x1,x2){
  x <- seq(from = 1, to = k)
  
  df <- data.frame(x = x,
                   y = 1/k)
  
  df2 <- data.frame(x = seq(from = x1, to = x2),
                    y = rep(1/k, times = length(seq(from = x1, to = x2))))
  
  g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
        geom = "segment", xlab = "x", ylab = "f(x)") + 
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                 data = df2, colour = "blue") + 
    scale_x_discrete(limits = x,labels = as.factor(df$x))
  
  list(g,r = length(seq(from = x1, to = x2)) / k )
}

binomial <- function(n,p,x1,x2) {
  y <- dbinom(0:(2*n),n,p)
  
  df <- data.frame(x = 0:(length(y)-1),
                   y = y)
  
  df <- df[df$y >= 0.0000001,]
  
  df2 <- data.frame(x = seq(from = x1, to = x2),
                    y = dbinom(x1:x2,n,p))
  
  g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
             geom = "segment", xlab = "x", ylab = "f(x)") + 
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                 data = df2, colour = "blue") + 
    scale_x_discrete(limits = unique(c(df$x,df2$x)),
                     labels = as.factor(unique(c(df$x,df2$x))))
  
  list(g,r = if(x1 == x2) {dbinom(x2,n,p)} else {sum(dbinom(x1:x2,n,p))})
}

poisson <- function(lambda,x1,x2) {
  f <- floor(lambda - sqrt(lambda) * 6)
  f <- if(f <= 0){0} else {f}
  x <- f:ceiling(lambda + sqrt(lambda) * 6)
  
  
  df <- data.frame(x = x,
                   y = dpois(x,lambda))
  
  df <- subset(df,df$y >= 0.0001)
  
  df2 <- data.frame(x = seq(from = x1, to = x2),
                    y = dpois(x1:x2,lambda))
  
  g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
             geom = "segment", xlab = "x", ylab = "f(x)") + 
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                 data = df2, colour = "blue") + 
    scale_x_discrete(limits = unique(c(df$x,df2$x)),
                     labels = as.factor(unique(c(df$x,df2$x))))
  
  list(g,r = if(x1 == x2) {dpois(x2,lambda)} else {sum(dpois(x1:x2,lambda))})
}

geometrico <- function(p,x1,x2){
  x <- 0:(5/p)
  
  df <- data.frame(x = x,
                   y = dgeom(x,p))
  
  
  df2 <- data.frame(x = seq(from = x1, to = x2),
                    y = dgeom(x1:x2,p))
  
  df2 <- subset(df2, df2$y >= 0.0001)
  
  g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
             geom = "segment", xlab = "x", ylab = "f(x)")
  
  g <- if(nrow(df2) == 0){g} else {g + 
      geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                   data = df2, colour = "blue")
  }
  
  list(g,r = if(x1 == x2) {dgeom(x2,p)} else {sum(dgeom(x1:x2,p))})
}

hipergeometrica <- function(m,n,k,x1,x2){
    x <- 0:m
    
    df <- data.frame(x = x,
                     y = dhyper(x,m,n,k))
    
    # X1 tem que ser maior que 0 e x2 menor que k
    df2 <- data.frame(x = seq(from = x1, to = x2),
                      y = dhyper(x1:x2,m = m,n = n,k = k))
    
    
    g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
               geom = "segment", xlab = "m", ylab = "f(x)") +
      geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                   data = df2, colour = "blue") + 
      scale_x_discrete(limits = unique(c(df$x,df2$x)),
                       labels = as.factor(unique(c(df$x,df2$x))))
    
    
    list(g,r = if(x1 == x2) {dhyper(x2,m,n,k)} 
         else {sum(dhyper(x1:x2,m,n,k))})
}

binomialneg <- function(k,p,x1,x2) {
  x <- 0:(10/p)
  
  df <- data.frame(x = x,
                   y = dnbinom(x,size = k,prob = p))
  
  
  df2 <- data.frame(x = seq(from = x1, to = x2),
                    y = dnbinom(x1:x2,size = k,prob = p))
  
  df2 <- subset(df2, df2$y >= 0.0001)
  
  g <- qplot(x = x, xend = x, yend = y, y = 0, data = df,
             geom = "segment", xlab = "x", ylab = "f(x)")
  
  g <- if(nrow(df2) == 0){g} else {g + 
      geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                   data = df2, colour = "blue")
  }
  
  list(g,r = if(x1 == x2) {dnbinom(x2,k,p)} else {sum(dbinom(x1:x2,k,p))})
}

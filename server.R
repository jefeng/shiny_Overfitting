library(shiny)
library(ggplot2)
library(latex2exp)
library(corpcor)
source("multiplot.R")
load("err.Rdata")

shinyServer(function(input, output) {
  output$formula <- renderUI({
    # update model formula
    switch(input$ndeg,
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x$$"), #1
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2$$"), #2
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3$$"), #3
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4$$"), #4
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5$$"), #5
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5 + \\beta_6 x^6$$"), #6
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5 + \\beta_6 x^6 + \\beta_7 x^7$$"), 
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_7 x^7 + \\beta_8 x^8$$"),
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_8 x^8 + \\beta_9 x^9$$"),
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_9 x^9 + \\beta_{10} x^{10}$$")
           )
  })
  
  output$formula2 <- renderUI({
    # update model formula
    switch(input$ndeg2,
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x$$"), #1
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2$$"), #2
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3$$"), #3
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4$$"), #4
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5$$"), #5
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5 + \\beta_6 x^6$$"), #6
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + \\beta_3 x^3 + \\beta_4 x^4 + \\beta_5 x^5 + \\beta_6 x^6 + \\beta_7 x^7$$"), 
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_7 x^7 + \\beta_8 x^8$$"),
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_8 x^8 + \\beta_9 x^9$$"),
           withMathJax("$$\\hat{y} = \\beta_0 + \\beta_1 x + \\beta_2 x^2 + ... + \\beta_9 x^9 + \\beta_{10} x^{10}$$")
    )
  })
  
  # train data
  set.seed(1234)
  ntrain = 10
  ntest = 100
  
  x = seq(0, 1, length.out = ntrain)
  y_truth = 3*x + 2*x^2 + x^3 + 2 # model without noise
  y = y_truth + rnorm(ntrain, 0, 0.2) # model with noise
  
  # test data(to plot curve)
  x_test = seq(-0.2, 1.2, length.out = ntest)
  y_test_truth = 3*x_test + 2*x_test^2 + x_test^3 + 2
  y_test = y_test_truth + rnorm(ntest, 0, 0.2)
  
  # sample test
  sample_test = c(14, 30, 45, 70, 88)
  
  output$polyReg <- renderPlot({
    
    ## generate data
    ndeg = input$ndeg
    
    # polynomial regression
    X <- matrix(0, ncol = ndeg+1, nrow = ntrain)
    X_test <- matrix(0, ncol = ndeg+1, nrow = ntest)
    
    for(i in c(0:ndeg)){
      X[,i+1] = x^i
      X_test[,i+1] = x_test^i
    }
    
    # optimize coefficient
    beta_hat = pseudoinverse(t(X) %*% X) %*% t(X) %*% y
    
    # predicted result
    y_hat_train = as.vector(X %*% beta_hat) # training result
    y_hat_test = as.vector(X_test %*% beta_hat) # testing result
    
    # generate corresponding dataframe for ploting
    # train data frame
    dat_train = as.data.frame(X)
    dat_train$y = y
    dat_train$x = x
    dat_train$y_hat = y_hat_train
    dat_train$y_truth = y_truth
    
    # test data frame
    dat_test = as.data.frame(X_test)
    dat_test$y = y_test
    dat_test$x = x_test
    dat_test$y_hat = y_hat_test
    
    # coef data frame
    df_coef = as.data.frame(round(beta_hat,1))
    df_coef$x = 1:(ndeg+1)
    name = switch(ndeg,
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),TeX("$\\beta_3$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),TeX("$\\beta_6$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),TeX("$\\beta_6$"),TeX("$\\beta_7$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$"),TeX("$\\beta_9$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$"),
                    TeX("$\\beta_9$"),TeX("$\\beta_{10}$")))
    
    # scatter data plot/ fitted regression curve
    p1 <- ggplot(data = dat_train, aes(x=x, y=y)) +
      geom_line(aes(x=x, y=y_truth, color = "true relation"),
                linetype="dashed", size=1.5) + # add truth model curve
      geom_line(data = dat_test,
                aes(x=x_test, y=y_hat_test, color="regression result")) + # add fitted model curve
      geom_point(aes(fill="train data"),
                 size = 5,shape=21) + # add training point
      geom_point(data = dat_test[sample_test,],
                 aes(x=x, y=y, fill = "test data"),
                 shape = 21, size=5) + # add testing point
      coord_cartesian(xlim = c(0.1,1.1)) + coord_cartesian(ylim = c(1,10)) + # set display size
      theme(axis.text.x = element_text(size=rel(2)),
            axis.text.y = element_text(size=rel(2)),
            legend.position=c(0.07,0.80),
            legend.justification=c(0.5,0.5))+
      scale_fill_manual("Points",
                        breaks = c("test data",
                                   "train data"),
                        values = c("test data"="mediumseagreen",
                                   "train data"="lightcoral"))+
      scale_color_manual("Curves",
                         breaks = c("true relation",
                                    "regression result"),
                         values = c("true relation"="blue",
                                    "regression result"="red"))
    
    p2 <- ggplot(data = df_coef, aes(x=x, y=V1)) +
      geom_bar(stat = "identity",
               fill="lightblue", colour="black",
               width=0.5)+
      geom_text(aes(label=V1),
                vjust=-0.2) +
      scale_x_continuous(breaks=1:(ndeg+1),
                         labels=name) + 
      ylab("coefficient value") + xlab("")+
      theme(axis.text.x = element_text(size=rel(2)),
            axis.text.y = element_text(size=rel(1)))
    multiplot(p1,p2,cols=2)
  })
  
  output$testError <- renderPlot({
    
    ndeg = input$ndeg2
    
    # polynomial regression
    X <- matrix(0, ncol = ndeg+1, nrow = ntrain)
    X_test <- matrix(0, ncol = ndeg+1, nrow = ntest)
    
    for(i in c(0:ndeg)){
      X[,i+1] = x^i
      X_test[,i+1] = x_test^i
    }
    
    # optimize coefficient
    beta_hat = pseudoinverse(t(X) %*% X) %*% t(X) %*% y
    
    # predicted result
    y_hat_train = as.vector(X %*% beta_hat) # training result
    y_hat_test = as.vector(X_test %*% beta_hat) # testing result
    
    # generate corresponding dataframe for ploting
    # train data frame
    dat_train = as.data.frame(X)
    dat_train$y = y
    dat_train$x = x
    dat_train$y_hat = y_hat_train
    dat_train$y_truth = y_truth
    
    # test data frame
    dat_test = as.data.frame(X_test)
    dat_test$y = y_test
    dat_test$x = x_test
    dat_test$y_hat = y_hat_test
    
    # coef data frame
    df_coef = as.data.frame(round(beta_hat,1))
    df_coef$x = 1:(ndeg+1)
    name = switch(ndeg,
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),TeX("$\\beta_3$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),TeX("$\\beta_6$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),TeX("$\\beta_6$"),TeX("$\\beta_7$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$"),TeX("$\\beta_9$")),
                  c(TeX("$\\beta_0$"),TeX("$\\beta_1$"),TeX("$\\beta_2$"),
                    TeX("$\\beta_3$"),TeX("$\\beta_4$"),TeX("$\\beta_5$"),
                    TeX("$\\beta_6$"),TeX("$\\beta_7$"),TeX("$\\beta_8$"),
                    TeX("$\\beta_9$"),TeX("$\\beta_{10}$")))
    
    # scatter data plot/ fitted regression curve
    p1 <- ggplot(data = dat_train, aes(x=x, y=y)) +
      geom_line(aes(x=x, y=y_truth, color = "true relation"),
                linetype="dashed", size=1.5) + # add truth model curve
      geom_line(data = dat_test,
                aes(x=x_test, y=y_hat_test, color="regression result")) + # add fitted model curve
      geom_point(aes(fill="train data"),
                 size = 5,shape=21) + # add training point
      geom_point(data = dat_test[sample_test,],
                 aes(x=x, y=y, fill = "test data"),
                 shape = 21, size=5) + # add testing point
      coord_cartesian(xlim = c(0.1,1.1)) + coord_cartesian(ylim = c(1,10)) + # set display size
      theme(axis.text.x = element_text(size=rel(2)),
            axis.text.y = element_text(size=rel(2)),
            legend.position=c(0.07,0.80),
            legend.justification=c(0.5,0.5))+
      scale_fill_manual("Points",
                        breaks = c("test data",
                                   "train data"),
                        values = c("test data"="mediumseagreen",
                                   "train data"="lightcoral"))+
      scale_color_manual("Curves",
                        breaks = c("true relation",
                                   "regression result"),
                        values = c("true relation"="blue",
                                   "regression result"="red"))
    
    p2 <- ggplot(data=df_err, aes(x=ndeg, y=tes_error))+
      geom_point(aes(fill ="test error"),
                 shape=23, size=6)+
      geom_line()+
      geom_point(data=df_err,
                 aes(x=ndeg, y=tra_error, fill="train error"),
                 size = 6,  shape=23) +
      geom_line(data=df_err,
                aes(x=ndeg, y=tra_error)) + 
      coord_cartesian(ylim = c(0,0.75), xlim=c(1,10))+
      scale_x_continuous(breaks=-1:12,labels=-1:12) + 
      geom_point(data=df_err[input$ndeg2,],
                 aes(x=ndeg, y=tes_error,fill="current test error"),
                 shape = 23, size=8)+
      ylab("train/test error") + 
      xlab("max power of x")+
      scale_shape_manual(values = c(1,2,3))+
      theme(legend.position=c(0.07,0.85),legend.justification=c(0.5,0.5))+
      scale_fill_manual("color",
                        breaks = c("test error",
                                   "train error",
                                   "current test error"),
                        values = c("test error"="mediumseagreen",
                                   "train error"="lightcoral",
                                   "current test error"="gold"))+
      annotate("text",
               x=df_err[input$ndeg2,3],
               y=df_err[input$ndeg2,1]+0.08,
               label=sprintf("test error: %.3f\ntrain error: %.3f",df_err[input$ndeg2,1],df_err[input$ndeg2,2]),
               size = 5,
               family="Times")
    
    multiplot(p1,p2,cols=2)
  })
})
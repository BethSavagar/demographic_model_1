library(sensitivity)
library(boot)
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))

# linear model : Y = X1 + X2 + X3

y <- with(X, X1 + X2 + X3)

# sensitivity analysis
x <- src(X, y, nboot = 100, rank = T)
x <- src(var_input_set, RSAoutput %>% pull(pop_growth), nboot = 0, rank = T)
x2 <- src(valid_as_pars3_df, agesex3_growth %>% pull(pop_growth), nboot = 0, rank = T)
print(x)
plot(x)

SRRC_comparison <- rbind(x$SRRC %>% mutate(df = "valid",
                                           par = rownames(x$SRRC)), 
                         x2$SRRC %>% mutate(df = "all", 
                                            par = rownames(x2$SRRC)))

library(ggplot2)
ggplot(SRRC_comparison, aes(x=par, y=original))+geom_point(aes(col=df))

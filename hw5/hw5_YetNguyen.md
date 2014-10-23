\documentclass{article}\usepackage[]{graphicx}\usepackage[]{color}
%% maxwidth is the original width if it is less than linewidth
%% otherwise use linewidth (to make sure the graphics do not exceed the margin)
\makeatletter
\def\maxwidth{ %
  \ifdim\Gin@nat@width>\linewidth
    \linewidth
  \else
    \Gin@nat@width
  \fi
}
\makeatother




\usepackage{alltt}
\IfFileExists{upquote.sty}{\usepackage{upquote}}{}
\begin{document}



\title{HW5 STAT512 Fall2014}

\author{Yet Nguyen}
  
\maketitle


\section*{1.}

```r
library(reshape2)
library(xtable)
dat <- matrix(c(1, 360, 67, 73, 83, 89, 
                1, 370, 65, 91, 87, 86, 
                1, 380, 155, 127, 147, 212, 
                2, 380, 108, 100, 90, 153, 
                2, 370, 140, 142, 121, 150, 
                2, 360, 33, 8, 46, 54), 
              byrow = T, nrow = 6)
dat <- as.data.frame(dat)
colnames(dat) <- c("rep", "temp", paste0("coating", 1:4))
dat$temp <- as.factor(dat$temp)
dat$rep <- as.factor(dat$rep)
dat.melt <- melt(dat)
colnames(dat.melt) <- c("rep","temp", "coating", "y")

lmout <- lm(y~temp*coating , data = dat.melt)
ano <- anova(lmout)
```



```r
print(xtable(ano, caption ="ANOVA table and F-statistics, 
             degree of freedom, 
             and pvalue for the three tests."))
```

```
% latex table generated in R 3.1.1 by xtable 1.7-4 package
% Thu Oct 23 16:07:42 2014
\begin{table}[ht]
\centering
\begin{tabular}{lrrrrr}
  \hline
 & Df & Sum Sq & Mean Sq & F value & Pr($>$F) \\ 
  \hline
temp & 2 & 26519.25 & 13259.62 & 10.23 & 0.0026 \\ 
  coating & 3 & 4289.12 & 1429.71 & 1.10 & 0.3860 \\ 
  temp:coating & 6 & 3269.75 & 544.96 & 0.42 & 0.8518 \\ 
  Residuals & 12 & 15560.50 & 1296.71 &  &  \\ 
   \hline
\end{tabular}
\caption{ANOVA table and F-statistics, 
             degree of freedom, 
             and pvalue for the three tests.} 
\end{table}
```

\section*{2.}

```r
res <- aov(y ~temp * coating + Error(temp:rep), data = dat.melt)
```

Whole Plot ANOVA

```r
print(xtable(summary(res)[[1]], caption = "Whole Plot ANOVA 
             including F-test, degree of freedom,
             and p-value of the test for main 
             effect of temperature factor."))
```

```
% latex table generated in R 3.1.1 by xtable 1.7-4 package
% Thu Oct 23 16:07:42 2014
\begin{table}[ht]
\centering
\begin{tabular}{lrrrrr}
  \hline
 & Df & Sum Sq & Mean Sq & F value & Pr($>$F) \\ 
  \hline
temp      & 2 & 26519.25 & 13259.62 & 2.75 & 0.2093 \\ 
  Residuals & 3 & 14439.62 & 4813.21 &  &  \\ 
   \hline
\end{tabular}
\caption{Whole Plot ANOVA 
             including F-test, degree of freedom,
             and p-value of the test for main 
             effect of temperature factor.} 
\end{table}
```

Split-Plot ANOVA 


```r
print(xtable(summary(res)[[2]], caption = "Split-Plot ANOVA 
             including F-test, 
             degree of freedom,
             and p-value of the tests for the 
             main effect of coating and 
             interaction of temperature and coating."))
```

```
% latex table generated in R 3.1.1 by xtable 1.7-4 package
% Thu Oct 23 16:07:42 2014
\begin{table}[ht]
\centering
\begin{tabular}{lrrrrr}
  \hline
 & Df & Sum Sq & Mean Sq & F value & Pr($>$F) \\ 
  \hline
coating      & 3 & 4289.12 & 1429.71 & 11.48 & 0.0020 \\ 
  temp:coating & 6 & 3269.75 & 544.96 & 4.38 & 0.0241 \\ 
  Residuals    & 9 & 1120.87 & 124.54 &  &  \\ 
   \hline
\end{tabular}
\caption{Split-Plot ANOVA 
             including F-test, 
             degree of freedom,
             and p-value of the tests for the 
             main effect of coating and 
             interaction of temperature and coating.} 
\end{table}
```
\end{document}

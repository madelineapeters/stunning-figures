library("ggplot2", lib.loc="~/R/win-library/3.3")
library("ggthemes", lib.loc="~/R/win-library/3.3")

fig3<-ggplot(data=h_1) + xlab("s/t") + ylab("Equilibrium p") + ylim(0,1)+scale_colour_manual(values=cbpalette) +
  theme_gdocs() +
  geom_line(aes(x=`<U+FEFF>st`, y= `r = 0`), size=0.5) +
  geom_line(aes(x=`<U+FEFF>st`, y=`r = 0.25`), size=0.5) +
  geom_line(aes(x=`<U+FEFF>st`, y=`r = 0.5`), size=0.5) +
  geom_line(aes(x=`<U+FEFF>st`, y=`r = 0.75`), size=0.5) +
  geom_line(aes(x=`<U+FEFF>st`, y=`r = 1.0`), size=0.5) +
  geom_text(data = h_1[h_1$`<U+FEFF>st` == 0.5,
                       ], aes(x = `<U+FEFF>st`, y = `r = 0`, label = "r = 0"), hjust = -0.1, vjust = -17) +
  geom_text(data = h_1[h_1$`<U+FEFF>st` == 0.4,
                       ], aes(x = `<U+FEFF>st`, y = `r = 0.25`, label = "r = 0.25"), hjust = -0.2, vjust = 9) +
  geom_text(data = h_1[h_1$`<U+FEFF>st` == 0.4,
                       ], aes(x = `<U+FEFF>st`, y = `r = 0.5`, label = "r = 0.5"), hjust = 0.3, vjust = -9) +
  geom_text(data = h_1[h_1$`<U+FEFF>st` == 0.4,
                       ], aes(x = `<U+FEFF>st`, y = `r = 0.75`, label = "r = 0.75"), hjust = 1, vjust = -5) +
  geom_text(data = h_1[h_1$`<U+FEFF>st` == 0,
                       ], aes(x = `<U+FEFF>st`, y = `r = 1.0`, label = "r = 1.0"), hjust = 0, vjust = -1)
print(fig3)




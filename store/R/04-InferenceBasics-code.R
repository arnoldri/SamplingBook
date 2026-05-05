
library(dplyr)
data(starwars)
sum(is.na(starwars$height))
starwars <- starwars |> 
  mutate(height=case_when(name=="Arvel Crynyd" ~ 172, # same as Luke
                          name=="Finn" ~ 172, # same as Luke
                          name=="Rey" ~ 150,  # same as Leia
                          name=="Poe Dameron" ~ 172, # same as Luke
                          name=="BB8" ~ 96, # same as R2D2
                          name=="Captain Phasma" ~ 167, # same as C3P0
                          .default=height))



set.seed(222)
bigN <- nrow(starwars)
n <- 20
samplemembers <- starwars[sample(bigN, n, replace=FALSE),]
samplemembers |>
  dplyr::select(name,height,mass,hair_color,homeworld) |>
  kbl() |>
  kable_styling()



hist(samplemembers$height, xlab="Height (cm)", ylab="Frequency", 
     breaks=15, main="Heights of Star Wars characters")
abline(v=mean(samplemembers$height),col="red",lwd=2)



ybar <- mean(samplemembers$height)
ybar
sdy <- sd(samplemembers$height)
sdy



tstar <- qt(0.05/2, df=n-1, lower.tail=FALSE)
se <- sdy/sqrt(n)
moe <- tstar*se
ci <- ybar + moe*c(-1,1)
fpc <- sqrt(1-n/bigN)
sefpc <- se*fpc
moefpc <- moe*fpc
cifpc <- ybar + moefpc*c(-1,1)



lm1 <- lm(height ~ 1, data=samplemembers)
summary(lm1)
confint(lm1)



sigma(lm1)



samplemembers$bigN <- bigN
ss <- svymean(height~1, design=svydesign(ids=~1, fpc=~bigN, data=samplemembers))
ss
confint(ss)



ff <- data.frame(UnitID=c(1, 2, 3, "$\\vdots$", "$i$", "$\\vdots$", "N"),
                 X=c("${\\bf X}_1$","${\\bf X}_2$","${\\bf X}_3$",
                     "$\\vdots$","${\\bf X}_i$","$\\vdots$","${\\bf X}_N$"),
                 P=c("$\\pi_1$","$\\pi_2$","$\\pi_3$","$\\vdots$","$\\pi_i$","$\\vdots$","$\\pi_N$"),
                 I=c("$I_1$","$I_2$","$I_3$","$\\vdots$","$I_i$","$\\vdots$","$I_N$")
                 )
ff |>
  kbl(caption = "Sample Frame ($N$ rows)",
      col.names=c("Unit ID", "Data", "Probability", "Indicator"),
      escape=FALSE, align = 'cccc') |>
  kable_styling() |>
  add_header_above(c(" ","Auxiliary", "Selection", "Selection")) 



fs <- data.frame(Unit=c(1, 2, 3, "$\\vdots$", "$k$", "$\\vdots$", "$n$"),
                 UnitID=c("$i_1$", "$i_2$", "$i_3$", "$\\vdots$", "$i_k$", "$\\vdots$", "$i_n$"),
                 X=c("${\\bf x}_1$","${\\bf x}_2$","${\\bf x}_3$",
                     "$\\vdots$","${\\bf x}_k$","$\\vdots$","${\\bf x}_n$"),
                 P=c("$\\pi_1$","$\\pi_2$","$\\pi_3$","$\\vdots$","$\\pi_k$","$\\vdots$","$\\pi_n$"),
                 W=c("$w_1$","$w_2$","$w_3$","$\\vdots$","$w_k$","$\\vdots$","$w_n$"),
                 Y=c("${\\bf y}_1$","${\\bf y}_2$","${\\bf y}_3$",
                     "$\\vdots$","${\\bf y}_k$","$\\vdots$","${\\bf y}_n$")
                 )
fs |>
  kbl(caption = "Sample Data ($n$ rows)",
      col.names=c("Unit", "Unit ID", "Data", "Probability", "Weight", "Data"),
      escape=FALSE, align = 'ccccc') |>
  kable_styling() |>
  add_header_above(c("Sample"," ","Auxiliary", "Selection", "Sample", "Sample")) 



knitr::include_graphics("include-files/fig/Weights.png")



hframe <- data.frame(
  ID=c(6451,6452,8773,7164,6125),
  Address=c("1 Chestnut Place","2 Chestnut Place","2A Chestnut Place",
            "3 Chestnut Place","4 Chestnut Place"),
  Value=c(1.201, 0.780, 0.520, 2.530, 0.900)
)
hframe |>
  kbl(col.names=c("ID","Address","Value ($M)"), 
      align='clc') |>
  kable_styling()



hframe$prob <- rep(0.4,5)
hframe$selected <- c(0,0,1,0,1)
hframe |>
  kbl(col.names=c("ID","Address","Value (\\$M)","Prob. $\\pi_i$", "Selected, $I_i$"), 
      align='clccc', escape=FALSE) |>
  kable_styling()



dframe <- hframe[hframe$selected==1,]
dframe$weight <- 1/dframe$prob
dframe$y <- c(75, 19)
dframe$prob <- NULL
dframe$selected <- NULL
dframe |>
  kbl(col.names=c("ID","Address","Value (\\$M)","Weight $w_k$", "Internet spend, $y_k$"), 
      align='clccc', escape=FALSE) |>
  kable_styling()



n <- 2
N <- 5
ybar <- mean(dframe$y)
sy <- sd(dframe$y)
se <- sqrt(1-n/N)*sy/sqrt(n)
tstar <- abs(qt(0.025,n-1))
tstar <- 1.96
moe <- tstar*se
ci <- ybar + moe*c(-1,1)
ytot <- N*ybar
citot <- N*ci



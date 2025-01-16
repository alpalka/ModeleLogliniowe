ankieta <- read.csv2(file="Ankieta.csv",header=TRUE)

#zadanie 1
ankieta_f=ftable(ankieta)
ankieta.df <- as.data.frame(as.table(ankieta_f))
ankieta.df[,-4] <- lapply(ankieta.df[,-4], relevel, ref = "0") #ustawiamy referencje na "no"
ankieta.df

#model [12 3]
model1 <- glm(Freq ~ SEN + BIEGANIE + PIES + 
                SEN*BIEGANIE, 
              data = ankieta.df, family = poisson)
summary(model1)
1-pchisq(deviance(model1), df = df.residual(model1))
# porownujemy licznosci z modelem
cbind(model1$data, fitted(model1))
model1$data
  

#model [12 23]
model2 <- glm(Freq ~ SEN + BIEGANIE + PIES + 
                SEN*BIEGANIE+BIEGANIE*PIES, 
              data = ankieta.df, family = poisson)
summary(model2)
cbind(model1$data, fitted(model1),fitted(model2))
1-pchisq(deviance(model2), df = df.residual(model2))
# porownujemy licznosci z modelem
cbind(model2$data, fitted(model2))


#model [12 23 13]
model3 <- glm(Freq ~ SEN + BIEGANIE + PIES + 
                SEN*BIEGANIE+BIEGANIE*PIES+SEN*PIES, 
              data = ankieta.df, family = poisson)
summary(model3)
1-pchisq(deviance(model3), df = df.residual(model3))
# porownujemy licznosci z modelem
cbind(model3$data, fitted(model3))



#zadanie 2
#a
#P(sen=1|bieganie=1)
#sen=1 i bieganei=1 -> kolumna 4 i 8 
#bieganie=1 _>kolumny 3,4,7,8
licznosciModel1 = cbind(model1$data, fitted(model1))[[5]]
licznosciModel2 = cbind(model2$data, fitted(model2))[[5]]
licznosci = model1$data[[4]]

licznosciModel1
licznosciModel2
licznosci

pa_model1 = (licznosciModel1[4]+licznosciModel1[8])/(licznosciModel1[4]+licznosciModel1[8]+licznosciModel1[3]+licznosciModel1[7])
pa_model2 = (licznosciModel2[4]+licznosciModel2[8])/(licznosciModel2[4]+licznosciModel2[8]+licznosciModel2[3]+licznosciModel2[7])
pa_ = (licznosci[4]+licznosci[8])/(licznosci[4]+licznosci[8]+licznosci[3]+licznosci[7])

pa_model1
pa_model2
pa_

#b
#P(biega=1|pies=1)
#pies=1 i bieganie=1 ->kolumny 7 i 8
#pies=1 _> kolumny 5,6,7,8

pb_model1 = (licznosciModel1[7]+licznosciModel1[8])/(licznosciModel1[5]+licznosciModel1[8]+licznosciModel1[6]+licznosciModel1[7])
pb_model2 = (licznosciModel2[7]+licznosciModel2[8])/(licznosciModel2[5]+licznosciModel2[8]+licznosciModel2[6]+licznosciModel2[7])
pb_ = (licznosci[7]+licznosci[8])/(licznosci[6]+licznosci[8]+licznosci[5]+licznosci[7])

pb_model1
pb_model2
pb_


#zadanie 3
#a      H0:[1 2 3]
#H1 : [123]
#H1 : [12 23 32]

model_a <- glm(Freq ~ SEN + BIEGANIE + PIES , 
              data = ankieta.df, family = poisson)
summary(model_a)
1-pchisq(deviance(model_a), df = df.residual(model_a))

model_h1 <- glm(Freq ~ (SEN + BIEGANIE + PIES)^2 , 
               data = ankieta.df, family = poisson)
test_a <- anova(model_a,model_h1)
1-pchisq(test_a$Deviance[2], df = test_a$Df[2])
#b      H0:[12 3]
#H1 : [123]
#H1 : [12 23 32]
model_b <- glm(Freq ~ SEN + BIEGANIE + PIES + SEN*BIEGANIE, 
               data = ankieta.df, family = poisson)
summary(model_b)
1-pchisq(deviance(model_b), df = df.residual(model_b))


test_b <- anova(model_b,model_h1)
1-pchisq(test_b$Deviance[2], df = test_b$Df[2])
#c     H0:[12 32]
#H1 : [123] i nie [12 23]
#H1 : [12 23 32] i nie [12 23]
model_c <- glm(Freq ~ SEN*BIEGANIE + PIES*BIEGANIE, 
               data = ankieta.df, family = poisson)
summary(model_c)
model_pelny <- glm(Freq ~ (SEN + BIEGANIE + PIES)^2 +SEN*BIEGANIE*PIES, 
                   data = ankieta.df, family = poisson)
1-pchisq(deviance(model_c)-deviance(model_pelny), df = df.residual(model_c)-df.residual(model_pelny))

1-pchisq(deviance(model_c)-deviance(model_h1), df = df.residual(model_c)-df.residual(model_h1))


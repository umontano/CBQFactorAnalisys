#TIENES QUE CARGAR LOS DATOS: Karen_base.sav 
#Karen_base <- read_sav("Manuscrito-Temperamento-Adquisición-Desarrollo/DATA/BLINDNESSkaren/DATOS/Karen-base.sav")
#save.image("~/Box Sync/Dropbox/SY2/SYNC2SHARED/Manuscrito-Temperamento-Adquisición-Desarrollo/DATA/R-Files/HISTORYANDSESSIONS/KAREN_TTEST.RData")
#load("~/Box Sync/Dropbox/SY2/SYNC2SHARED/Manuscrito-Temperamento-Adquisición-Desarrollo/DATA/R-Files/HISTORYANDSESSIONS/KAREN_TTEST.RData")

#CAMBIA LOS DATOS DE NUMERIC A FACTOR
Karen_base$CEAltosBajos <- as.factor(Karen_base$CEAltosBajos)

################T TESTS
# T TEST BETWEEN GROUPS 0 AND 1
t.test(Karen_base$ConteocorrectoyTarget1213 ~ Karen_base$CEAltosBajos,
          alternative="two.sided")

#T TESTS grupo 1; CE ALTO
t.test(Karen_base$ConteocorrectoyTarget1213[Karen_base$CEAltosBajos==1]
       , mu=0.5, alternative="two.sided")
#T TESTS grupo 0; CE BAJO
t.test(Karen_base$ConteocorrectoyTarget1213[Karen_base$CEAltosBajos==0]
       , mu=0.5, alternative="two.sided")


################# BINOMIAL TESTS
#GENERATEs CONTINGENCY TABLE FOR GROUP 0, LOW EFFORTFUL CONTROL
tabLOW <- table(Karen_base$ConteocorrectoyTarget1213[
      Karen_base$CEAltosBajos==0] )
#BINOMIAL TEST for GRUOP 0, LOW EC, TWO SIDED
binom.test(tabLOW, p=0.5, alternative="two.sided", conf.level=0.95)


#GENERATEs CONTINGENCY TABLE FOR GROUP 1, HIGH EFFORTFUL CONTROL
tabHIGH <- table(Karen_base$ConteocorrectoyTarget1213[
  Karen_base$CEAltosBajos==1] )
#BINOMIAL TEST for GRUOP 1, HIGH EC, TWO SIDED
binom.test(tabHIGH, p=0.5, alternative="two.sided", conf.level=0.95)


################## CHI SQUARE TESTS
#CHISQUARE TEST GROUP 0, LOW EC
chisq.test(tabLOW, correct=T)
#CHISQUARE TEST GROUP 1, HIGH EC
chisq.test(tabHIGH, correct=T)

###########TERMINA AQUI
###########TERMINA AQUI
###########TERMINA AQUI
barplot(tabLOW)
barplot(tabHIGH)

library(readxl)
lfsdt= read_excel("local food Sys.xlsx", skip = 1, sheet = "Numerical")

library(plyr)
lfs_prom= mapvalues(lfsdt$`Timely promot`, from = c("1", "2", "3", "4", "5"), 
          to = c("0", "0", "0", "1", "1")) 
idl_lnd= mapvalues(lfsdt$`Idle land agri`, from = c("1", "2", "3", "4", "5"), 
                   to = c("0", "0", "0", "1", "1")) 
involv= mapvalues(lfsdt$Involvement1, from=c("1", "2"), 
                  to = c("1", "0"))
ptcipa= mapvalues(lfsdt$Participation1, from=c("1", "2"), 
                  to = c("1", "0"))


# model own gardeing  
library(ordinal)
own_prtice= clm(factor(lfsdt$`Own Practise1`) ~ Age1+ Gender1+ Residence1+ Education1+ 
                  Occupation1+ Income1+ lfsdt$`Labour Shortage1`+ lfsdt$`Market Fluctuation1`+
                  lfsdt$`Food Shortage1`+ lfsdt$`Price Change1`+ lfsdt$`Food Quality1`+
                  lfsdt$`Stable Supply1`+ lfsdt$Affordability1, data = lfsdt)

summary(own_prtice)

#Model involvement 
dtinv= cbind(lfsdt, involv, ptcipa, lfs_prom)
invlv= glm(factor(involv)~ Age1+ Gender1+ Residence1+ Education1+ 
              Occupation1+ Income1+ lfsdt$`Labour Shortage1`+ lfsdt$`Market Fluctuation1`+
             lfsdt$`Food Shortage1`+ lfsdt$`Price Change1`+ lfsdt$`Food Quality1`+
             lfsdt$`Stable Supply1`+ lfsdt$Affordability1, data = dtinv, family = "binomial")

summary(invlv)

# Model participation
particp= glm(factor(ptcipa)~ Age1+ Gender1+ Residence1+ Education1+ 
               Occupation1+ Income1+ lfsdt$`Labour Shortage1`+ lfsdt$`Market Fluctuation1`+
               lfsdt$`Food Shortage1`+ lfsdt$`Price Change1`+ lfsdt$`Food Quality1`+
               lfsdt$`Stable Supply1`+ lfsdt$Affordability1, data = dtinv, family = "binomial")
summary(particp)

# Model government promotion of local food production

gov_prom_lfs=glm(factor(lfs_prom)~ Age1+ Gender1+ Residence1+ Education1+ 
      Occupation1+ Income1+ lfsdt$`Labour Shortage1`+ lfsdt$`Market Fluctuation1`+
      lfsdt$`Food Shortage1`+ lfsdt$`Price Change1`+ lfsdt$`Food Quality1`+
      lfsdt$`Stable Supply1`+ lfsdt$Affordability1, data = dtinv, family = "binomial")
summary(gov_prom_lfs)



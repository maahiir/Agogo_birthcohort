library(bigmemory)
library(ff)
library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(psych) # load libraries
library(nFactors)
library(reshape2)
library(ggplot2)
library(dplyr)
library(GPArotation)
library(corrplot)
library(Hmisc)
library(factoextra)
library(usethis)
library(freqtables)
library(here)
library(janitor)
library(labelled)
library(gtsummary)
library(jtools)
#install.packages("Hmisc")
#install.packages("jtools")


Sys.setenv("LANGUAGE"= "EN")
rm(doPlots)
rm(Agogo_MahirPCA)

Agogo_MahirPCA<- read.csv(file="agogo_2015_Sophie_PCA.csv",header=TRUE,sep = ";",na.strings=c("NA","NaN", " "), dec = ",")
nodiet<-read.csv(file="agogo_2000_2015_cohort_NoDietPA.csv",header = T,sep=";",na.strings = c("NA","NaN",""),dec = ",")

mothers_age<-nodiet[,c(1:2,7,30)]
mothers_age<-mothers_age%>%arrange(id)

Agogo_MahirPCA<-Agogo_MahirPCA[(-201:-205),]
Agogo_MahirPCA<- Agogo_MahirPCA%>%arrange(id)

head(Agogo_MahirPCA)

head(Agogo_MahirPCA$plantain_3_fingers)

#Converting responses from food frequency questionnaire in to kilo calories per day

Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cassava_day = case_when(cassava == 1 ~ (0.5/7)*150,
                                 cassava == 2 ~ (1.5/7)*150,
                                 cassava == 3 ~ (3.5/7)*150,
                                 cassava == 4 ~ (5.5/7)*150,
                                 cassava == 5 ~ 1*150))
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(plantain_day = case_when(plantain_3_fingers== 1 ~ (0.5/7)*150,
                                  plantain_3_fingers == 2 ~ (1.5/7)*150,
                                  plantain_3_fingers == 3 ~ (3.5/7)*150,
                                  plantain_3_fingers == 4 ~ (5.5/7)*150,
                                  plantain_3_fingers == 5 ~ 1*150))
head(Agogo_MahirPCA$plantain_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cocoyam_day = case_when(cocoyam == 1 ~ (0.5/7)*150,
                                 cocoyam == 2 ~ (1.5/7)*150,
                                 cocoyam == 3 ~ (3.5/7)*150,
                                 cocoyam == 4 ~ (5.5/7)*150,
                                 cocoyam == 5 ~ 1*150))
head(Agogo_MahirPCA$cocoyam_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(yam_day = case_when(yam == 1 ~ (0.5/7)*150,
                             yam == 2 ~ (1.5/7)*150,
                             yam == 3 ~ (3.5/7)*150,
                             yam == 4 ~ (5.5/7)*150,
                             yam == 5 ~ 1*150))
head(Agogo_MahirPCA$yam_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(sweet_potato_day = case_when(sweet_potato == 1 ~ (0.5/7)*200,
                                      sweet_potato == 2 ~ (1.5/7)*200,
                                      sweet_potato == 3 ~ (3.5/7)*200,
                                      sweet_potato == 4 ~ (5.5/7)*200,
                                      sweet_potato == 5 ~ 1*200))
head(Agogo_MahirPCA$sweet_potato_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(fufu_day = case_when(fufu == 1 ~ (0.5/7)*194,
                              fufu == 2 ~ (1.5/7)*194,
                              fufu == 3 ~ (3.5/7)*194,
                              fufu == 4 ~ (5.5/7)*194,
                              fufu == 5 ~ 1*194))
head(Agogo_MahirPCA$fufu_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(banku_day = case_when(banku == 1 ~ (0.5/7)*200,
                               banku == 2 ~ (1.5/7)*200,
                               banku == 3 ~ (3.5/7)*200,
                               banku == 4 ~ (5.5/7)*200,
                               banku == 5 ~ 1*200))
head(Agogo_MahirPCA$banku_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(kenkey_day = case_when(kenkey == 1 ~ (0.5/7)*200,
                                kenkey == 2 ~ (1.5/7)*200,
                                kenkey == 3 ~ (3.5/7)*200,
                                kenkey == 4 ~ (5.5/7)*200,
                                kenkey == 5 ~ 1*200))
head(Agogo_MahirPCA$kenkey_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(millet_day = case_when(millet == 1 ~ (0.5/7)*120,
                                millet == 2 ~ (1.5/7)*120,
                                millet == 3 ~ (3.5/7)*120,
                                millet == 4 ~ (5.5/7)*120,
                                millet == 5 ~ 1*120))
head(Agogo_MahirPCA$millet_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(oats_day = case_when(oats == 1 ~ (0.5/7)*225,
                              oats == 2 ~ (1.5/7)*225,
                              oats == 3 ~ (3.5/7)*225,
                              oats == 4 ~ (5.5/7)*225,
                              oats == 5 ~ 1*225))
head(Agogo_MahirPCA$oats_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cornfla_day = case_when(cornfla == 1 ~ (0.5/7)*150,
                                 cornfla == 2 ~ (1.5/7)*150,
                                 cornfla == 3 ~ (3.5/7)*150,
                                 cornfla == 4 ~ (5.5/7)*150,
                                 cornfla == 5 ~ 1*150))
head(Agogo_MahirPCA$cornfla_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(rice_day = case_when(rice == 1 ~ (0.5/7)*134,
                              rice == 2 ~ (1.5/7)*134,
                              rice == 3 ~ (3.5/7)*134,
                              rice == 4 ~ (5.5/7)*134,
                              rice == 5 ~ 1*134))
head(Agogo_MahirPCA$rice_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(bread_day = case_when(bread == 1 ~ (0.5/7)*60,
                               bread == 2 ~ (1.5/7)*60,
                               bread == 3 ~ (3.5/7)*60,
                               bread == 4 ~ (5.5/7)*60,
                               bread == 5 ~ 1*60))
head(Agogo_MahirPCA$bread_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(noodl_day = case_when(noodl == 1 ~ (0.5/7)*75,
                               noodl == 2 ~ (1.5/7)*75,
                               noodl == 3 ~ (3.5/7)*75,
                               noodl == 4 ~ (5.5/7)*75,
                               noodl == 5 ~ 1*75))
head(Agogo_MahirPCA$noodl_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(fish_day = case_when(fish == 1 ~ (0.5/7)*100,
                              fish == 2 ~ (1.5/7)*100,
                              fish == 3 ~ (3.5/7)*100,
                              fish == 4 ~ (5.5/7)*100,
                              fish == 5 ~ 1*100))
head(Agogo_MahirPCA$fish_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(r_meat_day = case_when(r_meat == 1 ~ (0.5/7)*100,
                                r_meat == 2 ~ (1.5/7)*100,
                                r_meat == 3 ~ (3.5/7)*100,
                                r_meat == 4 ~ (5.5/7)*100,
                                r_meat == 5 ~ 1*100))
head(Agogo_MahirPCA$r_meat_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(sausag_day = case_when(sausag == 1 ~ (0.5/7)*90,
                                sausag == 2 ~ (1.5/7)*90,
                                sausag == 3 ~ (3.5/7)*90,
                                sausag == 4 ~ (5.5/7)*90,
                                sausag == 5 ~ 1*90))
head(Agogo_MahirPCA$sausag_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(p_meat_day = case_when(p_meat == 1 ~ (0.5/7)*108,
                                p_meat == 2 ~ (1.5/7)*108,
                                p_meat == 3 ~ (3.5/7)*108,
                                p_meat == 4 ~ (5.5/7)*108,
                                p_meat == 5 ~ 1*108))
head(Agogo_MahirPCA$p_meat_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(egg_day = case_when(egg == 1 ~ (0.5/7)*60,
                             egg == 2 ~ (1.5/7)*60,
                             egg == 3 ~ (3.5/7)*60,
                             egg == 4 ~ (5.5/7)*60,
                             egg == 5 ~ 1*60))
head(Agogo_MahirPCA$egg_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(milk_day = case_when(milk == 1 ~ (0.5/7)*150,
                              milk == 2 ~ (1.5/7)*150,
                              milk == 3 ~ (3.5/7)*150,
                              milk == 4 ~ (5.5/7)*150,
                              milk == 5 ~ 1*150))
head(Agogo_MahirPCA$milk_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(yogh_day = case_when(yogh == 1 ~ (0.5/7)*150,
                              yogh == 2 ~ (1.5/7)*150,
                              yogh == 3 ~ (3.5/7)*150,
                              yogh == 4 ~ (5.5/7)*150,
                              yogh == 5 ~ 1*150))
head(Agogo_MahirPCA$yogh_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(chees_day = case_when(chees == 1 ~ (0.5/7)*75,
                               chees == 2 ~ (1.5/7)*75,
                               chees == 3 ~ (3.5/7)*75,
                               chees == 4 ~ (5.5/7)*75,
                               chees == 5 ~ 1*75))
head(Agogo_MahirPCA$chees_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(crab_day = case_when(crab == 1 ~ (0.5/7)*100,
                              crab == 2 ~ (1.5/7)*100,
                              crab == 3 ~ (3.5/7)*100,
                              crab == 4 ~ (5.5/7)*100,
                              crab == 5 ~ 1*100))
head(Agogo_MahirPCA$crab_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(snails_day = case_when(snails == 1 ~ (0.5/7)*61.4,
                                snails == 2 ~ (1.5/7)*61.4,
                                snails == 3 ~ (3.5/7)*61.4,
                                snails == 4 ~ (5.5/7)*61.4,
                                snails == 5 ~ 1*61.4))
head(Agogo_MahirPCA$snails_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(beans_day = case_when(beans == 1 ~ (0.5/7)*150,
                               beans == 2 ~ (1.5/7)*150,
                               beans == 3 ~ (3.5/7)*150,
                               beans == 4 ~ (5.5/7)*150,
                               beans == 5 ~ 1*150))
head(Agogo_MahirPCA$beans_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(groundnuts_day = case_when(groundnuts == 1 ~ (0.5/7)*20,
                                    groundnuts == 2 ~ (1.5/7)*20,
                                    groundnuts == 3 ~ (3.5/7)*20,
                                    groundnuts == 4 ~ (5.5/7)*20,
                                    groundnuts == 5 ~ 1*20))
head(Agogo_MahirPCA$groundnuts_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(lent_day = case_when(lent == 1 ~ (0.5/7)*150,
                              lent == 2 ~ (1.5/7)*150,
                              lent == 3 ~ (3.5/7)*150,
                              lent == 4 ~ (5.5/7)*150,
                              lent == 5 ~ 1*150))
head(Agogo_MahirPCA$lent_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(peas_day = case_when(peas == 1 ~ (0.5/7)*150,
                              peas == 2 ~ (1.5/7)*150,
                              peas == 3 ~ (3.5/7)*150,
                              peas == 4 ~ (5.5/7)*150,
                              peas == 5 ~ 1*150))
head(Agogo_MahirPCA$peas_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(nuts_day = case_when(nuts == 1 ~ (0.5/7)*20,
                              nuts == 2 ~ (1.5/7)*20,
                              nuts == 3 ~ (3.5/7)*20,
                              nuts == 4 ~ (5.5/7)*20,
                              nuts == 5 ~ 1*20))
head(Agogo_MahirPCA$nuts_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(seeds_day = case_when(seeds == 1 ~ (0.5/7)*80,
                               seeds == 2 ~ (1.5/7)*80,
                               seeds == 3 ~ (3.5/7)*80,
                               seeds == 4 ~ (5.5/7)*80,
                               seeds == 5 ~ 1*80))
head(Agogo_MahirPCA$seeds_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(orange_day = case_when(orange == 1 ~ (0.5/7)*150,
                                orange == 2 ~ (1.5/7)*150,
                                orange == 3 ~ (3.5/7)*150,
                                orange == 4 ~ (5.5/7)*150,
                                orange == 5 ~ 1*150))
head(Agogo_MahirPCA$orange_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(mango_day = case_when(mango == 1 ~ (0.5/7)*150,
                               mango == 2 ~ (1.5/7)*150,
                               mango == 3 ~ (3.5/7)*150,
                               mango == 4 ~ (5.5/7)*150,
                               mango == 5 ~ 1*150))
head(Agogo_MahirPCA$mango_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(pawpaw_day = case_when(pawpaw == 1 ~ (0.5/7)*150,
                                pawpaw == 2 ~ (1.5/7)*150,
                                pawpaw == 3 ~ (3.5/7)*150,
                                pawpaw == 4 ~ (5.5/7)*150,
                                pawpaw == 5 ~ 1*150))
head(Agogo_MahirPCA$pawpaw_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(pineapple_day = case_when(pineapple == 1 ~ (0.5/7)*150,
                                   pineapple == 2 ~ (1.5/7)*150,
                                   pineapple == 3 ~ (3.5/7)*150,
                                   pineapple == 4 ~ (5.5/7)*150,
                                   pineapple == 5 ~ 1*150))
head(Agogo_MahirPCA$pineapple_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(banana_day = case_when(banana == 1 ~ (0.5/7)*100,
                                banana == 2 ~ (1.5/7)*100,
                                banana == 3 ~ (3.5/7)*100,
                                banana == 4 ~ (5.5/7)*100,
                                banana == 5 ~ 1*100))
head(Agogo_MahirPCA$banana_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(avocado_day = case_when(avocado == 1 ~ (0.5/7)*150,
                                 avocado == 2 ~ (1.5/7)*150,
                                 avocado == 3 ~ (3.5/7)*150,
                                 avocado == 4 ~ (5.5/7)*150,
                                 avocado == 5 ~ 1*150))
head(Agogo_MahirPCA$avocado_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(apple_day = case_when(apple == 1 ~ (0.5/7)*125,
                               apple == 2 ~ (1.5/7)*125,
                               apple == 3 ~ (3.5/7)*125,
                               apple == 4 ~ (5.5/7)*125,
                               apple == 5 ~ 1*125))
head(Agogo_MahirPCA$apple_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(watermelone_day = case_when(watermelone == 1 ~ (0.5/7)*250,
                                     watermelone == 2 ~ (1.5/7)*250,
                                     watermelone == 3 ~ (3.5/7)*250,
                                     watermelone == 4 ~ (5.5/7)*250,
                                     watermelone == 5 ~ 1*250))
head(Agogo_MahirPCA$watermelone_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(grapes_day = case_when(grapes == 1 ~ (0.5/7)*100,
                                grapes == 2 ~ (1.5/7)*100,
                                grapes == 3 ~ (3.5/7)*100,
                                grapes == 4 ~ (5.5/7)*100,
                                grapes == 5 ~ 1*100))
head(Agogo_MahirPCA$grapes_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(tomato_day = case_when(tomato == 1 ~ (0.5/7)*125,
                                tomato == 2 ~ (1.5/7)*125,
                                tomato == 3 ~ (3.5/7)*125,
                                tomato == 4 ~ (5.5/7)*125,
                                tomato == 5 ~ 1*125))
head(Agogo_MahirPCA$tomato_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(s_pepper_day = case_when(s_pepper == 1 ~ (0.5/7)*80,
                                  s_pepper == 2 ~ (1.5/7)*80,
                                  s_pepper == 3 ~ (3.5/7)*80,
                                  s_pepper == 4 ~ (5.5/7)*80,
                                  s_pepper == 5 ~ 1*80))
head(Agogo_MahirPCA$s_pepper_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(g_egg_day = case_when(g_egg == 1 ~ (0.5/7)*100,
                               g_egg == 2 ~ (1.5/7)*100,
                               g_egg == 3 ~ (3.5/7)*100,
                               g_egg == 4 ~ (5.5/7)*100,
                               g_egg == 5 ~ 1*100))
head(Agogo_MahirPCA$g_egg_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(okro_day = case_when(okro == 1 ~ (0.5/7)*100,
                              okro == 2 ~ (1.5/7)*100,
                              okro == 3 ~ (3.5/7)*100,
                              okro == 4 ~ (5.5/7)*100,
                              okro == 5 ~ 1*100))
head(Agogo_MahirPCA$okro_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(g_leaves_day = case_when(g_leaves == 1 ~ (0.5/7)*100,
                                  g_leaves == 2 ~ (1.5/7)*100,
                                  g_leaves == 3 ~ (3.5/7)*100,
                                  g_leaves == 4 ~ (5.5/7)*100,
                                  g_leaves == 5 ~ 1*100))
head(Agogo_MahirPCA$g_leaves_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(carrot_day = case_when(carrot == 1 ~ (0.5/7)*150,
                                carrot == 2 ~ (1.5/7)*150,
                                carrot == 3 ~ (3.5/7)*150,
                                carrot == 4 ~ (5.5/7)*150,
                                carrot == 5 ~ 1*150))
head(Agogo_MahirPCA$carrot_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cucumber_day = case_when(cucumber == 1 ~ (0.5/7)*150,
                                  cucumber == 2 ~ (1.5/7)*150,
                                  cucumber == 3 ~ (3.5/7)*150,
                                  cucumber == 4 ~ (5.5/7)*150,
                                  cucumber == 5 ~ 1*150))
head(Agogo_MahirPCA$cucumber_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(lettuce_day = case_when(lettuce == 1 ~ (0.5/7)*30,
                                 lettuce == 2 ~ (1.5/7)*30,
                                 lettuce == 3 ~ (3.5/7)*30,
                                 lettuce == 4 ~ (5.5/7)*30,
                                 lettuce == 5 ~ 1*30))
head(Agogo_MahirPCA$lettuce_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cabbage_day = case_when(cabbage == 1 ~ (0.5/7)*150,
                                 cabbage == 2 ~ (1.5/7)*150,
                                 cabbage == 3 ~ (3.5/7)*150,
                                 cabbage == 4 ~ (5.5/7)*150,
                                 cabbage == 5 ~ 1*150))
head(Agogo_MahirPCA$cabbage_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(cauliflower_day = case_when(cauliflower == 1 ~ (0.5/7)*150,
                                     cauliflower == 2 ~ (1.5/7)*150,
                                     cauliflower == 3 ~ (3.5/7)*150,
                                     cauliflower == 4 ~ (5.5/7)*150,
                                     cauliflower == 5 ~ 1*150))
head(Agogo_MahirPCA$cauliflower_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(onion_day = case_when(onion == 1 ~ (0.5/7)*50,
                               onion == 2 ~ (1.5/7)*50,
                               onion == 3 ~ (3.5/7)*50,
                               onion == 4 ~ (5.5/7)*50,
                               onion == 5 ~ 1*50))
head(Agogo_MahirPCA$onion_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(palm_oil_day = case_when(palm_oil == 1 ~ (0.5/7)*18,
                                  palm_oil == 2 ~ (1.5/7)*18,
                                  palm_oil == 3 ~ (3.5/7)*18,
                                  palm_oil == 4 ~ (5.5/7)*18,
                                  palm_oil == 5 ~ 1*18))
head(Agogo_MahirPCA$palm_oil_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(veg_oil_day = case_when(veg_oil == 1 ~ (0.5/7)*18,
                                 veg_oil == 2 ~ (1.5/7)*18,
                                 veg_oil == 3 ~ (3.5/7)*18,
                                 veg_oil == 4 ~ (5.5/7)*18,
                                 veg_oil == 5 ~ 1*18))
head(Agogo_MahirPCA$veg_oil_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(olive_oil_day = case_when(olive_oil == 1 ~ (0.5/7)*18,
                                   olive_oil == 2 ~ (1.5/7)*18,
                                   olive_oil == 3 ~ (3.5/7)*18,
                                   olive_oil == 4 ~ (5.5/7)*18,
                                   olive_oil == 5 ~ 1*18))
head(Agogo_MahirPCA$olive_oil_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(margarine_day = case_when(margarine == 1 ~ (0.5/7)*8,
                                   margarine == 2 ~ (1.5/7)*8,
                                   margarine == 3 ~ (3.5/7)*8,
                                   margarine == 4 ~ (5.5/7)*8,
                                   margarine == 5 ~ 1*8))
head(Agogo_MahirPCA$margarine_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(s_butter_day = case_when(s_butter == 1 ~ (0.5/7)*7,
                                  s_butter == 2 ~ (1.5/7)*7,
                                  s_butter == 3 ~ (3.5/7)*7,
                                  s_butter == 4 ~ (5.5/7)*7,
                                  s_butter == 5 ~ 1*7))
head(Agogo_MahirPCA$s_butter_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(groundnut_paste_day = case_when(groundnut_paste == 1 ~ (0.5/7)*45,
                                         groundnut_paste == 2 ~ (1.5/7)*45,
                                         groundnut_paste == 3 ~ (3.5/7)*45,
                                         groundnut_paste == 4 ~ (5.5/7)*45,
                                         groundnut_paste == 5 ~ 1*45))
head(Agogo_MahirPCA$groundnut_paste_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(r_pepper_day = case_when(r_pepper == 1 ~ (0.5/7)*4,
                                  r_pepper == 2 ~ (1.5/7)*4,
                                  r_pepper == 3 ~ (3.5/7)*4,
                                  r_pepper == 4 ~ (5.5/7)*4,
                                  r_pepper == 5 ~ 1*4))
head(Agogo_MahirPCA$r_pepper_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(b_pepper_day = case_when(b_pepper == 1 ~ (0.5/7)*7,
                                  b_pepper == 2 ~ (1.5/7)*7,
                                  b_pepper == 3 ~ (3.5/7)*7,
                                  b_pepper == 4 ~ (5.5/7)*7,
                                  b_pepper == 5 ~ 1*7))
head(Agogo_MahirPCA$b_pepper_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(sugar_day = case_when(sugar == 1 ~ (0.5/7)*3,
                               sugar == 2 ~ (1.5/7)*3,
                               sugar == 3 ~ (3.5/7)*3,
                               sugar == 4 ~ (5.5/7)*3,
                               sugar == 5 ~ 1*3))
head(Agogo_MahirPCA$sugar_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(ketchup_day = case_when(ketchup == 1 ~ (0.5/7)*20,
                                 ketchup == 2 ~ (1.5/7)*20,
                                 ketchup == 3 ~ (3.5/7)*20,
                                 ketchup == 4 ~ (5.5/7)*20,
                                 ketchup == 5 ~ 1*20))
head(Agogo_MahirPCA$ketchup_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(salad_cream_day = case_when(salad_cream == 1 ~ (0.5/7)*8,
                                     salad_cream == 2 ~ (1.5/7)*8,
                                     salad_cream == 3 ~ (3.5/7)*8,
                                     salad_cream == 4 ~ (5.5/7)*8,
                                     salad_cream == 5 ~ 1*8))
head(Agogo_MahirPCA$salad_cream_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(chocolate_day = case_when(chocolate == 1 ~ (0.5/7)*50,
                                   chocolate == 2 ~ (1.5/7)*50,
                                   chocolate == 3 ~ (3.5/7)*50,
                                   chocolate == 4 ~ (5.5/7)*50,
                                   chocolate == 5 ~ 1*50))
head(Agogo_MahirPCA$chocolate_day)
Agogo_MahirPCA<-Agogo_MahirPCA %>%
  mutate(ice_cream_day = case_when(ice_cream == 1 ~ (0.5/7)*54,
                                   ice_cream == 2 ~ (1.5/7)*54,
                                   ice_cream == 3 ~ (3.5/7)*54,
                                   ice_cream == 4 ~ (5.5/7)*54,
                                   ice_cream == 5 ~ 1*54))
head(Agogo_MahirPCA$ice_cream_day)

pca<-Agogo_MahirPCA[,c(1,253:315)]


KMO(pca[,c(-1)])  ###kaiser - Meyer - olkin test for sampling adequacy


####alternative dietary pattern score####

fr_pca<-Agogo_MahirPCA[,c(1,187:242,245,246,247,249,250,251,252)]
stfr_pca<-scale(fr_pca[,c(-1)])
ps_fr<-stfr_pca%*%factor_four_matrix
ps_fr <- data.frame(ID = fr_pca[, c(1)], ps_fr)
dm_ag<-Agogo_MahirPCA[,c(1:74)]
dm_ag<-left_join(dm_ag,pattern_scores,by=c("id"="ID"))
dm_ag<-dm_ag%>%mutate(edu_mo=na_if(edu_mo,"#NULL!"))
dm_ag<-dm_ag%>%mutate(mps_pos=na_if(mps_pos,"#NULL!"))
dm_ag$edu_mo<-as.factor(dm_ag$edu_mo)
dm_ag$mps_pos<-as.factor(dm_ag$mps_pos)
dm_rg<-subset(dm_ag,is.na(mps_pos)==F)
summary(lm(ML3~mps_pos,data = dm_rg))
pattern_scores<-pattern_scores[(-1:-5),]
factor_four_fr <- fa(fr_pca [,c(-1)] , nfactors = 4, rotate = "varimax", fm= "ml")
factor_four_fr
factor_four_fr<-as.matrix(factor_four_fr$loadings)
loadings<-unclass(factor_four_fr$loadings)
loadings<-as.data.frame(loadings)
newAgogo_final<-Agogo_MahirPCA[,c(1:74)]
newAgogo_final<-left_join(newAgogo_final,ps_fr,by=c("id"="ID")) #join the pattern scores with the subset data frame for analysis
newAgogo_final<-left_join(newAgogo_final,mothers_age,by=c("id"="id"))

write_csv2(loadings,"D:/Lenovo data/Masterarbeit publikation/Data/Thesis anaylsis/Agogo_birthcohort/fa.csv")

write.csv2(loadings,"D:/Lenovo data/Masterarbeit publikation/Data/Thesis anaylsis/Agogo_birthcohort/fa.csv", row.names=T)

####factor analysis####

factor_four <- fa(pca [,c(-1)] , nfactors = 4, rotate = "varimax", fm= "ml")
factor_four
factor_four_matrix <- as.matrix(factor_four$loadings) # create a matrix of factor loading 


std_pca<-scale(pca[,c(-1)]) #standardize the food intake in kilo calories per day n(0,1)

pattern_scores<-std_pca%*%factor_four_matrix #matrix multiplication to calculate dietary pattern score for each individual

pattern_scores <- data.frame(ID = pca[, c(1)], pattern_scores) #bind the id column to the pattern scores 

#manual calculation to check if the matrix multiplication for pattern score calculation worked or not
ff1<-std_pca*f1$f1[match(names(std_pca), f1$newColName)][col(std_pca)]
ff1$pat_sc<-rowSums(ff1,na.rm=T)


Agogo_final<-Agogo_MahirPCA[,c(1:74)] #create a subset of variables from original variables to be used in further analysis

Agogo_final<-left_join(Agogo_final,pattern_scores,by=c("id"="ID")) #join the pattern scores with the subset data frame for analysis
Agogo_final<-left_join(Agogo_final,mothers_age,by=c("id"="id"))


Agogo_final$edu_m<-as.factor(Agogo_final$edu_m)
str(Manufactured_based)

table(Agogo_final$occu_m,useNA = "always")

####Regression analysis####

### 1 Association between in utero malaria and dietary pattern scores###

#crude model
regression1<-subset(Agogo_final,is.na(pcr_fal_mo)==F) ###create a regression data subset 
#create a function
run_lm <- function(ot_v, regression1) {
  model <- lm(as.formula(paste(ot_v, "~ pcr_fal_mo")), data = regression1)
  return(model)
}

#select outcome variables
ot_vr<-c("ML1","ML4","ML2","ML3")

#empty list to store models
models<-list()
#for loop to run all crude regressions
for (ot_v in ot_vr) {
  model <- run_lm(ot_v, regression1)
  models[[ot_v]] <- model
}

#print the results
for(ot_v in ot_vr){
  
 print(summ(models[[ot_v]], confint = T))
}


### 2. Association between dieatry patterns and cardio metabollic risk factors 

run_lm <- function(ot_v2, pr_v2, Agogo_final) {
  formula <- as.formula(paste(ot_v2, "~",pr_v2))
  model<-lm(formula,data = Agogo_final)
  return(model)
}

#select outcome variables
pr_vr<-c("BMI","FPG","sysBP_mean","diaBP_mean")
ot_vr<-c("ML1","ML4","ML2","ML3")

#empty list to store models
models<-list()
#for loop to run all crude regressions
for(ot_v2 in ot_vr){
   
     for(pr_v2 in pr_vr){
          model_name<-paste(ot_v2,pr_v2, sep="~")
          model<-run_lm(ot_v2,pr_v2,Agogo_final)
          models[[model_name]]<-model
   }
}

#print the results
for(model_name in names(models)){
  
  cat("model name:",model_name,"\n")
  print(summ(models[[model_name]]))
  cat("\n")
}

### 3. Mediation analysis with roots and tuber based dietary pattern as mediator

#crude model
mediation<-subset(Agogo_final,is.na(mps_pos)==F)

run_lm <- function(med_ot, mediation) {
  model <- lm(as.formula(paste(med_ot, "~ mps_pos")), data = mediation)
  return(model)
}

#select outcome variables
mediation_cr<-c("BMI","FPG","sysBP_mean","diaBP_mean")

#empty list to store models
models<-list()
#for loop to run all crude regressions
for (med_ot in mediation_cr) {
  model <- run_lm(med_ot, mediation)
  models[[med_ot]] <- model
}

#print the results
for(med_ot in mediation_cr){
  
  print(summ(models[[med_ot]], confint = T))
}


###adjusted model
run_lm <- function(med_ot, mediation) {
  model <- lm(as.formula(paste(med_ot, "~ mps_pos+sex_2015+edu_m")), data = mediation)
  return(model)
}

#select outcome variables
mediation_ad<-c("BMI","FPG","sysBP_mean","diaBP_mean")

#empty list to store models
models<-list()
#for loop to run all crude regressions
for (med_ot in mediation_ad) {
  model <- run_lm(med_ot, mediation)
  models[[med_ot]] <- model
}

#print the results
for(med_ot in mediation_ad){
  
  print(summ(models[[med_ot]], confint = T))
}

#adjusted +mediator

#create terciles of each dietary pattern

#Manufactured food based diet

Agogo_final<- Agogo_final %>%
  mutate(Manufactured_based = ntile(ML1, 3)) %>%
  mutate(Manufactured_based = if_else(Manufactured_based == 1, '1', if_else(Manufactured_based == 2, '2', '3')))
Manufactured_based<-Agogo_final[,c(4,5,6,9,10,29,40,41,43,59,60,74,79)]
Manufactured_based$Manufactured_based <- factor(Manufactured_based$Manufactured_based, levels = c(1, 2, 3), 
                                                labels = c("Tercile 1", "Tercile 2", "Tercile 3"))
Manufactured_based$Manufactured_based<-factor(Manufactured_based$residence_2015, levels = c(1,2), labels = c("Agogo","Other"))

table(Manufactured_based$residence_2015)

Manufactured_based%>%tbl_summary(by=Manufactured_based,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                        all_categorical() ~ "{n} ({p}%)"), digits = all_continuous()~2)


#VeManufactured_based#Vegetables and fruits based diet
Agogo_final<- Agogo_final %>%
  mutate(Vegetable_based = ntile(ML4, 3)) %>%
  mutate(Vegetable_based = if_else(Vegetable_based == 1, '1', if_else(Vegetable_based == 2, '2', '3')))
Agogo_final$Vegetable_based<-as.factor(Agogo_final$Vegetable_based)
Vegetable_based<-Agogo_final[,c(1:78,80)]

#Roots and tuber based diet
Agogo_final<- Agogo_final %>%
  mutate(Roots_based = ntile(ML2, 3)) %>%
  mutate(Roots_based = if_else(Roots_based == 1, '1', if_else(Roots_based == 2, '2', '3')))
Agogo_final$Roots_based<-as.factor(Agogo_final$Roots_based)
Roots_based<-Agogo_final[,c(1:78,81)]

#Soup and fufu based diet 
Agogo_final<- Agogo_final %>%
  mutate(Soup_based = ntile(ML3, 3)) %>%
  mutate(Soup_based = if_else(Soup_based == 1, '1', if_else(Soup_based == 2, '2', '3')))
Agogo_final$Soup_based<-as.factor(Agogo_final$Soup_based)
Soup_based<-Agogo_final[,c(1:78,82)]


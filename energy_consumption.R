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
#install.packages("Hmisc")
use_git()
Sys.setenv("LANGUAGE"= "EN")
rm(doPlots)

Agogo_MahirPCA<- read.csv(file="agogo_2015_Sophie_PCA.csv",header=TRUE,sep = ";",na.strings=c("NA","NaN", " "))
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

rm(Agogo_MahirPCA)



Agogo_MahirPCA$id<-as.character(Agogo_MahirPCA$id)
ord_food<-Agogo_MahirPCA[,c(187:252)]  
ord_food<-ord_food[,c(-57,-58,-62)]
ord_food<-na.omit(ord_food)

std_ord<-scale(ord_food)

pca<-Agogo_MahirPCA[,c(1,253:315)]
pca<-na.omit(pca)
std_pca<-scale(pca)

KMO(pca)  ###kaiser - Meyer - olkin test for sampling adequacy

c1<-cor(pca)
corrplot(c1, # visualize correlations
         type = "lower",
         method = "number",
         tl.col = "darkblue",
         number.cex = 0.6,
         tl.cex = 0.6,
         col = COL2('RdBu'))

fa.parallel(pca, # perform parallel analysis
            fa = "fa",
            fm = "ml",
            show.legend = TRUE,
            main = "Scree Plot and Parallel Analysis")
??screeplot()
??fa.parallel()
fviz_eig(pca, 
         addlabels = TRUE, 
         ylim = c(0, 18),
         main="Figure 1")

factanal(pca,factors = 4,rotation = "varimax")

factor_four <- fa(pca [,c(-1)] , nfactors = 4, rotate = "varimax", fm= "ml")
factor_four
factor_four_matrix <- as.matrix(factor_four$loadings)

std_pca<-std_pca[,c(-1)]
pattern_scores<-std_pca%*%factor_four_matrix

pattern_scores <- data.frame(ID = pca[, c(1)], pattern_scores)

#manual calculation to check if the matrix multiplication for pattern score calculation worked or not
ff1<-std_pca*f1$f1[match(names(std_pca), f1$newColName)][col(std_pca)]
ff1$pat_sc<-rowSums(ff1,na.rm=T)


Agogo_final<-Agogo_MahirPCA[,c(1:74)]
Agogo_final<-left_join(Agogo_final,pattern_scores,by=c("id"="ID"))



write.table(std_pca,"D:/Lenovo data/Masterarbeit publikation/Data/Thesis anaylsis/std_pca.csv", sep = ";", dec = ",", row.names = F)
write.table(factor_four,"D:/Lenovo data/Masterarbeit publikation/Data/Thesis anaylsis/fac1.csv",sep = ";", dec = ",",col.names = NA)




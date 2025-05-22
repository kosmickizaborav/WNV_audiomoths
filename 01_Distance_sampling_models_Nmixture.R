################## Distance sampling model with Distance #######################
#' Here, we develop the distance sampling models to estimate the relative abundances
#' of birds on the PNOA using N.mixture models. In addition, we want to add the 
#' suitability estimates calculated by  eBird as "detectability probability" to 
#' see if the models improves.
library(tidyverse)
library(tidyr)
library(unmarked)

rm(list = ls())
# Directories ------------------------------------------------------------------
loc.data <- paste0(getwd(), "/DATA/")
loc.census <- paste0(getwd(), "/DATA/BBDD_ALEX_Ocells_Aiguamolls/")
loc.out <- paste0(getwd(), "/OUTPUT/")

# Loading data -----------------------------------------------------------------
transects <- readRDS(paste0(loc.out, "ebird_abundances.rds")) %>%
  mutate(
    id = paste0(transect_name, "_", sector)
  ) %>%
  sf::st_drop_geometry()

birds <- readRDS(paste0(loc.out, "full_wider_db.rds")) 
birds <- birds %>%
  mutate(
    season = case_when(
      month %in% c("abril", "maig") ~ "spring",
      month %in% c("juny", "juliol", "agost") ~ "summer",
      month %in% c("setembre", "octubre", "novembre") ~ "fall"
    )
  )

spp <- unique(birds$species) # 171

# Building the data for distance sampling modeling -----------------------------
model_results <- data.frame()
for (sp in spp){
  cat("---------------------- ", sp, "\n")
  
  # Transform data in a temporal-distance matrix
  birds_sample <- birds %>% 
    filter(species == sp) %>%
    rename("transect" = "id") %>% 
    mutate(
      dist = paste0(distbegin, "-", distend, "-", month)
    ) %>% 
    dplyr::select(transect, dist, size) %>%
    pivot_wider(
      names_from = dist,
      values_from = size
    ) %>%
    dplyr::select(transect, 
                  starts_with("0-25-abril"), starts_with("25-100-abril"), starts_with("100-250-abril"),
                  starts_with("0-25-maig"), starts_with("25-100-maig"), starts_with("100-250-maig"),
                  starts_with("0-25-juny"), starts_with("25-100-juny"), starts_with("100-250-juny"),
                  starts_with("0-25-juliol"), starts_with("25-100-juliol"), starts_with("100-250-juliol"),
                  starts_with("0-25-agost"), starts_with("25-100-agost"), starts_with("100-250-agost"),
                  starts_with("0-25-setembre"), starts_with("25-100-setembre"), starts_with("100-250-setembre"),
                  starts_with("0-25-octubre"), starts_with("25-100-octubre"), starts_with("100-250-octubre"),
                  starts_with("0-25-novembre"), starts_with("25-100-novembre"), starts_with("100-250-novembre")
    )
  birds_sample[is.na(birds_sample)] <- 0
  
  # Ensure the 30 transects:
  transects_names <- unique(birds$id)
  
  for (i in 1:length(transects_names)){
    if (transects_names[i] %in% unique(birds_sample$transect) == FALSE){
      # print(transects_names[i])
      birds_sample <- rbind(birds_sample, c(transects_names[i], rep(0, 24)))
    }
  }
  
  # Add transect and sector habitat
  birds_sample <- merge(birds_sample, transects %>% 
                          filter(scientific_name == sp) %>%
                          dplyr::select(id, transect_habitat, sector_habitat, ebird_abund), 
                        by.x = "transect", by.y = "id", all.x = TRUE)
  
  # Should be present all months
  months <- c("abril", "maig", "juny", "juliol", "agost", "setembre", "octubre", "novembre")
  distances <- c("0-25", "25-100", "100-250")
  expected_columns <- c("transect", "transect_habitat", "sector_habitat",
                        as.vector(outer(distances, months, paste, sep="-")))
  for (col in expected_columns) {
    # print(col)
    if (!(col %in% colnames(birds_sample))) {
      birds_sample[[col]] <- 0
    }
  }
  
  birds_sample <- birds_sample %>%
    dplyr::select(transect,
                  starts_with("0-25-abril"), starts_with("25-100-abril"), starts_with("100-250-abril"),
                  starts_with("0-25-maig"), starts_with("25-100-maig"), starts_with("100-250-maig"),
                  starts_with("0-25-juny"), starts_with("25-100-juny"), starts_with("100-250-juny"),
                  starts_with("0-25-juliol"), starts_with("25-100-juliol"), starts_with("100-250-juliol"),
                  starts_with("0-25-agost"), starts_with("25-100-agost"), starts_with("100-250-agost"),
                  starts_with("0-25-setembre"), starts_with("25-100-setembre"), starts_with("100-250-setembre"),
                  starts_with("0-25-octubre"), starts_with("25-100-octubre"), starts_with("100-250-octubre"),
                  starts_with("0-25-novembre"), starts_with("25-100-novembre"), starts_with("100-250-novembre"),
                  transect_habitat, sector_habitat, ebird_abund
    )
  
  
  # We have tyday()# We have to create a specific data frame for unmarked models ------------------
  umf <- unmarkedFrameGDS(y = as.matrix(apply(birds_sample[,2:25], 2, as.numeric)), 
                          survey= "line", 
                          unitsIn = "m",
                          tlength = rep(600, 30),
                          dist.breaks = c(0, 25, 100, 250), 
                          numPrimary = 8,
                          siteCovs = data.frame(birds_sample[c("transect_habitat", "sector_habitat", "ebird_abund")])
  )
  # summary(umf)

  if (sum(is.na(birds_sample$ebird_abund)) > 0) {
    data_row <- data.frame(
      scientific_name = sp,
      census = sum(as.matrix(apply(birds_sample[,2:25], 2, as.numeric))),
      expected = NA
    )
  } else {
    # Fitting simple models --------------------------------------------------------
    fall <- list()
    
    fall$Null <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                           keyfun = "halfnorm", output = "abund", K = 150,
                           mixture = "NB", se = TRUE, data = umf)
    fall$ebird <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~ebird_abund,
                           keyfun = "halfnorm", output = "abund", K = 150,
                           mixture = "NB", se = TRUE, data = umf)
    fall$lambda_sector <- gdistsamp(lambdaformula = ~sector_habitat, phiformula = ~1, pformula = ~1,
                                    keyfun = "halfnorm", output = "abund", K = 150,
                                    mixture = "NB", se = TRUE, data = umf)
    fall$lambda_sector_ebird <- gdistsamp(lambdaformula = ~sector_habitat, phiformula = ~1, pformula = ~ebird_abund,
                                    keyfun = "halfnorm", output = "abund", K = 150,
                                    mixture = "NB", se = TRUE, data = umf)
    fall$lambda_transect <- gdistsamp(lambdaformula = ~transect_habitat, phiformula = ~1, pformula = ~1,
                                    keyfun = "halfnorm", output = "abund", K = 150,
                                    mixture = "NB", se = TRUE, data = umf)
    fall$lambda_transect_ebird <- gdistsamp(lambdaformula = ~transect_habitat, phiformula = ~1, pformula = ~ebird_abund,
                                          keyfun = "halfnorm", output = "abund", K = 150,
                                          mixture = "NB", se = TRUE, data = umf)
    
    (msFall <- modSel(fitList(fits=fall)))
    
    model <- msFall@Full$model[1]
    
    # Check out the goodness-of-fit of this model
    fitstats <- function(fm, na.rm=TRUE) {
      observed <- getY(fm@data)
      expected <- fitted(fm)
      resids <- residuals(fm)
      sse <- sum(resids^2, na.rm = TRUE)
      chisq <- sum((observed - expected)^2 / expected, na.rm = TRUE)
      freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, na.rm = TRUE)
      out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
      return(out)
    }

    gof1 <- parboot(fall[[model]], fitstats, nsim=25, report=1)

    # Express the magnitude of lack of fit by an overdispersion factor
    c.hat <- gof1@t0[2] / mean(gof1@t.star[,2]) # Chisq
    
    getN <- function(fm, newdata=NULL){
      pred <- data.frame(
        pred = predict(fm, type="lambda", newdata=newdata)[,1], 
        transect = rep(1:5, each = 6)
        )
      
      pred <- pred %>% group_by(transect) %>% summarise(pred = mean(pred, na.rm = TRUE))
      
      return(sum(pred$pred, na.rm = TRUE))
    }
    
    data_row <- data.frame(
      scientific_name = sp,
      census = sum(as.matrix(apply(birds_sample[,2:25], 2, as.numeric))),
      expected = getN(fall[[model]])
    )
  }
  
  model_results <- rbind(model_results, data_row)
}

#' We follow the basic ideas of the binomial and multinomial N-mixture models 
#' assuming that unit sample (s) is the transect (transect + sector) and it has a local
#' abundane which distribution is a Poisson.
#' Ns,k ~ Poisson(gamma_s)
#' gamma_s (the mean abundance per unit sample) may be influenced by one or more covariates(v_s)
#' log(gamma_s) = b0 + b1v_s
#' and we assumed that the birds censused is dependent on a detection function parameter (omega)
#' (y_s1,..., ysH) ~ Multimodal(Ns, pi_s) where H is distances
#' the detection function has the omega parameter (omega)
#' log(omega_s) = alpha_0 + alpha_1*V_s
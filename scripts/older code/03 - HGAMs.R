# 03 common trend - HGAMS
	

	library(mgcv)
	
	glimpse(biomass_EBS)
	
	biomass_EBS$stockname <- as.factor(biomass_EBS$stockname)

  fit_GI <- gam(log(value) ~ s(year, k=5, m=2, bs="tp") + s(year, by=stockname, k=5, m=1, bs="tp") +
                  s(stockname, bs = "re", k=17),
                 data = biomass_EBS, method = "REML", family = "gaussian")
                
  gratia::draw(fit_GI)
  
  # I - no global smoother, different wiggliness
  fit_I <- gam(log(value) ~ s(year, by=commonname, k=5, bs="tp", m=2) +
                 s(commonname, bs="re", k=17),
               data = biomass_EBS, method = "REML")
  
  gratia::draw(fit_I)
  
  AIC(fit_GI)
	AIC(fit_I)
  
	sp <- c("Pacific cod", "Northern rockfish", "Alaska plaice")
	
	d <- biomass_EBS |>
		filter(commonname %in% sp)
	
	d <- droplevels(d)
  
  
  fit_GI <- gam(log(value) ~ s(year, k=5, m=2, bs="tp") + s(year, by=commonname, k=5, m=1, bs="tp") +
                  s(commonname, bs = "re", k=9),
                 data = d, method = "REML", family = "gaussian")
                
  gratia::draw(fit_GI)
  
  # I - no global smoother, different wiggliness
  fit_I <- gam(log(value) ~ s(year, by=commonname, k=5, bs="tp", m=2) +
                 s(commonname, bs="re", k=9),
               data = d, method = "REML")
  
  gratia::draw(fit_I)
  
  AIC(fit_GI)
	AIC(fit_I)

	

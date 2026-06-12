# HGAMs by jurisdiction


	library(mgcv)
	
	glimpse(biomass_dat)
	
	biomass_dat$Jurisdiction <- as.factor(biomass_dat$Jurisdiction)

  fit_GI <- gam(log(Value) ~ s(Year, k=5, m=2, bs="tp") + s(Year, by=Jurisdiction, k=5, m=1, bs="tp") +
                  s(Jurisdiction, bs = "re", k=14),
                 data = biomass_dat, method = "REML", family = "gaussian")
  
  
  fit_GI <- gam(log(Value) ~ s(Year, bs = "tp") + s(Year, by=Jurisdiction, bs="tp") +
                s(Jurisdiction, bs = "re"),
                data = biomass_dat, method = "REML", family = "gaussian")
                
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

	

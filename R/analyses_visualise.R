# TOOLS, SETTINGS, DATA
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
    source(here::here('R/prepare_data.R'))
    
# VISUALISE including estimates
    # tarsus
        dtg = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

        m=  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )

        nsim <- 5000
        bsim <- sim(m, n.sim=nsim)  
            
        # coefficients
            v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
            ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))

        # values to predict for
            newD=data.frame(net_rearing_manipulation = seq(min(dtg$net_rearing_manipulation), max(dtg$net_rearing_manipulation), length.out = 300), chick_sex_molec = c('m','f','u'), brood_sex_ratio = mean(dtg$brood_sex_ratio))

        # exactly the model which was used has to be specified here 
            X <- model.matrix(~ net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio ,data=newD)   
                        
        # calculate predicted values and creditability intervals
            newD$pred <-(X%*%v) 
            predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
            for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
                predmatrix[predmatrix < 0] <- 0
                newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
        pt=newD    
        #ptm = pt[pt$chick_sex_molec == 'm',]
        #ptf = pt[pt$chick_sex_molec == 'f',]
        #ptu = pt[pt$chick_sex_molec == 'u',]

        ggplot(pt, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) +theme_MB + 
            labs(x = 'Number of taken/added chicks', y = 'Tarsus length [mm]')
           

        ggsave(file='Output/Tarsus-net_change.png', width = 7, height = 4.5, units = "cm" , dpi = 600)
    # weight
        dtg = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

        m=  lmer(day_14_weight ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )

        nsim <- 5000
        bsim <- sim(m, n.sim=nsim)  
            
        # coefficients
            v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
            ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))

        # values to predict for
            newD=data.frame(net_rearing_manipulation = seq(min(dtg$net_rearing_manipulation), max(dtg$net_rearing_manipulation), length.out = 300), chick_sex_molec = c('m','f','u'), brood_sex_ratio = mean(dtg$brood_sex_ratio))

        # exactly the model which was used has to be specified here 
            X <- model.matrix(~ net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio ,data=newD)   
                        
        # calculate predicted values and creditability intervals
            newD$pred <-(X%*%v) 
            predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
            for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
                predmatrix[predmatrix < 0] <- 0
                newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
        pt=newD    
        #ptm = pt[pt$chick_sex_molec == 'm',]
        #ptf = pt[pt$chick_sex_molec == 'f',]
        #ptu = pt[pt$chick_sex_molec == 'u',]

        ggplot(pt, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) +theme_MB + 
            labs(x = 'Number of taken/added chicks', y = 'Weight [g]')  

        ggsave(file='Output/Weight-net_change.png', width = 7, height = 4.5, units = "cm" , dpi = 600)
    # BMI
        dtg = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

        m=  lmer(body_mass_index ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )

        nsim <- 5000
        bsim <- sim(m, n.sim=nsim)  
            
        # coefficients
            v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
            ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))

        # values to predict for
            newD=data.frame(net_rearing_manipulation = seq(min(dtg$net_rearing_manipulation), max(dtg$net_rearing_manipulation), length.out = 300), chick_sex_molec = c('m','f','u'), brood_sex_ratio = mean(dtg$brood_sex_ratio))

        # exactly the model which was used has to be specified here 
            X <- model.matrix(~ net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio ,data=newD)   
                        
        # calculate predicted values and creditability intervals
            newD$pred <-(X%*%v) 
            predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
            for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
                predmatrix[predmatrix < 0] <- 0
                newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
        pt=newD    
        #ptm = pt[pt$chick_sex_molec == 'm',]
        #ptf = pt[pt$chick_sex_molec == 'f',]
        #ptu = pt[pt$chick_sex_molec == 'u',]

        ggplot(pt, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) +theme_MB + 
            labs(x = 'Number of taken/added chicks', y = 'Body mass index')
           

        ggsave(file='Output/BMI-net_change.png', width = 7, height = 4.5, units = "cm" , dpi = 600)

    # other
        ggplot(a, aes(x = chick_sex_molec, y = body_mass_index)) + geom_boxplot() + theme_MB
        ggplot(a, aes(x = chick_sex_molec, y = day_14_weight)) + geom_boxplot() + theme_MB
        ggplot(a, aes(x = chick_sex_molec, y = day_14_tarsus_length)) + geom_boxplot() + theme_MB

        ggplot(a, aes(x = net_rearing_manipulation, y = body_mass_index)) + geom_point() + stat_smooth(method = "rlm") + theme_MB
        ggplot(a, aes(x = net_rearing_manipulation, y = day_14_weight))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
        ggplot(a, aes(x = net_rearing_manipulation, y = day_14_tarsus_length))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
       
# END            
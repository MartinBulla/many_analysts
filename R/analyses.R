# TOOLS, SETTINGS, DATA
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
    source(here::here('R/prepare_data.R'))
    
# MORTALITY - Table M 
    # prepare data
        # net_rearing_namipulation as continues
        m = lmer(change_chick_n ~ net_rearing_manipulation  +       
                      brood_sex_ratio +     
                      (1|day14_measurer) + 
                      (1|rear_area) +      
                      (1|rear_nest_OH_l) + (1|hatch_year),       
                      data = mou, REML = reml        
                 )      
        summary(m)
        summary(glht(m))
        plot(allEffects(m))
      
        # net_rearing_namipulation as a factor      
         mf = lmer(change_chick_n ~ net_rearing_manipulation_factor  +       
                      brood_sex_ratio +     
                      (1|day14_measurer) + (1|rear_area) +      
                      (1|rear_nest_OH_l) + (1|hatch_year),       
                      data = mou, REML = reml)

    # TABLE M + model assumptions
        o_m = m_out(name = "mortality", model = m, round_ = 3, nsim = 5000, aic = TRUE, N = nrow(mou))
        sname = 'Table_M'
        write.xlsx(o_m, paste0("Output/",sname,'.xlsx'), sheetName='Estimates')
        #write_xlsx(rbind(o_mt0g,o_mt14g,o_mt0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

        m_ass(name = 'Table M - change_chick_n-net_rearing_man+sex_ratio', mo = m, dat = mou, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = NULL, trans = c('none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

# MAIN TEXT TABLES
    # TARSUS - Table T 
        # prepare data
            dtg = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

            cor(dtg$d14_rear_nest_brood_size, dtg$net_rearing_manipulation)
            
            # main text model simple
                mt0g =  lmer(day_14_tarsus_length ~ 
                    net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) +
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dtg, REML = reml
                    )
                summary(mt0g) 
                summary(glht(mt0g))
                plot(allEffects(mt0g))

            # net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence warning, but see extended Table Te where the variables were scaled and model converged well)
                mt14g =  lmer(day_14_tarsus_length ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dtg, REML = reml
                    )
                summary(mt14g) 
                summary(glht(mt14g))
                plot(allEffects(mt14g))

            # main text model simple with sex interaction
                mt0gs =  lmer(day_14_tarsus_length ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dtg, REML = reml
                    )
                summary(mt0gs) 
                summary(glht(mt0gs))
                plot(allEffects(mt0gs))
       
        # Table T - main text - and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mt0gs = m_out(name = "c - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)   
            
            sname = 'Table_T'

            aic = data.table(rbind(o_mt0g,o_mt14g,o_mt0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mt0g,o_mt14g,o_mt0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            #write_xlsx(rbind(o_mt0g,o_mt14g,o_mt0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

            m_ass(name = 'Table Ta - tarsus-net+sex+sex-ratio', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            m_ass(name = 'Table Tb - tarsus-d14+sex+sex-ratio', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table Tc - tarsus-netxsex+sex-ratio', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
        # Table Te - Extended material
    # WEIGHT - Table W
        # prepare data
            dwg = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]
            # main text model simple
                mw0g =  lmer(day_14_weight ~ 
                    net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) +
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dwg, REML = reml
                    )
                summary(mw0g) 
                summary(glht(mw0g))
                plot(allEffects(mw0g))

            # main text CHECK model
                mw14g =  lmer(day_14_weight ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dwg, REML = reml
                    )
                summary(mw14g) 
                summary(glht(mw14g))
                plot(allEffects(mw14g))

           
            # main text model simple with sex interaction
                mw0gs =  lmer(day_14_weight ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dwg, REML = reml
                    )
                summary(mw0gs) 
                summary(glht(mw0gs))
                plot(allEffects(mw0gs))
         # Table W - main text - and model assumptions
            o_mw0g = m_out(name = "a - net chick", model = mw0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mw14g  = m_out(name = "b - day 14 chick #", model = mw14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mw0gs = m_out(name = "c - (a) with sex interaction", model = mw0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)   
            
            sname = 'Table_W'

            aic = data.table(rbind(o_mw0g,o_mw14g,o_mw0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mw0g,o_mw14g,o_mw0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            
            m_ass(name = 'Table Wa - weight-net+sex+sex-ratio', mo = mw0g, dat = dwg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            m_ass(name = 'Table Wb - weight-d14+sex+sex-ratio', mo = mw14g, dat = dwg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table Wc - weight-netxsex+sex-ratio', mo = mw0gs, dat = dwg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    # BODY MASS INDEX - Table B
        # prepare data
            dmg = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]
            # main text model simple
                mb0g =  lmer(body_mass_index ~ 
                    net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) +
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dmg, REML = reml
                    )
                summary(mb0g) 
                summary(glht(mb0g))
                plot(allEffects(mb0g))

            # net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence issues)
                mb14g =  lmer(body_mass_index ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dmg, REML = reml
                    )
                summary(mb14g) 
                summary(glht(mb14g))
                plot(allEffects(mb14g))

            
            # main text model simple with sex interaction
                mb0gs =  lmer(body_mass_index ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad),
                    data = dmg, REML = reml
                    )
                summary(mb0gs) 
                summary(glht(mb0gs))
                plot(allEffects(mb0gs))
           
        # Table B - main text - and model assumptions
            o_mb0g = m_out(name = "a - net chick", model = mb0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mb14g  = m_out(name = "b - day 14 chick #", model = mb14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            o_mb0gs = m_out(name = "c - (a) with sex interaction", model = mb0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)   
            
            sname = 'Table_B'

            aic = data.table(rbind(o_mb0g,o_mb14g,o_mb0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mb0g,o_mb14g,o_mb0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            #write_xlsx(rbind(o_mb0g,o_mb14g,o_mb0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

            m_ass(name = 'Table Ba - BMI-net+sex+sex-ratio', mo = mb0g, dat = dmg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            m_ass(name = 'Table Bb - BMI-d14+sex+sex-ratio', mo = mb14g, dat = dmg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table Bc - BMI-netxsex+sex-ratio', mo = mb0gs, dat = dmg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        # Table Be - Extended material

# VISUALISE including estimates
    ggplot(a, aes(x = chick_sex_molec, y = body_mass_index)) + geom_boxplot() + theme_MB
    ggplot(a, aes(x = chick_sex_molec, y = day_14_weight)) + geom_boxplot() + theme_MB
    ggplot(a, aes(x = chick_sex_molec, y = day_14_tarsus_length)) + geom_boxplot() + theme_MB

    ggplot(a, aes(x = net_rearing_manipulation, y = body_mass_index)) + geom_point() + stat_smooth(method = "rlm") + theme_MB
    ggplot(a, aes(x = net_rearing_manipulation, y = day_14_weight))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
    ggplot(a, aes(x = net_rearing_manipulation, y = day_14_tarsus_length))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
    # prepare predictions
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

# EXTENDED MATERIAL TABLES
    # TARSUS - Table TE 
        # INFO
            # scaled response and continuous predictors - standardized effects 
            # uses models from Table T and additional models described in methods
        # prepare data
            dtg = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

            # no sex interactions
                # main text model simple
                    mt0g =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0g) 
                    summary(glht(mt0g))
                    plot(allEffects(mt0g))
                # net_rearing_manipulation exchanged for d14_rear_nest_brood_size 
                    mt14g =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(d14_rear_nest_brood_size) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt14g) 
                    summary(glht(mt14g))
                    plot(allEffects(mt14g))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtig =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg,  REML = reml
                        )
                       summary(mtig) 
                       summary(glht(mtig))
                       plot(allEffects(mtig))

                # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1g =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1g) 
                       summary(glht(mt1g))
                       plot(allEffects(mt1g))   
            # sex interaction
                # main text model simple 
                    mt0gs =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0gs) 
                    summary(glht(mt0gs))
                    plot(allEffects(mt0gs))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtigs =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mtigs) 
                       summary(glht(mtigs))
                       plot(allEffects(mtigs))

                    # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1gs =  lmer(scale(day_14_tarsus_length) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec +
                        scale(net_rearing_manipulation):chick_sex_molec + 
                        scale(d14_rear_nest_brood_size):chick_sex_molec +
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1gs) 
                       summary(glht(mt1gs))
                       plot(allEffects(mt1gs))   
            # AIC
                aic2 = data.table(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE)), model = rownames(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE))))         
                aic2[, deltaAIC := AIC-min(AIC)]
                aic2[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
                aic2[, ER := round(max(prob)/prob, 2)]
                aic2[order(deltaAIC)]          
        # Table Te -and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEa - tarsus-net+sex+sex-ratio - scaled', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEb - tarsus-d14+sex+sex-ratio - scaled', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mtig  = m_out(name = "c - net*d14", model = mtig, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEc - tarsus-netxd14+sex-ratio scaled', mo = mtig, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt1g  = m_out(name = "d - net+d14", model = mt1g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEd - tarsus-net+d14+sex-ratio scaled', mo = mt1g, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt0gs = m_out(name = "e - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEe - tarsus-netxsex+sex-ratio - scaled', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
           
            o_mtigs  = m_out(name = "f - (c) with sex interaction", model = mtigs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEf - tarsus-netxd14xsex+sex-ratio - scaled', mo = mtigs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mt1gs  = m_out(name = "g - (d) with sex interaction", model = mt1gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table TEg - tarsus-netxsex+d14xsex+sex-ratio - scaled', mo = mt1gs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            sname = 'Table_TE'
            o = data.table(rbind(o_mt0g,o_mt14g,o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
            aic = o[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
    # WEIGHT - Table WE 
        # INFO
            # scaled response and continuous predictors - standardized effects 
            # uses models from Table T and additional models described in methods
        # prepare data
            dtg = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

            # no sex interactions
                # main text model simple
                    mt0g =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0g) 
                    summary(glht(mt0g))
                    plot(allEffects(mt0g))
                # net_rearing_manipulation exchanged for d14_rear_nest_brood_size 
                    mt14g =  lmer(scale(day_14_weight) ~ 
                        scale(d14_rear_nest_brood_size) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt14g) 
                    summary(glht(mt14g))
                    plot(allEffects(mt14g))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtig =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg,  REML = reml
                        )
                       summary(mtig) 
                       summary(glht(mtig))
                       plot(allEffects(mtig))

                # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1g =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1g) 
                       summary(glht(mt1g))
                       plot(allEffects(mt1g))   
            # sex interaction
                # main text model simple 
                    mt0gs =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0gs) 
                    summary(glht(mt0gs))
                    plot(allEffects(mt0gs))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtigs =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mtigs) 
                       summary(glht(mtigs))
                       plot(allEffects(mtigs))

                    # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1gs =  lmer(scale(day_14_weight) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec +
                        scale(net_rearing_manipulation):chick_sex_molec + 
                        scale(d14_rear_nest_brood_size):chick_sex_molec +
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1gs) 
                       summary(glht(mt1gs))
                       plot(allEffects(mt1gs))   
            # AIC
                aic2 = data.table(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE)), model = rownames(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE))))         
                aic2[, deltaAIC := AIC-min(AIC)]
                aic2[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
                aic2[, ER := round(max(prob)/prob, 2)]
                aic2[order(deltaAIC)]
        # Table WE -and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEa - weight-net+sex+sex-ratio - scaled', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEb - weight-d14+sex+sex-ratio - scaled', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mtig  = m_out(name = "c - net*d14", model = mtig, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEc - weight-netxd14+sex-ratio scaled', mo = mtig, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt1g  = m_out(name = "d - net+d14", model = mt1g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEd - weight-net+d14+sex-ratio scaled', mo = mt1g, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt0gs = m_out(name = "e - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEe - weight-netxsex+sex-ratio - scaled', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
           
            o_mtigs  = m_out(name = "f - (c) with sex interaction", model = mtigs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEf - weight-netxd14xsex+sex-ratio - scaled', mo = mtigs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mt1gs  = m_out(name = "g - (d) with sex interaction", model = mt1gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table WEg - weight-netxsex+d14xsex+sex-ratio - scaled', mo = mt1gs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            sname = 'Table_TE'
            o = data.table(rbind(o_mt0g,o_mt14g,o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
            aic = o[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
    # BODY MASS INDEX - Table BE 
        # INFO
            # scaled response and continuous predictors - standardized effects 
            # uses models from Table T and additional models described in methods
        # prepare data
            dtg = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

            # no sex interactions
                # main text model simple
                    mt0g =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0g) 
                    summary(glht(mt0g))
                    plot(allEffects(mt0g))
                # net_rearing_manipulation exchanged for d14_rear_nest_brood_size 
                    mt14g =  lmer(scale(body_mass_index) ~ 
                        scale(d14_rear_nest_brood_size) +  
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt14g) 
                    summary(glht(mt14g))
                    plot(allEffects(mt14g))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtig =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID) +
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg,  REML = reml
                        )
                       summary(mtig) 
                       summary(glht(mtig))
                       plot(allEffects(mtig))

                # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1g =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1g) 
                       summary(glht(mt1g))
                       plot(allEffects(mt1g))   
            # sex interaction
                # main text model simple 
                    mt0gs =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                    summary(mt0gs) 
                    summary(glht(mt0gs))
                    plot(allEffects(mt0gs))
                # interaction: net_rearing_manipulation changes with d14_rear_nest_brood_size ? (was proposed initial model, but was dropped because of a priori decision to use only one of the >0.6 correlated variables)
                    mtigs =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mtigs) 
                       summary(glht(mtigs))
                       plot(allEffects(mtigs))

                    # net_rearing_manipulation and  d14_rear_nest_brood_size
                       mt1gs =  lmer(scale(body_mass_index) ~ 
                        scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                        chick_sex_molec +
                        scale(net_rearing_manipulation):chick_sex_molec + 
                        scale(d14_rear_nest_brood_size):chick_sex_molec +
                        scale(brood_sex_ratio) +
                        (1|day14_measurer) + (1|rear_area) + 
                        (1|rear_nest_OH_l) + (1|hatch_year)  +
                        (1|rear_nest_breed_ID)+
                        (1|hatch_mom_Ring) + (1|genetic_dad),
                        data = dtg, REML = reml
                        )
                       summary(mt1gs) 
                       summary(glht(mt1gs))
                       plot(allEffects(mt1gs))   
            # AIC
                aic2 = data.table(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE)), model = rownames(AIC(update(mt0g,REML = FALSE),update(mt14g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE), update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE))))         
                aic2[, deltaAIC := AIC-min(AIC)]
                aic2[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
                aic2[, ER := round(max(prob)/prob, 2)]
                aic2[order(deltaAIC)]
        # Table BE -and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEa - BMI-net+sex+sex-ratio - scaled', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEb - BMI-d14+sex+sex-ratio - scaled', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mtig  = m_out(name = "c - net*d14", model = mtig, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEc - BMI-netxd14+sex-ratio scaled', mo = mtig, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt1g  = m_out(name = "d - net+d14", model = mt1g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEd - BMI-net+d14+sex-ratio scaled', mo = mt1g, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            o_mt0gs = m_out(name = "e - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEe - BMI-netxsex+sex-ratio - scaled', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
           
            o_mtigs  = m_out(name = "f - (c) with sex interaction", model = mtigs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEf - BMI-netxd14xsex+sex-ratio - scaled', mo = mtigs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            o_mt1gs  = m_out(name = "g - (d) with sex interaction", model = mt1gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
            m_ass(name = 'Table BEg - BMI-netxsex+d14xsex+sex-ratio - scaled', mo = mt1gs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            sname = 'Table_TE'
            o = data.table(rbind(o_mt0g,o_mt14g, o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
            aic = o[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')

# EXTENDED MATERILA (TABLES) - controlling and using only experimental pairs            
    dtg = p[complete.cases(p),.(day_14_tarsus_length, day_14_weight, body_mass_index,net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad, pair_ID)] # limit data

    # TARSUS - Table TEP 
        # prepare data
            # simple
                mt0g =  lmer(day_14_tarsus_length ~ 
                    net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) +
                    (1|hatch_mom_Ring) + (1|genetic_dad) +
                    (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt0g) 
                summary(glht(mt0g))
                plot(allEffects(mt0g))
            # net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence warning, but see extended Table Te where the variables were scaled and model converged well)
                mt14g =  lmer(day_14_tarsus_length ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad) + (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt14g) 
                summary(glht(mt14g))
                plot(allEffects(mt14g))
            # simple with sex interaction
                mt0gs =  lmer(day_14_tarsus_length ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad)+ (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt0gs) 
                summary(glht(mt0gs))
                plot(allEffects(mt0gs))  
        # Table TEP  and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mt0gs = m_out(name = "c - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)   
            
            sname = 'Table_TEP'

            aic = data.table(rbind(o_mt0g,o_mt14g,o_mt0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mt0g,o_mt14g,o_mt0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            #write_xlsx(rbind(o_mt0g,o_mt14g,o_mt0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

            m_ass(name = 'Table TEPa - tarsus-net+sex+sex-ratio', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            m_ass(name = 'Table TEPb - tarsus-d14+sex+sex-ratio', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table TEPc - tarsus-netxsex+sex-ratio', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/") 
    # WEIGHT - Table WEP 
        # prepare data
            # simple
                mt0g =  lmer(day_14_weight ~ 
                    net_rearing_manipulation +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) +
                    (1|hatch_mom_Ring) + (1|genetic_dad) +
                    (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt0g) 
                summary(glht(mt0g))
                plot(allEffects(mt0g))
            # net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence warning, but see extended Table Te where the variables were scaled and model converged well)
                mt14g =  lmer(day_14_weight ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID) + (1|hatch_mom_Ring) + (1|genetic_dad) + (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt14g) 
                summary(glht(mt14g))
                plot(allEffects(mt14g))
            # simple with sex interaction
                mt0gs =  lmer(day_14_weight ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad)+ (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mt0gs) 
                summary(glht(mt0gs))
                plot(allEffects(mt0gs))  
        # Table WEP  and model assumptions
            o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mt0gs = m_out(name = "c - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)   
            
            sname = 'Table_WEP'

            aic = data.table(rbind(o_mt0g,o_mt14g,o_mt0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mt0g,o_mt14g,o_mt0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            #write_xlsx(rbind(o_mt0g,o_mt14g,o_mt0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

            m_ass(name = 'Table WEPa - weight-netxsex+sex-ratio-net+sex+sex-ratio', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
            
            m_ass(name = 'Table WEPb - weight-d14+sex+sex-ratio', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table WEPc - weight-netxsex+sex-ratio', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    # BODY MASS INDEX - Table BEP
        # prepare data    
            # simple
            mb0g =  lmer(body_mass_index ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad)+ (1|pair_ID),
                data = dtg, REML = reml
                )
            summary(mb0g) 
            summary(glht(mb0g))
            plot(allEffects(mb0g))
            # net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence issues)
                mb14g =  lmer(body_mass_index ~ 
                    d14_rear_nest_brood_size +  
                    chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad)+ (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mb14g) 
                summary(glht(mb14g))
                plot(allEffects(mb14g))
            # simple with sex interaction
                mb0gs =  lmer(body_mass_index ~ 
                    net_rearing_manipulation*chick_sex_molec + 
                    brood_sex_ratio +
                    (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year)  +
                    (1|rear_nest_breed_ID)+
                    (1|hatch_mom_Ring) + (1|genetic_dad)+ (1|pair_ID),
                    data = dtg, REML = reml
                    )
                summary(mb0gs) 
                summary(glht(mb0gs))
                plot(allEffects(mb0gs))    
        # Table BEP - main text - and model assumptions
            o_mb0g = m_out(name = "a - net chick", model = mb0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mb14g  = m_out(name = "b - day 14 chick #", model = mb14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)
            o_mb0gs = m_out(name = "c - (a) with sex interaction", model = mb0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2070)   
            
            sname = 'Table_BEP'

            aic = data.table(rbind(o_mb0g,o_mb14g,o_mb0gs))[AIC!=""]
            aic[, AIC := as.numeric(AIC)]
            aic[, deltaAIC := AIC-min(AIC)]
            aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
            aic[, ER := round(max(prob)/prob, 2)]
            aic[order(deltaAIC)]
            o = rbind(o_mb0g,o_mb14g,o_mb0gs)
            o$deltaAIC[o$AIC!=""] = aic$deltaAIC[match(o$model[o$AIC!=""], aic$model)]
            o$prob[o$AIC!=""] = aic$prob[match(o$model[o$AIC!=""], aic$model)]
            o$ER[o$AIC!=""] = aic$ER[match(o$model[o$AIC!=""], aic$model)]
            write.xlsx(o, paste0("Output/",sname,'.xlsx'), sheetName='Estimates&AIC')
            #write_xlsx(rbind(o_mb0g,o_mb14g,o_mb0g), paste0("Outputs/",sname,'.xlsx'), sheetName='AICcompar', append = TRUE)

            m_ass(name = 'Table BEPa - BMI-net+sex+sex-ratio', mo = mb0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
            m_ass(name = 'Table BEPb - BMI-d14+sex+sex-ratio', mo = mb14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

            m_ass(name = 'Table BEPc - BMI-netxsex+sex-ratio', mo = mb0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

# END            
# TOOLS, SETTINGS, DATA
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
    source(here::here('R/prepare_data.R'))
    
# additional data checking
    # NAs
        summary(a) 
        summary(factor(a$chick_sex_molec))
        length(a$hatch_mom_Ring[is.na(a$hatch_mom_Ring)])
        length(a$genetic_dad[is.na(a$genetic_dad)])

    # distributions
        summary(factor(a$d14_rear_nest_brood_size)) 
        summary(factor(a$net_rearing_manipulation)) 
        summary(as.factor(a$rear_Cs_at_start_of_rearing))
        summary(as.factor(a$d0_hatch_nest_brood_size))
        summary(a$prop_change_in_brood_size)


        ggplot(a, aes(x = net_rearing_manipulation)) + geom_histogram()
        ggplot(a, aes(x = d14_rear_nest_brood_size-rear_Cs_at_start_of_rearing, fill = treatment)) + geom_histogram(position='dodge', bins = 12) + scale_x_continuous("dead chicks at day 14\n since start of experiment", breaks = seq(-10,1, by = 1), label = seq(-10,1, by = 1)) + xlab(c(-10, 1)) + theme_MB # in one case the brood size has increased by one chick

        ggplot(a, aes(x = (d14_rear_nest_brood_size-rear_Cs_at_start_of_rearing)/rear_Cs_at_start_of_rearing, fill = treatment)) + geom_histogram(position='dodge', bins = 12) + scale_x_continuous("proportion of dead chicks at day 14\n since start of experiment",breaks = seq(-1,0.3, by = 0.1), label = seq(-1,0.3, by = 0.1)) + xlab(c(-1, 0.3)) + theme_MB

        ggplot(a, aes(x = prop_change_in_brood_size, fill = treatment)) + geom_histogram() +xlim(c(-0.5,0.5))+ theme_MB

    # cor among  predictors
      # chick numbers at hatching and day 14    
        aa = a[complete.cases(a),.(rear_Cs_at_start_of_rearing,d14_rear_nest_brood_size )]
        cor(aa$rear_Cs_at_start_of_rearing, aa$d14_rear_nest_brood_size) # use 'd14_rear_nest_brood_size' in the models
        ggplot(a, aes(x = rear_Cs_at_start_of_rearing, y = d14_rear_nest_brood_size)) + geom_point() +facet_wrap(.~rear_nest_trt) + geom_smooth(method = 'lm') 
        ggplot(a, aes(x = rear_Cs_at_start_of_rearing, y = rear_Cs_at_start_of_rearing-d14_rear_nest_brood_size)) + geom_point() +facet_wrap(.~rear_nest_trt) + geom_smooth(method = 'lm')

      # chick numbers at day 14 and net change in chick numbers
        ggplot(a, aes(x = rear_Cs_at_start_of_rearing-d14_rear_nest_brood_size)) + geom_histogram()        
        ggplot(a, aes(x = as.factor(rear_nest_trt), y = rear_Cs_at_start_of_rearing-d14_rear_nest_brood_size)) + geom_boxplot()                                            
        ggplot(a, aes(x = as.factor(rear_nest_trt), y = (rear_Cs_at_start_of_rearing-d14_rear_nest_brood_size)/rear_Cs_at_start_of_rearing)) + geom_boxplot()  

        cor(a$d14_rear_nest_brood_size, a$net_rearing_manipulation)
        ggplot(a, aes( y = net_rearing_manipulation, x = d14_rear_nest_brood_size)) +
            geom_point() + stat_smooth(method = 'lm') + facet_wrap(.~ treatment)


        cor(a$brood_sex_ratio, a$net_rearing_manipulation)    
        ggplot(a, aes( y = net_rearing_manipulation, x = brood_sex_ratio)) +
            geom_point() + stat_smooth(method = 'lm') + facet_wrap(.~ treatment)

      # chick mortality dataset
      ggplot(mou, aes(x = rear_nest_OH_l)) + geom_histogram() +theme_MB
      ggplot(mou, aes(x = net_rearing_manipulation, y = brood_sex_ratio)) + geom_point() + geom_smooth() +theme_MB
      cor(mou$brood_sex_ratio, mou$net_rearing_manipulation)
  
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

        m_ass(name = 'Table Ba - tarsus-net+sex+sex-ratio', mo = mb0g, dat = dmg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
        m_ass(name = 'Table Bb - tarsus-d14+sex+sex-ratio', mo = mb14g, dat = dmg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")

        m_ass(name = 'Table Bc - tarsus-netxsex+sex-ratio', mo = mb0gs, dat = dmg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    # Table Be - Extended material

# VISUALISE including estimates
    ggplot(a, aes(x = chick_sex_molec, y = body_mass_index)) + geom_boxplot() + theme_MB
    ggplot(a, aes(x = chick_sex_molec, y = day_14_weight)) + geom_boxplot() + theme_MB
    ggplot(a, aes(x = chick_sex_molec, y = day_14_tarsus_length)) + geom_boxplot() + theme_MB

    ggplot(a, aes(x = net_rearing_manipulation, y = body_mass_index)) + geom_point() + stat_smooth(method = "rlm") + theme_MB
    ggplot(a, aes(x = net_rearing_manipulation, y = day_14_weight))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
    ggplot(a, aes(x = net_rearing_manipulation, y = day_14_tarsus_length))  + geom_point() + stat_smooth(method = "rlm") + theme_MB

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
        o = data.table(rbind(o_mt0g,o_mt14g,,o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
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
    # Table WE -and model assumptions
        o_mt0g = m_out(name = "a - net chick", model = mt0g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEa - tarsus-net+sex+sex-ratio - scaled', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEb - tarsus-d14+sex+sex-ratio - scaled', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        o_mtig  = m_out(name = "c - net*d14", model = mtig, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEc - tarsus-netxd14+sex-ratio scaled', mo = mtig, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt1g  = m_out(name = "d - net+d14", model = mt1g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEd - tarsus-net+d14+sex-ratio scaled', mo = mt1g, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt0gs = m_out(name = "e - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEe - tarsus-netxsex+sex-ratio - scaled', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
       
        o_mtigs  = m_out(name = "f - (c) with sex interaction", model = mtigs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEf - tarsus-netxd14xsex+sex-ratio - scaled', mo = mtigs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        o_mt1gs  = m_out(name = "g - (d) with sex interaction", model = mt1gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table WEg - tarsus-netxsex+d14xsex+sex-ratio - scaled', mo = mt1gs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
        sname = 'Table_TE'
        o = data.table(rbind(o_mt0g,o_mt14g,,o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
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
        m_ass(name = 'Table BEa - tarsus-net+sex+sex-ratio - scaled', mo = mt0g, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt14g  = m_out(name = "b - day 14 chick #", model = mt14g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEb - tarsus-d14+sex+sex-ratio - scaled', mo = mt14g, dat = dtg, fixed = c('d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        o_mtig  = m_out(name = "c - net*d14", model = mtig, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEc - tarsus-netxd14+sex-ratio scaled', mo = mtig, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt1g  = m_out(name = "d - net+d14", model = mt1g, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEd - tarsus-net+d14+sex-ratio scaled', mo = mt1g, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
    
        o_mt0gs = m_out(name = "e - (a) with sex interaction", model = mt0gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEe - tarsus-netxsex+sex-ratio - scaled', mo = mt0gs, dat = dtg, fixed = c('net_rearing_manipulation', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
       
        o_mtigs  = m_out(name = "f - (c) with sex interaction", model = mtigs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEf - tarsus-netxd14xsex+sex-ratio - scaled', mo = mtigs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        o_mt1gs  = m_out(name = "g - (d) with sex interaction", model = mt1gs, round_ = 3, nsim = 5000, aic = TRUE, N = 2550)
        m_ass(name = 'Table BEg - tarsus-netxsex+d14xsex+sex-ratio - scaled', mo = mt1gs, dat = dtg, fixed = c('net_rearing_manipulation','d14_rear_nest_brood_size', 'brood_sex_ratio'),categ = 'chick_sex_molec', trans = c('none','none','none'), spatial = FALSE, temporal = TRUE, PNG = TRUE, outdir = "Output/Model_ass/")
        
        sname = 'Table_TE'
        o = data.table(rbind(o_mt0g,o_mt14g,,o_mtig,o_mt1g,o_mt0gs,o_mtigs,o_mt1gs))
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
# TOOLS and SETTINGS
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)

# DATA preparation
    a = fread('Data/blue_tit_data.csv') 
    setnames(a, old = 'genetic_dad_ring_(WP_or_EP)', new = 'genetic_dad')

    a = a[net_rearing_manipulation != ".", .(rear_nest_breed_ID, hatch_year, rear_nest_OH, chick_sex_molec, hatch_mom_Ring, genetic_dad, day_14_weight, day_14_tarsus_length, 
        rear_nest_trt, net_rearing_manipulation, rear_Cs_at_start_of_rearing, d14_rear_nest_brood_size,  d0_hatch_nest_brood_size, rear_d0_rear_nest_brood_size,day14_measurer, rear_area)]

    # 203455 nest has one chicks with different year for when the eggs began to hatch in the nest in which the chick was reared. 1 = April 1., we assign the same year as is the year for the remaining chicks
        a[rear_nest_breed_ID == '203455', hatch_year := 2003]

    # define NAs and column and variable definitions
        a[hatch_mom_Ring %in% ".", hatch_mom_Ring := NA]
        a[genetic_dad %in% ".", genetic_dad := NA]
        a[, rear_Cs_at_start_of_rearing := as.numeric(rear_Cs_at_start_of_rearing)]
        a[, net_rearing_manipulation := as.numeric(net_rearing_manipulation)]
        a[, d14_rear_nest_brood_size := as.numeric(d14_rear_nest_brood_size)]
        a[, d0_hatch_nest_brood_size := as.numeric(d0_hatch_nest_brood_size)]
        a[, rear_d0_rear_nest_brood_size := as.numeric(rear_d0_rear_nest_brood_size)]
        a[chick_sex_molec %in% ".", chick_sex_molec := "u"]
        a[chick_sex_molec %in% "1", chick_sex_molec := "f"]
        a[chick_sex_molec %in% "2", chick_sex_molec := "m"]
        a[rear_nest_trt %in% "5", treatment := "increased"]
        a[rear_nest_trt %in% "6", treatment := "reduced"]
        a[rear_nest_trt %in% "7", treatment := "constant"]

    # compute  variables
        a[, change_chick_n := rear_Cs_at_start_of_rearing-d14_rear_nest_brood_size]
        a[, rear_nest_OH_l :=  rear_nest_OH - min(rear_nest_OH), by = hatch_year] # day when first nest hatch in a given year is zero day of the season
        a[, prop_change_in_brood_size := net_rearing_manipulation/d0_hatch_nest_brood_size]
        # sex ration at day 14
            a[chick_sex_molec == 'm', sex := 1]
            a[chick_sex_molec == 'f', sex := 0]
            a[chick_sex_molec == 'u', sex := 0.5]
            a[, brood_sex_ratio := sum(sex)/length(sex), by = rear_nest_breed_ID]
        # body mass index
            a[, body_mass_index := day_14_weight/day_14_tarsus_length^2]

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

# MORTALITY
    mo = a[complete.cases(a),.(change_chick_n, net_rearing_manipulation, d14_rear_nest_brood_size, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]
    mou = unique(mo)
    nrow(mou)
    length(unique(mo$rear_nest_breed_ID))
    mou[, net_rearing_manipulation_factor := as.factor(net_rearing_manipulation)]
    # data checking
        ggplot(mou, aes(x = rear_nest_OH_l)) + geom_histogram() +theme_MB
        ggplot(mou, aes(x = rear_nest_OH_l)) + geom_histogram() +theme_MB

    # one nest has chicks with different date for when the eggs began to hatch in the nest in which the chick was reared. 1 = April 1., we excluded the first chick
    
    m = lmer(change_chick_n ~ net_rearing_manipulation  + 
                     brood_sex_ratio + # only if doable 
                     (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year),
                    data = mou, REML = reml
                )

    summary(m) 
    summary(glht(m))
    plot(allEffects(m))

    mf = lmer(change_chick_n ~ net_rearing_manipulation_factor  + 
                     brood_sex_ratio + # only if doable 
                     (1|day14_measurer) + (1|rear_area) + 
                    (1|rear_nest_OH_l) + (1|hatch_year),
                    data = mou, REML = reml
                )

    summary(mf) 
    summary(glht(mf))
    plot(allEffects(mf))

# TARSUS 
    # tarsus without sex interaction and no genetic control
      dt0 = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]
      dt0[, net_rearing_manipulation_factor := as.factor(net_rearing_manipulation)]
        
        # main text model simple
            mt0 =  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
            summary(mt0) 
            summary(glht(mt0))
            plot(allEffects(mt0))

        # extended data model where net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence issues)
            mt14 =  lmer(day_14_tarsus_length ~ 
                d14_rear_nest_brood_size +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
            summary(mt14) 
            summary(glht(mt14))
            plot(allEffects(mt14))

        # extended data model where net_rearing_manipulation is a factor
            mt0f =  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation_factor +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
            summary(mt0f) 
            summary(glht(mt0f))
            plot(allEffects(mt0f))
    
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mti =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, , REML = reml
                )
               summary(mti) 
               summary(glht(mti))
               plot(allEffects(mti))

              # no interaction
               mt1 =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
               summary(mt1) 
               summary(glht(mt1))
               plot(allEffects(mt1))   

              AIC(update(mt0,REML = FALSE),update(mti, REML = FALSE), update(mt1, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dt0 = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]
      dt0[, net_rearing_manipulation_factor := as.factor(net_rearing_manipulation)]

        # main text model simple - and the single relevant one
            mt0s =  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
            summary(mt0s) 
            summary(glht(mt0s))
            plot(allEffects(mt0s))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mtis =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, , REML = reml
                )
               summary(mtis) 
               summary(glht(mtis))
               plot(allEffects(mtis))

              # no interaction
               mt1s =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dt0, REML = reml
                )
               summary(mt1s) 
               summary(glht(mt1s))
               plot(allEffects(mt1s))   
    aic = data.table(AIC(update(mt0,REML = FALSE),update(mti, REML = FALSE), update(mt1, REML = FALSE),
        update(mt0s,REML = FALSE),update(mtis, REML = FALSE), update(mt1s, REML = FALSE), update(mt14,REML = FALSE)), model = rownames(AIC(update(mt0,REML = FALSE),update(mti, REML = FALSE), update(mt1, REML = FALSE), update(mt0s,REML = FALSE),update(mtis, REML = FALSE), update(mt1s, REML = FALSE), update(mt14,REML = FALSE), update(mt0f,REML = FALSE))))          
    aic[, deltaAIC := AIC-min(AIC)]
    aic[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aic[, ER := round(max(prob)/prob, 2)]
    aic[order(deltaAIC)]

    # tarsus without sex interaction and genetic control
      dtg = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

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
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mtig =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg,  REML = reml
                )
               summary(mtig) 
               summary(glht(mtig))
               plot(allEffects(mtig))

              # no interaction
               mt1g =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )
               summary(mt1g) 
               summary(glht(mt1g))
               plot(allEffects(mt1g))   

              AIC(update(mt0g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dtg = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)

        # main text model simple - and the single relevant one
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
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mtigs =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )
               summary(mtigs) 
               summary(glht(mtigs))
               plot(allEffects(mtigs))

              # no interaction
               mt1gs =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dtg, REML = reml
                )
               summary(mt1gs) 
               summary(glht(mt1gs))
               plot(allEffects(mt1gs))   
    AIC(update(mt0g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE),
        update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE))         

    aic2 = data.table(AIC(update(mt0g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE),
        update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE)), model = rownames(AIC(update(mt0g,REML = FALSE),update(mtig, REML = FALSE), update(mt1g, REML = FALSE),
        update(mt0gs,REML = FALSE),update(mtigs, REML = FALSE), update(mt1gs, REML = FALSE))))          
    aic2[, deltaAIC := AIC-min(AIC)]
    aic2[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aic2[, ER := round(max(prob)/prob, 2)]
    aic2[order(deltaAIC)]
# WEIGHT - all model have warning because effect of measurer is zero
    # tarsus without sex interaction and no genetic control
      dw0 = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple
            mw0 =  lmer(day_14_weight ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, REML = reml
                )
            summary(mw0) 
            summary(glht(mw0))
            plot(allEffects(mw0))

        # extended data model where net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence issues)
            mw14 =  lmer(day_14_weight ~ 
                d14_rear_nest_brood_size +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, REML = reml
                )
            summary(mw14) 
            summary(glht(mw14))
            plot(allEffects(mw14))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mwi =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, , REML = reml
                )
               summary(mwi) 
               summary(glht(mwi))
               plot(allEffects(mwi))

              # no interaction
               mw1 =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, REML = reml
                )
               summary(mw1) 
               summary(glht(mw1))
               plot(allEffects(mw1))   

              AIC(update(mw0,REML = FALSE),update(mwi, REML = FALSE), update(mw1, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dw0 = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple - and the single relevant one
            mw0s =  lmer(day_14_weight ~ 
                net_rearing_manipulation*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, REML = reml
                )
            summary(mw0s) 
            summary(glht(mw0s))
            plot(allEffects(mw0s))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mwis =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, , REML = reml
                )
               summary(mwis) 
               summary(glht(mwis))
               plot(allEffects(mwis))

              # no interaction
               mw1s =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dw0, REML = reml
                )
               summary(mw1s) 
               summary(glht(mw1s))
               plot(allEffects(mw1s))   
    aic3 = data.table(AIC(update(mw0,REML = FALSE),update(mwi, REML = FALSE), update(mw1, REML = FALSE),
        update(mw0s,REML = FALSE),update(mwis, REML = FALSE), update(mw1s, REML = FALSE), update(mw14,REML = FALSE)), model = rownames(AIC(update(mw0,REML = FALSE),update(mwi, REML = FALSE), update(mw1, REML = FALSE), update(mw0s,REML = FALSE),update(mwis, REML = FALSE), update(mw1s, REML = FALSE), update(mw14,REML = FALSE))))          
    aic3[, deltaAIC := AIC-min(AIC)]
    aic3[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aic3[, ER := round(max(prob)/prob, 2)]
    aic3[order(deltaAIC)]

    # tarsus without sex interaction and genetic control
      dwg = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

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
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mwig =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dwg,  REML = reml
                )
               summary(mwig) 
               summary(glht(mwig))
               plot(allEffects(mwig))

              # no interaction
               mw1g =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dwg, REML = reml
                )
               summary(mw1g) 
               summary(glht(mw1g))
               plot(allEffects(mw1g))   

              AIC(update(mw0g,REML = FALSE),update(mwig, REML = FALSE), update(mw1g, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dwg = a[complete.cases(a),.(day_14_weight, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)

        # main text model simple - and the single relevant one
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
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mwigs =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dwg, REML = reml
                )
               summary(mwigs) 
               summary(glht(mwigs))
               plot(allEffects(mwigs))

              # no interaction
               mw1gs =  lmer(day_14_weight ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dwg, REML = reml
                )
               summary(mw1gs) 
               summary(glht(mw1gs))
               plot(allEffects(mw1gs))   
    AIC(update(mw0g,REML = FALSE),update(mwig, REML = FALSE), update(mw1g, REML = FALSE),
        update(mw0gs,REML = FALSE),update(mwigs, REML = FALSE), update(mw1gs, REML = FALSE))         

    aic4 = data.table(AIC(update(mw0g,REML = FALSE),update(mwig, REML = FALSE), update(mw1g, REML = FALSE),
        update(mw0gs,REML = FALSE),update(mwigs, REML = FALSE), update(mw1gs, REML = FALSE)), model = rownames(AIC(update(mw0g,REML = FALSE),update(mwig, REML = FALSE), update(mw1g, REML = FALSE),
        update(mw0gs,REML = FALSE),update(mwigs, REML = FALSE), update(mw1gs, REML = FALSE))))          
    aic4[, deltaAIC := AIC-min(AIC)]
    aic4[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aic4[, ER := round(max(prob)/prob, 2)]
    aic4[order(deltaAIC)]
# BMS 
    # tarsus without sex interaction and no genetic control
      dm0 = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple
            mm0 =  lmer(body_mass_index ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, REML = reml
                )
            summary(mm0) 
            summary(glht(mm0))
            plot(allEffects(mm0))

        # extended data model where net_rearing_manipulation exchanged for d14_rear_nest_brood_size (convergence issues)
            mm14 =  lmer(body_mass_index ~ 
                d14_rear_nest_brood_size +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, REML = reml
                )
            summary(mm14) 
            summary(glht(mm14))
            plot(allEffects(mm14))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mmi =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, , REML = reml
                )
               summary(mmi) 
               summary(glht(mmi))
               plot(allEffects(mmi))

              # no interaction
               mm1 =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, REML = reml
                )
               summary(mm1) 
               summary(glht(mm1))
               plot(allEffects(mm1))   

              AIC(update(mm0,REML = FALSE),update(mmi, REML = FALSE), update(mm1, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dm0 = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple - and the single relevant one
            mm0s =  lmer(body_mass_index ~ 
                net_rearing_manipulation*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, REML = reml
                )
            summary(mm0s) 
            summary(glht(mm0s))
            plot(allEffects(mm0s))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mmis =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, , REML = reml
                )
               summary(mmis) 
               summary(glht(mmis))
               plot(allEffects(mmis))

              # no interaction
               mm1s =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = dm0, REML = reml
                )
               summary(mm1s) 
               summary(glht(mm1s))
               plot(allEffects(mm1s))              
    aicM = data.table(AIC(update(mm0,REML = FALSE),update(mmi, REML = FALSE), update(mm1, REML = FALSE),
        update(mm0s,REML = FALSE),update(mmis, REML = FALSE), update(mm1s, REML = FALSE), update(mm14,REML = FALSE)), model = rownames(AIC(update(mm0,REML = FALSE),update(mmi, REML = FALSE), update(mm1, REML = FALSE), update(mm0s,REML = FALSE),update(mmis, REML = FALSE), update(mm1s, REML = FALSE), update(mm14,REML = FALSE))))          
    aicM[, deltaAIC := AIC-min(AIC)]
    aicM[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aicM[, ER := round(max(prob)/prob, 2)]
    aicM[order(deltaAIC)]

    # tarsus without sex interaction and genetic control
      dmg = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

        # main text model simple
            mm0g =  lmer(body_mass_index ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg, REML = reml
                )
            summary(mm0g) 
            summary(glht(mm0g))
            plot(allEffects(mm0g))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mmig =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID) +
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg,  REML = reml
                )
               summary(mmig) 
               summary(glht(mmig))
               plot(allEffects(mmig))

              # no interaction
               mm1g =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg, REML = reml
                )
               summary(mm1g) 
               summary(glht(mm1g))
               plot(allEffects(mm1g))   

              AIC(update(mm0g,REML = FALSE),update(mmig, REML = FALSE), update(mm1g, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dmg = a[complete.cases(a),.(body_mass_index, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]

        # main text model simple - and the single relevant one
            mm0gs =  lmer(body_mass_index ~ 
                net_rearing_manipulation*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg, REML = reml
                )
            summary(mm0gs) 
            summary(glht(mm0gs))
            plot(allEffects(mm0gs))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mmigs =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size)*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg, REML = reml
                )
               summary(mmigs) 
               summary(glht(mmigs))
               plot(allEffects(mmigs))

              # no interaction
               mm1gs =  lmer(body_mass_index ~ 
                scale(net_rearing_manipulation) + scale(d14_rear_nest_brood_size) +
                chick_sex_molec +
                scale(net_rearing_manipulation):chick_sex_molec + 
                scale(d14_rear_nest_brood_size):chick_sex_molec +
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID)+
                (1|hatch_mom_Ring) + (1|genetic_dad),
                data = dmg, REML = reml
                )
               summary(mm1gs) 
               summary(glht(mm1gs))
               plot(allEffects(mm1gs))   
    aicM2 = data.table(AIC(update(mm0g,REML = FALSE),update(mmig, REML = FALSE), update(mm1g, REML = FALSE),
        update(mm0gs,REML = FALSE),update(mmigs, REML = FALSE), update(mm1gs, REML = FALSE)), model = rownames(AIC(update(mm0g,REML = FALSE),update(mmig, REML = FALSE), update(mm1g, REML = FALSE),
        update(mm0gs,REML = FALSE),update(mmigs, REML = FALSE), update(mm1gs, REML = FALSE))))          
    aicM2[, deltaAIC := AIC-min(AIC)]
    aicM2[, prob := round(exp(-0.5*deltaAIC)/sum(exp(-0.5*deltaAIC)),2)]
    aicM2[, ER := round(max(prob)/prob, 2)]
    aicM2[order(deltaAIC)]
      
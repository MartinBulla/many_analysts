# TOOLS and SETTINGS
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE

# DATA preparation
    a = fread('Data/blue_tit_data.csv') 
    setnames(a, old = 'genetic_dad_ring_(WP_or_EP)', new = 'genetic_dad')

    a = a[net_rearing_manipulation != ".", .(rear_nest_breed_ID, hatch_year, rear_nest_OH, chick_sex_molec, hatch_mom_Ring, genetic_dad, day_14_weight, day_14_tarsus_length, 
        rear_nest_trt, net_rearing_manipulation, rear_Cs_at_start_of_rearing, d14_rear_nest_brood_size,  d0_hatch_nest_brood_size, rear_d0_rear_nest_brood_size,day14_measurer, rear_area)]

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

# RESULTS and 
    # tarsus without sex interaction and no genetic control
      dt0 = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple
            mt0 =  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation +  
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = d, REML = reml
                )
            summary(mt0) 
            summary(glht(mt0))
            plot(allEffects(mt0))
        # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
             # interaction
               mti =  lmer(day_14_tarsus_length ~ 
                scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
                chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = d, , REML = reml
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
                data = d, REML = reml
                )
               summary(mt1) 
               summary(glht(mt1))
               plot(allEffects(mt1))   

              AIC(update(mt0,REML = FALSE),update(mti, REML = FALSE), update(mt1, REML = FALSE))
    # tarsus with sex interaction and no genetic control
      dt0 = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

        # main text model simple
            mt0s =  lmer(day_14_tarsus_length ~ 
                net_rearing_manipulation*chick_sex_molec + 
                brood_sex_ratio +
                (1|day14_measurer) + (1|rear_area) + 
                (1|rear_nest_OH_l) + (1|hatch_year)  +
                (1|rear_nest_breed_ID),
                data = d, REML = reml
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
                data = d, , REML = reml
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
                data = d, REML = reml
                )
               summary(mt1s) 
               summary(glht(mt1s))
               plot(allEffects(mt1s))   

              AIC(update(mt0s,REML = FALSE),update(mtis, REML = FALSE), update(mt1s, REML = FALSE))

  # tarsus with sex interaction and no genetic control
    dt0 = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]

    # main text model simple
        mt0 =  lmer(day_14_tarsus_length ~ 
            net_rearing_manipulation +  
            chick_sex_molec + 
            brood_sex_ratio +
            (1|day14_measurer) + (1|rear_area) + 
            (1|rear_nest_OH_l) + (1|hatch_year)  +
            (1|rear_nest_breed_ID),
            data = d, REML = reml
            )
        summary(mt0) 
        summary(glht(mt0))
        plot(allEffects(mt0))
    # extended data models (checking if net_rearing_manipulation changes with d14_rear_nest_brood_size - and initial model dropped because of a priori decision to use only one of the >0.6 correlated variables)
         # interaction
           mti =  lmer(day_14_tarsus_length ~ 
            scale(net_rearing_manipulation) * scale(d14_rear_nest_brood_size) +
            chick_sex_molec + 
            brood_sex_ratio +
            (1|day14_measurer) + (1|rear_area) + 
            (1|rear_nest_OH_l) + (1|hatch_year)  +
            (1|rear_nest_breed_ID),
            data = d, , REML = reml
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
            data = d, REML = reml
            )
           summary(mt1) 
           summary(glht(mt1))
           plot(allEffects(mt1))   

          AIC(update(mt0,REML = FALSE),update(mti, REML = FALSE), update(mt1, REML = FALSE))       
    # tarsus with genetics
        d = a[complete.cases(a),.(day_14_tarsus_length, net_rearing_manipulation, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad]
        mt0g =  lmer(day_14_tarsus_length ~ 
            net_rearing_manipulation * d14_rear_nest_brood_size +
            chick_sex_molec + 
            brood_sex_ratio +
            (1|day14_measurer) + (1|rear_area) + 
            (1|rear_nest_OH_l) + (1|hatch_year)  +
            (1|rear_nest_breed_ID) +
            (1|hatch_mom_Ring) + (1|genetic_dad),
            data = d
            )

# EXTENDED MATERIAL        
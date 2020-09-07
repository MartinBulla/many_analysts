# TOOLS, SETTINGS, DATA
    require(here)
    source(here::here('R/tools.R'))
    reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
    source(here::here('R/prepare_data.R'))
    
    nsim <- 5000 # number of simulations for predictions

    # colors
        f_col = "#FCB42C" 
        m_col = "#535F7C"
        u_col = "#5eab2b"

    # functions
    get_legend<-function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }

    # limit data to available cases
      dtg = a[complete.cases(a),.(day_14_tarsus_length, day_14_weight, body_mass_index,  net_rearing_manipulation, net_rearing_manipulation_factor, d14_rear_nest_brood_size, chick_sex_molec, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID, hatch_mom_Ring, genetic_dad)]    

# Figure R
    # predictions 
        # mortality
            m = lmer(change_chick_n ~ net_rearing_manipulation  +       
                      brood_sex_ratio +     
                      (1|day14_measurer) + 
                      (1|rear_area) +      
                      (1|rear_nest_OH_l) + (1|hatch_year),       
                      data = mou, REML = reml        
                 )   
            bsim <- sim(m, n.sim=nsim)  
                
            # coefficients
                v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
                ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))

            # values to predict for
                newD=data.frame(net_rearing_manipulation = seq(min(mou$net_rearing_manipulation), max(mou$net_rearing_manipulation), length.out = 300), chick_sex_molec = c('m','f','u'), brood_sex_ratio = mean(mou$brood_sex_ratio))

            # exactly the model which was used has to be specified here 
                X <- model.matrix(~ net_rearing_manipulation +  
                        brood_sex_ratio ,data=newD)   
                            
            # calculate predicted values and creditability intervals
                newD$pred <-(X%*%v) 
                predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
                for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
                    predmatrix[predmatrix < 0] <- 0
                    newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                    newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                    newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
            pm=newD   
        
        # tarsus 
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
            pt$chick_sex_molec = ifelse(pt$chick_sex_molec == 'u', "unknown", ifelse(pt$chick_sex_molec =='f', 'female', 'male'))   
        # weight
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
            pw=newD   
            pw$chick_sex_molec = ifelse(pw$chick_sex_molec == 'u', "unknown", ifelse(pw$chick_sex_molec =='f', 'female', 'male'))   
        # BMI
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
            pb=newD    
            pb$chick_sex_molec = ifelse(pb$chick_sex_molec == 'u', "unknown", ifelse(pb$chick_sex_molec =='f', 'female', 'male'))   
    # prepare plots       
        ggm = ggplot(pm, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation), alpha = 0.1, show.legend = NA)+ 
            geom_line() +
            labs(x = 'Number of taken/added chicks', y = 'Dead chicks', tag = '(a)')  +
            theme_bw() + theme_MB + 
            theme(  legend.position="none",
                    axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                     plot.tag.position = c(0.95, 0.95),
                    plot.tag = element_text(size = 7.5))
        
        ggt = ggplot(pt, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) +
            labs(x = 'Number of taken/added chicks', y = 'Tarsus length [mm]', tag = '(b)', fill = 'Chick sex', col = 'Chick sex')+
            scale_fill_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            scale_color_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            theme_bw() + theme_MB + 
            theme(  axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    plot.tag.position = c(0.95, 0.95),
                    plot.tag = element_text(size = 7.5))
        legend = get_legend(ggt)
        ggt = ggt + theme(legend.position="none")   

        ggw = ggplot(pw, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) +
            labs(x = 'Number of taken/added chicks', y = 'Weight [g]', tag = '(c)')  +
            scale_fill_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            scale_color_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            theme_bw() + theme_MB + 
            theme(  legend.position="none",
                    axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                     plot.tag.position = c(0.95, 0.95),
                    plot.tag = element_text(size = 7.5))
        
        ggb = ggplot(pb, aes(x = net_rearing_manipulation, y = pred))  +
            geom_ribbon(aes(ymin=lwr, ymax=upr, x=net_rearing_manipulation, fill = chick_sex_molec), alpha = 0.2, show.legend = NA)+ 
            geom_line(aes(col =chick_sex_molec)) + 
            labs(x = 'Number of taken/added chicks', y = 'Body mass index', tag = '(d)') +
            scale_fill_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            scale_color_manual(labels = c("female", "male", 'unknown'), values = c(f_col, m_col, u_col))+
            theme_bw() + theme_MB + 
            theme(  legend.position="none",
                    #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)
                    plot.tag.position = c(0.95, 0.95),
                    plot.tag = element_text(size = 7.5))
    # plot COMBINED MTWB   
        g0 <- ggplotGrob(ggm)
        g1 <- ggplotGrob(ggt)
        g2 <- ggplotGrob(ggw)
        g3 <- ggplotGrob(ggb)
        
        #dev.new(width = 2.55906, height = 4.72441)
        #grid.arrange(rbind(g0,g1,g2,g3, size = "first"), legend, ncol = 2, padding = 0, widths =c(3.7, 1.25)) 

        gg = arrangeGrob(rbind(g0,g1,g2,g3, size = "first"),legend, ncol = 2, padding = 0.05, widths =c(3.7, 1.25))
     
        ggsave(file='Output/Figure_R.png', plot = gg, width = 7*1.25, height = 12*1.75, units = "cm" , dpi = 600)
    
    # simple individual plots    
        ggsave(file='Output/Mortality-net_change.png', plot = ggm, width = 7, height = 4.5, units = "cm" , dpi = 600)
        ggsave(file='Output/Tarsus-net_change.png', plot = ggt, width = 7, height = 4.5, units = "cm" , dpi = 600)
        ggsave(file='Output/Weight-net_change.png', plot = ggw, width = 7, height = 4.5, units = "cm" , dpi = 600)
        ggsave(file='Output/BMI-net_change.png', plot = ggb, width = 7, height = 4.5, units = "cm" , dpi = 600)
    # combined TWB   
        g1 <- ggplotGrob(ggt)
        g2 <- ggplotGrob(ggw)
        g3 <- ggplotGrob(ggb)
        
        #dev.new(width = 2.55906, height = 4.72441)
        #grid.arrange(rbind(g1,g2,g3, size = "first"), legend, ncol = 2, padding = 0, widths =c(3.7, 1.25)) 

        gg = arrangeGrob(rbind(g1,g2,g3, size = "first"),legend, ncol = 2, padding = 0.05, widths =c(3.7, 1.25))
     
        ggsave(file='Output/Figure_Rtwb.png', plot = gg, width = 6.5, height = 12, units = "cm" , dpi = 600)

# Figure EP 
    # prepare morphometric summaries
        morphometric_summaries <- 
          BT_data_tidy %>% 
          dplyr::mutate(day_14_BMI = day_14_weight/(day_14_tarsus_length^2)) %>% 
          dplyr::group_by(pair_ID, rear_nest_trt) %>% 
          dplyr::summarise(mean_tarsus = mean(day_14_tarsus_length, na.rm = TRUE),
                    median_tarsus = median(day_14_tarsus_length, na.rm = TRUE),
                    sd_tarsus = sd(day_14_tarsus_length, na.rm = TRUE),
                    se_tarsus = sd(day_14_tarsus_length, na.rm = TRUE)/sqrt(n()),
                    mean_mass = mean(day_14_weight, na.rm = TRUE),
                    median_mass = median(day_14_weight, na.rm = TRUE),
                    sd_mass = sd(day_14_weight, na.rm = TRUE),
                    se_mass = sd(day_14_weight, na.rm = TRUE)/sqrt(n()),
                    mean_BMI = mean(day_14_BMI, na.rm = TRUE),
                    median_BMI = median(day_14_BMI, na.rm = TRUE),
                    sd_BMI = sd(day_14_BMI, na.rm = TRUE),
                    se_BMI = sd(day_14_BMI, na.rm = TRUE)/sqrt(n()),
                    change_chick_n = change_chick_n) %>% 
          dplyr::distinct() %>% 
          dplyr::filter(rear_nest_trt != "control") %>% 
          tidyr::pivot_wider(names_from = rear_nest_trt, 
                             values_fn = list(mean_tarsus = mean,
                                              median_tarsus = mean,
                                              sd_tarsus = mean, 
                                              se_tarsus = mean, 
                                              mean_mass = mean, 
                                              median_mass = mean, 
                                              sd_mass = mean, 
                                              se_mass = mean, 
                                              mean_BMI = mean, 
                                              median_BMI = mean, 
                                              sd_BMI = mean, 
                                              se_BMI = mean,
                                              change_chick_n = mean),
                      values_from = c(mean_tarsus, median_tarsus, sd_tarsus, se_tarsus, 
                                      mean_mass, median_mass, sd_mass, se_mass, 
                                      mean_BMI, median_BMI, sd_BMI, se_BMI,
                                      change_chick_n)) %>% 
          na.omit    
    # prepare plots
        mortality_plot <-
          ggplot(data = morphometric_summaries) +
          geom_abline(intercept = 0 , slope = 1, linetype = "dashed", color = "grey30") + 
          # geom_point(aes(y = as.numeric(change_chick_n_increase), x = as.numeric(change_chick_n_decrease)), alpha = 0.5) +
          geom_jitter(aes(y = as.numeric(change_chick_n_increase), x = as.numeric(change_chick_n_decrease)), 
                      alpha = 0.5, width = 0.1, height = 0.2) +
          coord_fixed(ratio = 1) +
          xlab(expression(paste("Number of dead chicks in ", italic("REDUCED")," brood (± SE)"))) +
          ylab(expression(paste("Number of dead chicks in ", italic("ENLARGED")," brood (± SE)"))) +
          scale_y_continuous(limits = c(-1, 10)) +
          scale_x_continuous(limits = c(-1, 10)) +
          annotate(geom = "text", y = 7.25, x = 8.5, angle = 45,
                   label = "Brood reduction increased\nchick mortality",
                   color = "black", size = 2.5, fontface = 'italic') +
          annotate(geom = "text", y = 8.5, x = 7.25, angle = 45,
                   label = "Brood reduction decreased\nchick mortality",
                   color = "black", size = 2.5, fontface = 'italic') +
          annotate(geom = "text", y = 10, x = -1,
                   label = "M",
                   color = "black", size = 6, fontface = 'bold') +
          theme_bw() +
          theme(
            #text = element_text(family = "Franklin Gothic Book"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            axis.title.x = element_text(size = 10),
            axis.text.x  = element_text(size = 8), 
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.5, colour = "grey40"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "grey")
          )
        tarsus_plot <- 
          ggplot(data = morphometric_summaries) +
          geom_abline(intercept = 0 , slope = 1, linetype = "dashed", color = "grey30") + 
          geom_point(aes(y = mean_tarsus_increase, x = mean_tarsus_decrease), alpha = 0.5) +
          geom_errorbar(aes(y = mean_tarsus_increase, x = mean_tarsus_decrease, 
                            ymin = mean_tarsus_increase - se_tarsus_increase, 
                            ymax = mean_tarsus_increase + se_tarsus_increase), alpha = 0.2) +
          geom_errorbarh(aes(y = mean_tarsus_increase, x = mean_tarsus_decrease, 
                             xmin = mean_tarsus_decrease - se_tarsus_decrease, 
                             xmax = mean_tarsus_decrease + se_tarsus_decrease), alpha = 0.2) +
          coord_fixed(ratio = 1) +
          xlab(expression(paste("Mean tarsus length in ", italic("REDUCED")," brood (cm ± SE)"))) +
          ylab(expression(paste("Mean tarsus length in ", italic("ENLARGED")," brood (cm ± SE)"))) +
          scale_y_continuous(limits = c(13, 18)) +
          scale_x_continuous(limits = c(13, 18)) +
          annotate(geom = "text", y = 14, x = 14.5, angle = 45,
                   label = "Brood reduction increased\naverage body size in brood",
                   color = "black", size = 2.5, fontface = 'italic') +
          annotate(geom = "text", y = 14.5, x = 14, angle = 45,
                   label = "Brood reduction decreased\naverage body size in brood",
                   color = "black", size = 2.5, fontface = 'italic') +
          annotate(geom = "text", y = 18, x = 13,
                   label = "T",
                   color = "black", size = 6, fontface = 'bold') +
          theme_bw() +
          theme(
            #text = element_text(family = "Franklin Gothic Book"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            axis.title.x = element_text(size = 10),
            axis.text.x  = element_text(size = 8), 
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.5, colour = "grey40"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "grey")
          )
        mass_plot <-
          ggplot(data = morphometric_summaries) +
          geom_abline(intercept = 0 , slope = 1, linetype = "dashed", color = "grey30") + 
          geom_point(aes(y = mean_mass_increase, x = mean_mass_decrease), alpha = 0.5) +
          geom_errorbar(aes(y = mean_mass_increase, x = mean_mass_decrease, 
                            ymin = mean_mass_increase - se_mass_increase, 
                            ymax = mean_mass_increase + se_mass_increase), alpha = 0.2) +
          geom_errorbarh(aes(y = mean_mass_increase, x = mean_mass_decrease, 
                             xmin = mean_mass_decrease - se_mass_decrease, 
                             xmax = mean_mass_decrease + se_mass_decrease), alpha = 0.2) +
          coord_fixed(ratio = 1) +
          xlab(expression(paste("Mean body weight in ", italic("REDUCED")," brood (g ± SE)"))) +
          ylab(expression(paste("Mean body weight in ", italic("ENLARGED")," brood (g ± SE)"))) +
          scale_y_continuous(limits = c(6, 13)) +
          scale_x_continuous(limits = c(6, 13)) +
          annotate(geom = "text", y = 13, x = 6,
                   label = "W",
                   color = "black", size = 6, fontface = 'bold') +
          theme_bw() +
          theme(
            #text = element_text(family = "Franklin Gothic Book"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            axis.title.x = element_text(size = 10),
            axis.text.x  = element_text(size = 8), 
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.5, colour = "grey40"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "grey")
          )
        BMI_plot <-
          ggplot(data = morphometric_summaries) +
          geom_abline(intercept = 0 , slope = 1, linetype = "dashed", color = "grey30") + 
          geom_point(aes(y = mean_BMI_increase, x = mean_BMI_decrease), alpha = 0.5) +
          geom_errorbar(aes(y = mean_BMI_increase, x = mean_BMI_decrease, 
                            ymin = mean_BMI_increase - se_BMI_increase, 
                            ymax = mean_BMI_increase + se_BMI_increase), alpha = 0.2) +
          geom_errorbarh(aes(y = mean_BMI_increase, x = mean_BMI_decrease, 
                             xmin = mean_BMI_decrease - se_BMI_decrease, 
                             xmax = mean_BMI_decrease + se_BMI_decrease), alpha = 0.2) +
          coord_fixed(ratio = 1) +
          xlab(expression(paste("Mean body mass index in ", italic("REDUCED")," brood (± SE)"))) +
          ylab(expression(paste("Mean body mass index in ", italic("ENLARGED")," brood (± SE)"))) +
          scale_y_continuous(limits = c(0.025, 0.0475)) +
          scale_x_continuous(limits = c(0.025, 0.0475)) +
          annotate(geom = "text", y = 0.0475, x = 0.025,
                   label = "B",
                   color = "black", size = 6, fontface = 'bold') +
          theme_bw() + 
          theme(
           # text = element_text(family = "Franklin Gothic Book"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            axis.title.x = element_text(size = 10),
            axis.text.x  = element_text(size = 8), 
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.5, colour = "grey40"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.border = element_rect(linetype = "solid", colour = "grey")
          )   
    # plot
      ggarrange(mortality_plot,tarsus_plot,mass_plot,BMI_plot,ncol = 2, nrow = 2, align = "hv")
      ggsave(file='Output/Figure_EP.png', width = 8, height = 8,  dpi = 600)

# other unused (exploratory) plots
            ggplot(a, aes(x = chick_sex_molec, y = body_mass_index)) + geom_boxplot() + theme_MB
            ggplot(a, aes(x = chick_sex_molec, y = day_14_weight)) + geom_boxplot() + theme_MB
            ggplot(a, aes(x = chick_sex_molec, y = day_14_tarsus_length)) + geom_boxplot() + theme_MB

            ggplot(a, aes(x = net_rearing_manipulation, y = body_mass_index)) + geom_point() + stat_smooth(method = "rlm") + theme_MB
            ggplot(a, aes(x = net_rearing_manipulation, y = day_14_weight))  + geom_point() + stat_smooth(method = "rlm") + theme_MB
            ggplot(a, aes(x = net_rearing_manipulation, y = day_14_tarsus_length))  + geom_point() + stat_smooth(method = "rlm") + theme_MB    
           
# END            
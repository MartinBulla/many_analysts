# load data    
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
    a[, net_rearing_manipulation_factor := as.factor(net_rearing_manipulation)]
    a[, prop_change_in_brood_size := net_rearing_manipulation/d0_hatch_nest_brood_size]
    # sex ration at day 14
        a[chick_sex_molec == 'm', sex := 1]
        a[chick_sex_molec == 'f', sex := 0]
        a[chick_sex_molec == 'u', sex := 0.5]
        a[, brood_sex_ratio := sum(sex)/length(sex), by = rear_nest_breed_ID]
    # body mass index
        a[, body_mass_index := day_14_weight/day_14_tarsus_length^2]

# dataset for chick mortality
    mmo = a[complete.cases(a),.(change_chick_n, net_rearing_manipulation, d14_rear_nest_brood_size, brood_sex_ratio, day14_measurer, rear_area, rear_nest_OH_l, hatch_year,rear_nest_breed_ID)]
    mou = unique(mmo)
    nrow(mou)
    length(unique(mmo$rear_nest_breed_ID))
    mou[, net_rearing_manipulation_factor := as.factor(net_rearing_manipulation)]

# dataset for paired analyses
    # dataset for paired plots
    BT_data <- read.csv(file = "Data/blue_tit_data.csv", stringsAsFactors = FALSE)
    BT_data_tidy <-
      BT_data %>% 
      # select variables of interest
      dplyr::select(chick_ring_number, hatch_year, hatch_nest_breed_ID, rear_nest_breed_ID, rear_area, hatch_mom_Ring, 
             genetic_dad_ring_.WP_or_EP., hatch_nest_OH, rear_nest_OH, d0_hatch_nest_brood_size, rear_d0_rear_nest_brood_size,
             rear_nest_trt, net_rearing_manipulation, rear_Cs_at_start_of_rearing, d14_rear_nest_brood_size, 
             day_14_tarsus_length, day_14_weight, day14_measurer, chick_sex_molec, number_chicks_fledged_from_rear_nest) %>%
      # rename the genetic dad vaiable
      dplyr::rename(genetic_dad_ring = genetic_dad_ring_.WP_or_EP., 
             d0_rear_nest_brood_size = rear_d0_rear_nest_brood_size) %>% 
      # replace all "." with NA in factor variables
      dplyr::mutate(chick_sex_molec = as.factor(ifelse(chick_sex_molec == ".", NA, 
                                    ifelse(chick_sex_molec == "1", "Female", "Male"))),
             chick_BMI = day_14_weight/(day_14_tarsus_length^2)) %>%
      dplyr::mutate_at(c("hatch_year", "hatch_nest_breed_ID", "rear_nest_breed_ID", 
                  "day14_measurer", "rear_nest_trt", "hatch_mom_Ring", 
                  "genetic_dad_ring", "chick_ring_number", "rear_area"), 
                as.factor) %>% 
      dplyr::mutate_if(is.factor, list(~dplyr::na_if(., "."))) %>% 
      dplyr::mutate_at(c("net_rearing_manipulation", "rear_Cs_at_start_of_rearing", "d0_hatch_nest_brood_size", 
                  "d0_rear_nest_brood_size", "number_chicks_fledged_from_rear_nest"), 
                as.numeric) %>% 
      # re-name the treatment groups
      dplyr::mutate(rear_nest_trt = 
               as.factor(ifelse(rear_nest_trt == "5", "increase", 
                            ifelse(rear_nest_trt == "6", "decrease", "control")))) %>% 
      dplyr::mutate(prop_change_in_brood_size = net_rearing_manipulation/d0_hatch_nest_brood_size,
                    change_chick_n = rear_Cs_at_start_of_rearing - d14_rear_nest_brood_size)
    BT_nest_pairs <- 
      BT_data_tidy %>% 
      dplyr::group_by(chick_ring_number) %>% 
      dplyr::mutate_at(c("hatch_nest_breed_ID", "rear_nest_breed_ID"), as.character) %>% 
      dplyr::mutate_at(c("hatch_nest_breed_ID", "rear_nest_breed_ID"), as.numeric) %>% 
      dplyr::summarise(pair_ID_min = min(hatch_nest_breed_ID, rear_nest_breed_ID),
                pair_ID_max = max(hatch_nest_breed_ID, rear_nest_breed_ID)) %>% 
      dplyr::left_join(BT_data_tidy, ., by = "chick_ring_number") %>% 
      dplyr::select(hatch_nest_breed_ID, rear_nest_breed_ID, d0_hatch_nest_brood_size,
             d0_rear_nest_brood_size, rear_nest_trt, net_rearing_manipulation,
             rear_Cs_at_start_of_rearing, pair_ID_min, pair_ID_max) %>%
      dplyr::distinct() %>% 
      dplyr::filter(pair_ID_min != pair_ID_max) %>% 
      dplyr::mutate(pair_ID = paste(pair_ID_min, pair_ID_max, sep = "_")) %>% 
      dplyr::select(hatch_nest_breed_ID,rear_nest_breed_ID, pair_ID)

    BT_data_tidy <- 
        BT_data_tidy %>% 
        dplyr::left_join(., BT_nest_pairs[,c('hatch_nest_breed_ID', 'pair_ID')], by = "hatch_nest_breed_ID")

    p = data.table(BT_nest_pairs)
    p[, hatch_nest_breed_ID:= as.integer(hatch_nest_breed_ID)]
    p[, rear_nest_breed_ID:= as.integer(rear_nest_breed_ID)]
    p = merge(a,p[,.(rear_nest_breed_ID, pair_ID)], all.x = TRUE)
    p = p[!is.na(pair_ID)]
    


# END
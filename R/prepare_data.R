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
    p = fread('Data/blue_tit_nest_pairs_IDs.csv') 
    p = merge(a,p[,.(rear_nest_breed_ID, pair_ID)], all.x = TRUE)
    p = p[!is.na(pair_ID)]
    
# END
################################################################.
#             FABIO Footprints - function library
################################################################.


# this script creates the functions needed to calculate, organize, and visualize footprint results.
# NOTE: these functions do not calculate anything yet. They are used in the main code to calculate results based on given input parameters (e.g. country, year...)
# each function includes a short description of its arguments.

#----------------------------------------------------------------------------#
# ---- Functions to calculate footprints and organize results --------
#----------------------------------------------------------------------------#


## function to calculate footprints for a given final demand vector -------

# arguments:
# country = the country of interest
# consumption = the final demand type ("food", "other"...)
# allocation = one of "value" or "mass"
# year = the year of interest

footprint <- function(country = "AUT", consumption = "food", allocation = "value", year = 2013, y, X = X, E = E, v = vers){

  # extract data
  Xi <- X[, as.character(year)]
  #Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]

  if(allocation == "value") {
    L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/losses/",year,"_L_value.rds"))
  } else {
    L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/losses/",year,"_L_mass.rds"))
  }

  # calculate environmental intensities
  E_int <- Ei[,c("landuse","biomass","blue","green","biodiv","n_application","p_application", "ghg", "luh")]
  E_int <- E_int/Xi
  E_int[!is.finite(E_int)] <- 0
  E_int <- cbind(Ei[,1:7], E_int)

  # extract relevant final demand vector
  #Y_country <- Yi[, Y_codes$iso3c == country]
  #colnames(Y_country) <- Y_codes$fd[Y_codes$iso3c == country]
  #Y_target <- as.vector(as.matrix(Y_country[,consumption]))
  Y_target <- y

  # compute footprints

  # production footprint (= production of each commodity triggered by consumption of each commodity in Y_target)
  #FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))
  FP <- t(t(L) * Y_target)
  colnames(FP) <- rownames(FP) <- short_index <- paste0(index$iso3c, "_", index$item)
  FP <- as(FP, "dgTMatrix")
  results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], production =FP@x)
  results[,`:=`(country_origin = substr(origin,1,3),
                item_origin = substr(origin,5,100),
                country_target = substr(target,1,3),
                item_target = substr(target,5,100),
                country_consumer = country,
                year = year)]

  # remove L and FP matrices from memory (they are large!)
  rm(L,FP, Xi, Ei); gc()

  # calculate environmental footprints for all extensions from production footprint
  match_origin = match(results$origin, short_index)
  match_target = match(results$target, short_index)

  results[, `:=` (origin = NULL, target = NULL) ]

  results[,`:=`(landuse = production * E_int$landuse[match_origin], # replace by lapply E_int[[SD]][match_margin]
                biomass = production * E_int$biomass[match_origin],
                blue    = production * E_int$blue[match_origin],
                green   = production * E_int$green[match_origin],
                biodiv   = production * E_int$biodiv[match_origin],
                n_application   = production * E_int$n_application[match_origin],
                p_application   = production * E_int$p_application[match_origin],
                ghg     = production * E_int$ghg[match_origin],
                luh     = production * E_int$luh[match_origin])]

  # add direct consumption of each item for reference (optional)
  #results[, direct_consumption := Y_target[match_origin]]

  # add auxiliary info on origin and target commodities
  results[,`:=`(group_origin  = index$comm_group[match_origin],
                group_target  = index$comm_group[match_target],
                iso_origin = index$iso3c[match_origin],
                continent_origin  = index$continent[match_origin])]
  results[country_origin==country, continent_origin := country]

  # reorder columns
  results <- results %>%
    relocate(c(production, landuse:luh), .after = continent_origin) %>%
    relocate(c(iso_origin, continent_origin), .after = country_origin) %>%
    relocate(group_origin, .after = item_origin) %>%
    relocate(group_target, .after = item_target)


  return(results)
}


##  function to extract final demand vector being investigated -------

# arguments:
# Y = the Y list
# year = the year of interest
# country = the country of interest
# type = the final demand type ("food", "other"...)

get_demand <- function(Y = Y, year = 2013, consumption = "food", country = NA){
  Yi <- Y[[as.character(year)]]
  Y_country <- Yi[, Y_codes$iso3c == country]
  colnames(Y_country) <- Y_codes$fd[Y_codes$iso3c == country]
  Y_target <- as.vector(as.matrix(Y_country[,consumption]))
}


## define function to add per-capita-values to a footprint table -----------
# arguments:
# fp = the footprint data.table
add_per_capita <- function(fp){
  pop_data <- as.data.table(wbstats::wb_data(indicator = "SP.POP.TOTL", start_date = min(fp$year), end_date = max(fp$year)))
  pop_data <- pop_data[,.(iso3c, year = date, pop = SP.POP.TOTL)]
  attributes(pop_data$pop)$label <- NULL
  fp <- merge(fp, pop_data, by.x = c("country_consumer", "year"), by.y = c("iso3c", "year"))
  fp[, `:=` (production_pc = production/pop,
             landuse_pc = landuse/pop,
             biomass_pc = biomass/pop,
             blue_pc = blue/pop,
             green_pc = green/pop,
             ghg_pc = ghg/pop,
             luh_pc = luh/pop)]
}


##  function for easy aggregation of footprint table --------------------------------

# arguments:
# fp = the footprint data.table
# aggregate_by = a vector of columns to aggregate results by
# indicators = indicators to keep (default is all of them)
fp_aggregate <- function(fp, aggregate_by, indicators = c("landuse", "biomass", "green", "blue", "ghg", "luh", "biodiv", "n_application", "p_application")){
  indicators <- names(fp)[grep(paste(indicators, collapse = "|"), names(fp))]
  fp <- fp[, lapply(.SD, sum, na.rm=TRUE), by = aggregate_by, .SDcols = indicators]
}

## auxiliary functions  ------------
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))



#----------------------------------------------------------------------------#
# ---------     Functions to visualize results         --------
#----------------------------------------------------------------------------#



## footprint map visualization  ----------------------------------------------------------------

# arguments:
# fp = the footprint table (can be the raw fp_country table without any aggregation)
# map = map file to be plotted (world_map)
# indicator = the indicator to be plotted
# per_capita = should results be by capita or aggregate (TRUE/FALSE)
# origin_items = either "ALL" for aggregating over all origin items or a vector of product names (e.g. "Soyabeans") or code (e.g. 2555)
# target_items = either "ALL" for aggregating over all target items or a vector of product names (e.g. "Pigmeat") or code (e.g. 2733)
# origin_groups = if "origin_items" is not used, you can instead provide a vector of comm_group names (e.g. "Cereals")
# target_groups = if "target_items" is not used, you can instead provide a vector of comm_group names (e.g. "Meat")



fp_map <- function(fp, map = world_map, indicator = "landuse", per_capita = FALSE,
                   origin_items = "ALL", target_items = "ALL", origin_groups, target_groups, title = ""){

  # extract and aggregate accorging to input
  if (!missing(origin_groups)){
    origin_items <- "ALL"
    fp <- fp[group_origin %in% origin_groups,]
    cat("origin_groups is used and overwrites the origin_item argument")
  }
  if (!missing(target_groups)) {
    target_items <- "ALL"
    fp <- fp[group_target %in% target_groups,]
    cat("target_groups is used and overwrites the target_item argument")
  }

  if(is.numeric(target_items[1])) target_items <- items$item[match(target_items, items$item_code)]
  if(is.numeric(origin_items[1])) target_items <- items$item[match(target_items, items$item_code)]

  # filter (if needed) and aggregate by origin country
  if (target_items != "ALL") fp <- fp[item_target %in% target_items,]
  if (origin_items != "ALL") fp <- fp[item_target %in% origin_items,]
  fp <- fp_aggregate(fp, aggregate_by = c("country_origin"))

  # merge fp with world map
  world_fp <- left_join(map, fp, by = c("ISO_A3" = "country_origin"))

  # set unit for legend
  indicators_long <-  c("landuse" = "Flächenverbrauch", "biomass" = "Biomasse", "blue" = "Süßwasser- <br>verbrauch", "green" = "Grünes Wasser", "ghg" = "Emissionen", "luh" = "Emissionen", "ghg_all" = "Emissionen",
                        "biodiv" = "Biodiversitätsverlust", "n_application" = "Stickstoff", "p_application" = "Phosphor")
  indicator_long <- indicators_long[indicator]
  units <- c("landuse" = "m<sup>2</sup>", "biomass" = "t", "blue" = "m<sup>3</sup>", "green" = "m<sup>3</sup>", "ghg" = "t CO<sub>2</sub>-eq.", "luh" = "t CO<sub>2</sub>-eq.", "ghg_all" = "t CO<sub>2</sub>-eq.",
             "biodiv" = "10^-6 Spezien", "n_application" = "kg", "p_application" = "kg")
  unit = units[indicator]

  if(per_capita) indicator <- paste0(indicator,"_pc")

  # plot
  ggplot(data = world_fp) +
    geom_sf(aes(fill = .data[[indicator]]), size = 0.05) +
    labs(fill=paste(indicator_long, " <br> in", unit), title = title) +
    scale_fill_viridis_c(direction = -1, na.value = "lightgrey") +
    coord_sf(crs = "+proj=robin") + # "+proj=moll"   "+proj=wintri"
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot", legend.title = element_markdown()) #legend.position = "right"

}



## mosaic plot -------------------------------------------------

# arguments:

# fp = the footprint table (can be the raw fp_country table without any aggregation)
# indicator = the indicator to be plotted
# per_capita = should results be by capita or aggregate (TRUE/FALSE)
# target_items = optional, a vector of target items to subset results
# target_groups = optional, a vector of target commodity groups to subset results
# divide_by_cells = a factor by which the values displayed in the cells are divided (default 1000)
# divide_by_axis = a factor by which the values displayed on the x-axis are divided (default 1000000)
# display_min = a threshold defining the minimum value to be plotted in the cells to avoid overplotting
# axis_label = character string giving the x-axis label (depends on indicator chose, e.g. "Million hectares")
# plot_title = character string giving the title of the plot
# tick_offset = a vector of length 9 offsetting the tick marks on the x-axis. This has to be adapted manually for each plot


fp_mosaic <- function(fp, indicator, per_captia = FALSE, consumer_country = "AUT",
                      target_items, target_groups, aggregate_by =  c("group_target", "continent_origin"),
                      divide_by_cells = 1000, divide_by_axis = 1000000, axis_label = "Million hectares",
                      display_min = 10, round_digs = 0,
                      plot_title = "Land-use footprint by origin country and commodity group",
                      tick_offset = c(0)) {


  # if target item or group was given, filter the fp table accordingly
  if(!(missing(target_items)) & !(missing(target_groups))) stop("either specify 'target_items' or 'target_groups', but not both")
  if(!missing(target_items)) fp <- fp[item_target %in% target_items,]
  if(!missing(target_groups)) fp <- fp[group_target %in% target_groups,]

  # aggregate data for mosaic format
  fp_mosaic <- fp_aggregate(fp, aggregate_by = aggregate_by)
  setnames(fp_mosaic, aggregate_by, c("group", "region"))


  # select indicator
  if(per_captia) indicator <- paste0(indicator,"_pc")
  setnames(fp_mosaic, indicator, "value")

  # change unit of indicator

  fp_mosaic[,value := round(value/divide_by_cells, round_digs)]
  fp_mosaic <- fp_mosaic[value > 0,]

  mycols <- food_cols_vect[names(food_cols_vect) %in% unique(fp_mosaic$group)]
  fp_mosaic[,`:=` (group = factor(group, levels = names(mycols)),
                   region = factor(region, levels = c(consumer_country, "EU", "EUR", "ASI", "AFR", "LAM", "NAM", "OCE", "ROW")))]


  #mycols=c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black",
  #         "gold1", "skyblue2", "#FB9A99", "palegreen2", "maroon", "#CAB2D6", "orchid1", "deeppink1", "blue1",
  #         "steelblue4", "darkturquoise", "green1", "yellow4", "yellow3",
  #         "darkorange4")



  scale_x_productlist <- function (name = waiver(), breaks = ggmosaic:::product_breaks(), minor_breaks = NULL,
                                   labels = ggmosaic:::product_labels(), limits = NULL, expand = waiver(),
                                   oob = scales:::censor, na.value = NA_real_, trans = "identity",
                                   position = "bottom", sec.axis = waiver())
  {
    sc <- ggplot2::continuous_scale(c("x", "xmin", "xmax", "xend",
                                      "xintercept", "xmin_final", "xmax_final", "xlower",
                                      "xmiddle", "xupper"), "position_c", identity, name = name,
                                    breaks = breaks, minor_breaks = minor_breaks, labels = labels,
                                    limits = limits, expand = expand, oob = oob, na.value = na.value,
                                    trans = trans, guide = "none", position = position,
                                    super = ScaleContinuousProduct)
    if (!ggplot2:::is.waive(sec.axis)) {
      if (ggplot2:::is.formula(sec.axis))
        sec.axis <- ggplot2::sec_axis(sec.axis)
      is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
      if (!ggplot2:::is.sec_axis(sec.axis))
        stop("Secondary axes must be specified using 'sec_axis()'")
      sc$secondary.axis <- sec.axis
    }
    sc
  }

  breaks_values <- fp_mosaic %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(value = sum(value) / (divide_by_axis/divide_by_cells)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = cumsum(value) )

  # change to string for labelling
  breaks_values$region <- as.character(breaks_values$region)

  # debugonce(check_breaks_labels)
  # debugonce(geom_mosaic)
  mosaic <-
    ggplot(data = fp_mosaic) +
    geom_mosaic(aes(weight = value, x = product(group, region), fill = group), na.rm=T, divider=ddecker(), offset = 0.005) +
    #theme(axis.text.x=element_text(angle=-25, hjust= .1), legend.position="right") +
    theme(legend.position="right") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_productlist(position = "top", #  labels = breaks_values$region, breaks = breaks_values$value
                        sec.axis = ggplot2::sec_axis(~.*sum(fp_mosaic$value/(divide_by_axis/divide_by_cells)),
                                                     breaks = c(0,breaks_values$value) + tick_offset,
                                                     labels = round(c(0,breaks_values$value), 2), name = axis_label)) +
    # facet_grid(Group~.) +
    labs(x = "", y = "Anteil je Produktgruppe") +
    guides(fill=guide_legend(title = "Commodities", reverse = TRUE)) +
    # viridis::scale_fill_viridis(option = "magma", discrete = TRUE)
    scale_fill_manual(values = mycols)
    #ggsci::scale_fill_npg()


  (mosaic <- mosaic +
      geom_text(data = ggplot_build(mosaic)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=replace(.wt, .wt < display_min, "")))+
      ggtitle(label = plot_title) +
      theme(axis.text.x = element_text(angle = 50, hjust=1), plot.title.position = "plot",
            plot.title = element_text(margin=margin(0,0,25,0))))

  return(mosaic)

}



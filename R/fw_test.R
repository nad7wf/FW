#' Perform Finlay-Wilkinson Test
#'
#' Use Finlay-Wilkinson test to test for GxE interaction by comparing (GE ~ Location mean) regression coefficient against a slope of 1.
#'
#' @param GE_dat A data.frame containing yield values for each GE at tested locations. The first two columns must be 'GE' and 'Year', followed by columns of yield data at each location. See example data below.
#' @param Loc_dat A data.frame of location means for each location in GE_dat. Col names: 'Loc', 'Year', 'Loc_mean'.
#' @param plot_GE (optional) A numeric GE ID to be plotted.
#' @param test_slope The slope that the FW test should be performed against (default = 1).
#'
#' @return
#' If plot_GE is not specified: returns a data.frame containing a slope and p-value for each GE/Year combination. \cr \cr
#' If plot_GE is specified: returns a list, where the first element is a data.frame of slopes and p-values for all GE/Year combinations, and the second element
#' is a gtable containing the plot for the specified GE. The plot can be drawn using grid::grid.draw(list_name[[2]]).
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' ge <- matrix(rnorm(405, c(75, 78)), nrow = 27, ncol = 15) %>%
#'     tibble::as_tibble() %>%
#'     stats::setNames(paste0("Loc", seq(1, 15))) %>%
#'     dplyr::mutate(GE = rep(1:9, each = 3),
#'                   Year = rep(2018:2020, times = 9)) %>%
#'     dplyr::select(GE, Year, everything())
#'
#' loc <- plyr::ddply(ge, "Year", function (Year_group) {
#'     purrr::map_dfr(Year_group[, 3:ncol(Year_group)], mean)
#' }) %>%
#'     tidyr::gather(key = "Loc", value = "Loc_mean", -Year) %>%
#'     dplyr::select(Loc, Year, Loc_mean)
#'
#' results <- fw_test(ge, loc, 6)
fw_test <- function (GE_dat, Loc_dat, plot_GE = as.numeric(NA), test_slope = 1) {


    ### Combine GE Yield and Location means dataframes and run hypothesis test against test_slope.
    GE_Loc_combined_dat <- tidyr::gather(GE_dat, key = "Loc", value = "GE_Yield", -c("GE", "Year")) %>%
        dplyr::full_join(Loc_dat, by = c("Year", "Loc"))

    stats <- plyr::ddply(GE_Loc_combined_dat, c("GE", "Year"), function (GE_group) {
        r.x <- lm(GE_Yield ~ Loc_mean, dat = GE_group)
        t_stat <- (r.x$coefficients[[2]] - test_slope) / coef(summary(r.x))[2, 2]

        2*pt(t_stat, df = r.x$df.residual, lower.tail = (t_stat < 0)) %>%
            tibble::tibble(slope = r.x$coefficients[[2]],
                           pval = .)
    })


    ### If plot_GE is specified, and GE ID exists in the GE_dat dataframe, plot Loc_mean x GE_Yield data.
    if (!is.na(plot_GE) & sum(plot_GE %in% GE_dat[["GE"]]) > 0) {

        ### Filter for GE of interest and merge with stats output.
        single_ge_dat <- dplyr::filter(GE_Loc_combined_dat, GE == plot_GE) %>%
            dplyr::inner_join(stats, by = c("GE", "Year")) %>%
            dplyr::mutate(label_xpos = min(.$Loc_mean),
                          label_ypos = max(.$GE_Yield))


        ### Generate statistics coordinates for grand regression from all years.
        r.grand.x <- lm(GE_Yield ~ Loc_mean, dat = single_ge_dat)
        t_stat_grand <- (r.grand.x$coefficients[[2]] - test_slope) / coef(summary(r.grand.x))[2, 2]

        slope_grand <- r.grand.x$coefficients[[2]]
        pval_grand <- 2*pt(t_stat_grand, df = r.grand.x$df.residual, lower.tail = (t_stat_grand < 0))

        grand_stat_labels <- tibble::tibble(Year = unique(single_ge_dat$Year) %>% median() %>% floor(),
                                            slope = slope_grand,
                                            pval = pval_grand,
                                            label_xpos = max(single_ge_dat$Loc_mean),
                                            label_ypos = min(single_ge_dat$GE_Yield))



        ### Generate faceted plot for GE of interest.
        plot <- ggplot2::ggplot(data = single_ge_dat,
                                ggplot2::aes(x = Loc_mean, y = GE_Yield)) +
            ggplot2::geom_point(ggplot2::aes(color = as.character(Year))) +
            ggplot2::geom_smooth(ggplot2::aes(color = as.character(Year)),
                                 formula = "y ~ x",
                                 method = 'lm',
                                 se = FALSE) +
            ggplot2::geom_label(ggplot2::aes(x = label_xpos,
                                             y = label_ypos,
                                             label = paste0("Slope: ", round(slope, digits = 2), "\n",
                                                            "p-value: ", round(pval, digits = 2)),
                                             fill = as.character(Year),
                                             fontface = "bold",
                                             lineheight = "0.75",
                                             hjust = 0.05,
                                             vjust = 0.8),
                                stat = "identity",
                                color = "white",
                                size = 3.5) +
            ggplot2::geom_label(ggplot2::aes(x = label_xpos,
                                             y = label_ypos,
                                             label = paste0("Slope: ", round(slope, digits = 2), "\n",
                                                            "p-value: ", round(pval, digits = 2)),
                                             fontface = "bold",
                                             lineheight = "0.75",
                                             hjust = 0.95,
                                             vjust = 0.2),
                                data = grand_stat_labels,
                                stat = "identity",
                                color = "white",
                                fill = "#787878",
                                size = 3.5) +
            ggplot2::labs(title = paste("GE Yield ~ Location Mean for GE:", plot_GE, sep = " "),
                          x = "Location Mean",
                          y = "GE Yield") +
            ggplot2::scale_x_continuous(limits = c(min(single_ge_dat$Loc_mean),
                                                   max(single_ge_dat$Loc_mean))) +
            ggplot2::scale_y_continuous(limits = c(min(single_ge_dat$GE_Yield),
                                                   max(single_ge_dat$GE_Yield))) +
            ggplot2::facet_wrap(~Year,
                                strip.position = "bottom") +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                              face = "bold",
                                                              hjust = 0.5),
                           panel.spacing = ggplot2::unit(0, "lines"),
                           legend.position = "none",
                           strip.background = ggplot2::element_blank(),
                           strip.placement = "outside",
                           strip.text = ggplot2::element_text(size = 12,
                                                              face = "bold"),
                           axis.title = ggplot2::element_text(size = 16,
                                                              face = "bold"),
                           axis.text = ggplot2::element_text(size = 12))


        ### Build gtable from ggplot.
        plot_build <- ggplot2::ggplot_build(plot)
        plot_gtable <- ggplot2::ggplot_gtable(plot_build)


        ### Create plotting coordinates for grand regression line.
        grand_reg_coordinates <- tibble::tibble(Loc_mean = c(min(single_ge_dat$Loc_mean), max(single_ge_dat$Loc_mean)))
        grand_reg_coordinates$GE_Yield <- predict(r.grand.x, newdata = grand_reg_coordinates)

        grand_reg_y_coor <- scales::rescale(c(plot_build$layout$panel_params[[1]][["y.range"]],
                                              grand_reg_coordinates[[2]]),
                                            c(0, 1))[3:4]

        plot_gtable <- gtable::gtable_add_grob(plot_gtable, grid::linesGrob(x = ggplot2::unit(c(0.015, 0.985), "npc"),
                                                                            y = ggplot2::unit(grand_reg_y_coor, "npc"),
                                                                            gp = grid::gpar(lwd = 2,
                                                                                            col = "#787878")),
                                               t = 7,
                                               l = 5,
                                               ### Find the cell position of the last panel from gtable.
                                               r = max(plot_gtable$layout[grepl("panel", plot_gtable$layout$name),]$r),
                                               z = Inf)

        ### Turn off clipping to enable plotting grand regression across facets. Then plot and save results to list.
        plot_gtable$layout$clip <- "off"
        grid::grid.newpage()
        grid::grid.draw(plot_gtable)
        return(list(stats, plot_gtable))

        ### If GE ID for plotting doesn't exist in GE_dat dataframe, return an error.
    } else if (!is.na(plot_GE) & sum(plot_GE %in% GE_dat[["GE"]]) == 0) {

        print("GE to plot does not exist in supplied dataframe.")

        ### If no GE ID is specified for plotting, just return dataframe of pvals.
    } else if (is.na(plot_GE)) {

        return(stats)

    }

}

# 1.2 每年最大的一场事件的累计面积
{
    # df <- melt_list(lst, "year")
    I <- which(df[, order(-area_sum), .(nCellInter, prob, year)]$V1 <= 1)
    df_max <- df[I, ]
    
    df_area <- df_max[, .(year, area_mean, area_sum, area_max, global_areaMX, prob, nCellInter)] %>% 
        melt(id.vars = c("year", "prob", "nCellInter")) %>% 
        .[, .(value = sum(value, na.rm = TRUE)), .(year, variable, prob, nCellInter)]
    df_area$value %<>% divide_by(1e4)
    df_area[, smooth := movmean(value, halfwin = 2), .(variable, prob, nCellInter)]
    df_area[, flag := check_outlier(value), .(variable, prob, nCellInter)]
    
    varnames <- c("area_mean", "area_max", "global_areaMX", "area_sum")
    foreach(varname = varnames) %do% {
        dpat <- df_area[variable == varname, ]
        p2 <- ggplot(dpat, aes(year, value)) + 
            geom_point() + 
            # facet_wrap(~variable, scales = "free") +
            facet_grid(prob~nCellInter, scales = "free") + 
            geom_text_repel(data = dpat[!is.na(flag)], aes(label = year)) + 
            geom_line(aes(y = smooth)) + 
            scale_y_log10() + 
            geom_smooth() +
            labs(y = expression("Affected area of the yearly biggest HW event (10"^3 * " " * km^2 * ")"))
        outfile <- glue("Figure{fig.no}.2 {prefix} {varname} of yearly biggest HW area.pdf")
        write_fig(p2, outfile, 12, 6)    
    }
}

# 1.3 全所有热浪累加
{
    # df <- melt_list(lst, "year")
    # df$year %<>% as.integer()
    df_area <- df[, .(year, area_mean, area_sum, prob, nCellInter)] %>% 
        melt(c("year", "prob", "nCellInter")) %>% 
        .[, .(value = sum(value, na.rm = TRUE)), .(year, variable, prob, nCellInter)]
    df_area$value %<>% divide_by(1e4)
    df_area[, `:=`(smooth = movmean(value, halfwin = 2), 
                   flag   = check_outlier(value)), .(variable, prob, nCellInter)]
    
    dpat <- df_area[variable == "area_sum", .(value = sum(value), smooth = sum(smooth)), 
                    .(year, variable, prob, nCellInter)] %>% 
        .[, flag := check_outlier(value), .(variable, prob, nCellInter)]
    p3 <- ggplot(dpat, aes(year, value)) + 
        geom_point() + 
        # geom_point(aes(year, smooth)) + 
        geom_line(aes(year, smooth)) + 
        geom_text_repel(data = dpat[!is.na(flag)], aes(label = year)) + 
        # facet_wrap(~variable, scales = "free", ncol = 2) + 
        facet_grid(prob~nCellInter, scales = "free") + 
        labs(y = expression("Affected area of the yearly HW event (10"^3 * " " * km^2 * ")"))
    outfile <- glue("Figure{fig.no}.3 {prefix} yearly HW area.pdf")
    write_fig(p3, outfile, 12, 6)
}


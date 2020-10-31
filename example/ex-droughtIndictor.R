julia_eval("")

r <- droughtIndicator(mat, mask, SMI_thld)
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)

{
    system.time(r_status <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList))
}

## 2. visualization ------------------------------------------------------------
# devtools::install_github("kongdd/Ipaper")
{
    p <- plot.cluster(r_cluster$idCluster, 1:6)
    ratio <- 0.8
    write_fig(p, "b.pdf", 12 * ratio, 8 * ratio)
}

{
    p <- plot.cluster(idC, 1:6)
    ratio <- 0.8
    write_fig(p, "r_julia.pdf", 12 * ratio, 8 * ratio)
}

{
    p <- plot.cluster(idC2, 1:6)
    ratio <- 0.8
    write_fig(p, "r_julia_final.pdf", 12 * ratio, 8 * ratio)
}

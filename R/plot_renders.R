output$gwas_manhattan <- renderPlot(
  manhattan(manhattan_gwas$data, featureCol = manhattan_gwas$featureCol, chrCol = manhattan_gwas$chrCol,
            posCol = manhattan_gwas$posCol, pvalCol = manhattan_gwas$pvalCol)
)
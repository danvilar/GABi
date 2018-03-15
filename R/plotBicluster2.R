plotBicluster2 <- function(x,dataset, label=1, col=gray(seq(from=1,to=0,length=50))){
    library(gplots)

    rainbow12equal = c("#BF4D4D",
        "#BF864D",
        "#BFBF4D", 
        "#86BF4D",
        "#4DBF4D", 
        "#4DBF86", 
        "#4DBFBF", 
        "#4D86BF", 
        "#4D4DBF", 
        "#864DBF", 
        "#BF4DBF", 
        "#BF4D86")
    
    sampleOrder = c(x$samples,setdiff(1:ncol(dataset),x$samples))    

    heatmap.2(dataset[x$features, sampleOrder],
        col = col,
        dendrogram = 'none',
        Colv = NA,
        #Rowv = NA,
        ColSideColors = c(rep("black",length(x$samples)),rep("white",ncol(dataset)-length(x$samples))),
        trace = "none", 
        colCol = rainbow12equal[label[sampleOrder]+1],
        key = F,
        lwid = c(0.1,4),
        lhei = c(0.1,4)
    )
}

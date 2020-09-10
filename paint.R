paint<-function(chunks, main=""){
  par(mar=c(5,2,4,6))
  
  plot(0, 
       type="n", 
       xlim=c(chunks[1,2],chunks[nrow(chunks),2]), 
       ylim=c(0,5), 
       bty='L', 
       yaxt="n", 
       yaxs = "i", 
       xaxs="i", 
       main=main,
       xlab="Position (bp)")
  
  # Paint each chunk one by one
  for (i in 1:nrow(chunks)){
    if (i==1){
      rect(0,0,chunks[i,2],5,col=(chunks[i,1]+1), border=NA)  # Do the painting
    }else{
      rect(chunks[(i-1),2],0,chunks[i,2],5,col=(chunks[i,1]+1),border=NA)
    }
  }
  legend(2e8,5, fill=1:11, legend=0:10, xpd=TRUE, cex=0.75,x.intersp = 0.25, text.width =5e6)
}

chunk.dist<-function(chunks){
  sizes<-c()
  for (i in 1:nrow(chunks)){
    if (i==1){
      sizes<-c(chunks[i,2])
    }else{
      size<-chunks[i,2]-chunks[(i-1),2]
      sizes<-c(sizes,size)
    }
  }
  return(sizes)
}

plot.chunks <- function(chunks_a, chunks_b, a, b){
  chunks_plot_a <- chunks_a %>% filter(V2 > a) %>% filter(V2 < b)
  chunks_plot_b <- chunks_b %>% filter(V2 > a) %>% filter(V2 < b)
  par(mfrow=c(3,1), mar=c(3,4,3,2))
  
  par(mfrow=c(3,1))
  paint(chunks_plot_a, "Top plot")
  paint(chunks_plot_b, "Bottom plot")
  # paint(chunks_meso, "Painting of Modern 0 chromosome 3 painted with RELATE tree sequence of MesoNeo+GBR data")
  # legend(2e8,20, fill=1:11, legend=0:10, cex=0.8,x.intersp = 0.5,y.intersp = 0.7, text.width =5e6, xpd="NA", bty="n")  
}

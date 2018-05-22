#get data
all <- 
  do.call(rbind,
          lapply(list.files(path = "/Users/lyumu/Dropbox/2018muxuan/analysis/data_ratings/", 
                            full.names = TRUE), 
                 read.csv)) 
all_final <-na.omit(all)
all_final <- all_final[,-39]

#get the inverse ratings, mean, and sd
all_final<-data.frame(all_final[1], apply(all_final[2:38],2, function(x) + 1000 -x) )
all_final$Avg = apply(all_final[,c(2:38)],1,mean, na.rm = TRUE) 
all_final$SD = apply(all_final[,c(2:38)], 1, sd, na.rm = TRUE)

#get llvf data
all_llvf <- do.call(rbind,
                    lapply(list.files(path = "/Users/lyumu/Dropbox/2018muxuan/analysis/data_low_level_features/videos/", 
                                      full.names = TRUE), 
                           read.csv)) 
all_llvf <- all_llvf[,c(-1,-12)]
all_llvf$Type <- factor(all_llvf$Type)
#rename the levels
levels(all_llvf$Type) <- c("Flat", "Mount", "Hill")

#get semantics data
semantics <- read.csv("/Users/lyumu/Dropbox/2018muxuan/analysis/data_semantics/semantics.csv")
semantics <- semantics[1:1428,c(-1,-7)]
names(semantics)[c(6)] = c("video_type")
#reorder as flat, mount, hill
all_semantics <- rbind(semantics[1:480,], semantics[961:1428,], semantics[481:960,])
levels(all_semantics$video_type) <- c("Flat 1", "Flat 2", "Mount 1", "Mount 2", "Hill 1", "Hill 2")

#bind three datasets
ratings_llvf_sem <- cbind(all_final, all_llvf, all_semantics)
#create a csv 
write.csv(ratings_llvf_sem, file = "ratings_llvf_sem.csv")



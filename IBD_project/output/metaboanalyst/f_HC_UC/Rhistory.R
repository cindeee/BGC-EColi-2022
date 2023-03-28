# PID of current job: 2891635
mSet<-InitDataObjects("conc", "pathora", FALSE)
cmpd.vec<-c("HMDB0000244","HMDB0000446","HMDB0000251","HMDB0002172","HMDB0000893","HMDB0000626","HMDB0000157","HMDB0000752","HMDB0000448","HMDB0000929","HMDB0000500","HMDB0000422","HMDB0003331","HMDB0000159","HMDB0000254","HMDB0001138","HMDB0000661","HMDB0000631","HMDB0000784","HMDB0004620","HMDB0000678","HMDB0001032","HMDB0011103","HMDB0000708","HMDB0002123","HMDB0003334","HMDB0013677","HMDB0028942","HMDB0001844","HMDB0244966","HMDB0255727","HMDB0000730","HMDB0005807","HMDB0000687","HMDB0000881","HMDB0000956","HMDB0000729","HMDB0062640","HMDB0001991","HMDB0061384","HMDB0013713","HMDB0000152","HMDB0000301","HMDB0001847","HMDB0000822","HMDB0001406","HMDB0012275","HMDB0000226","HMDB0006029","HMDB0001325","HMDB0000235","HMDB0013676","HMDB0000721","HMDB0000641","HMDB0000158","HMDB0000355")
mSet<-Setup.MapData(mSet, cmpd.vec);
mSet<-CrossReferencing(mSet, "hmdb");
mSet<-CreateMappingResultTable(mSet)
mSet<-PerformDetailMatch(mSet, "HMDB0244966");
mSet<-GetCandidateList(mSet);
mSet<-SetKEGG.PathLib(mSet, "hsa", "current")
mSet<-SetMetabolomeFilter(mSet, F);
mSet<-CalculateOraScore(mSet, "rbc", "hyperg")
mSet<-PlotPathSummary(mSet, F, "path_view_0_", "png", 72, width=NA, NA, NA )
mSet<-PlotKEGGPath(mSet, "Phenylalanine, tyrosine and tryptophan biosynthesis",576, 480, "png", NULL)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282612181.png",576.0, 480.0, 100.0)
mSet<-PlotKEGGPath(mSet, "Phenylalanine, tyrosine and tryptophan biosynthesis",576, 480, "png", NULL)
mSet<-PlotKEGGPath(mSet, "Phenylalanine, tyrosine and tryptophan biosynthesis",576, 480, "png", NULL)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282640153.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282640816.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282641999.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282642327.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282642905.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282644076.png",576.0, 480.0, 100.0)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282644737.png",576.0, 480.0, 100.0)
mSet<-PlotKEGGPath(mSet, "Phenylalanine metabolism",576, 480, "png", NULL)
mSet<-PlotKEGGPath(mSet, "Phenylalanine metabolism",576, 480, "png", NULL)
mSet<-PlotKEGGPath(mSet, "Phenylalanine metabolism",576, 480, "png", NULL)
mSet<-PlotKEGGPath(mSet, "Riboflavin metabolism",576, 480, "png", NULL)
mSet<-SaveTransformedData(mSet)
mSet<-PlotKEGGPath(mSet, "Phenylalanine, tyrosine and tryptophan biosynthesis",576, 480, "png", NULL)
mSet<-RerenderMetPAGraph(mSet, "zoom1669282839481.png",576.0, 480.0, 100.0)

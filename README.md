# Signac.UIO
An interactive R Shiny application for the comprehensive analysis and visualization of single-cell chromatin accessibility data. Leveraging the power of Signac, which integrates seamlessly with the Seurat package for single-cell RNA-seq analysis, Signac.UiO offers a user-friendly platform for conducting end-to-end scATAC-seq data analysis. The application supports quality control, cell filtering, dimensionality reduction, clustering, DNA motif analysis, transcription factor footprinting, and interactive data visualization. Signac.UiO is the only web server providing a comprehensive Signac-based interface for single-cell chromatin accessibility analysis. Users can upload their data, visualize results in real time, and export analysis outcomes in multiple formats, including PDF, PNG, JPEG, CSV, and TXT.
![](man/figures/pinpeline.png) The Shiny application is additionally hosted at <https://xulabgdpu.org.cn/signacShiny>.

## Local Installation (Optional)

### 1. Download Linkage Source Code
You can obtain the source code in either of the following ways:
  + Clone the GitHub repository:
    ```bash
    git clone https://github.com/luoyyyy/Signac.UIO.git
    ```
    Or

   + Download the ZIP file from: [https://github.com/luoyyyy/Signac.UIO](https://github.com/luoyyyy/Signac.UIO).  
   The downloaded folder should be named Linkage-main.


   
  
### 2. Install Required R Packages & Run Linkage
+ **Manual Package Installation**    
1.Open your R or RStudio environment.  
2.Install the required packages:  
    ```r
    install.packages(c("shiny", "shinyBS", "ggplot2", "wordcloud2", "plotly","enrichplot", "visNetwork","lubridate", "forcats", "stringr", "purrr", "readr", "tidyr", "tibble", "tidyverse","igraph", "dplyr", "ggpmisc", "ggpp", "data.table"，"shinyjs","ggupset","ggimage","ggpubr","shinyWidgets", "shinycssloaders", "shinydashboardPlus", "shinydashboard", "DT"))  

    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")  

    BiocManager::install(c("clusterProfiler", "rtracklayer", "BiocIO", "XVector"，"BSgenome","BSgenome.Hsapiens.UCSC.hg38", "GenomicRanges", "GenomeInfoDb","IRanges"，"S4Vectors", "BiocGenerics","Gviz","ChIPseeker","motifmatchr","org.Hs.eg.db","org.Mm.eg.db","TxDb.Mmusculus.UCSC.mm10.knownGene","BSgenome.Mmusculus.UCSC.mm10"))
    ```    
    ℹ️ A complete list of packages and versions can be found in the [sessionInfo](https://github.com/luoyyyy/Signac.UIO/blob/main/sessionInfo) file.  
    3.Run the application from the directory:
    ```r
    shiny::runApp("/path/to/Signac.UIO-main")
    ```
   
## Authors
Please do not hesitate to post an issue or contact the authors :

Siwen Xu: siwxu@gdpu.edu.cn

Yuyan Luo : 1620218029@qq.com
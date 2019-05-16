#' Empty theme ggplot
#'
#' Removes all text, gridlines, background color etc.
#'
#' @return ggtheme
#'
#' @examples
#'  ggplot2::ggplot(data.frame(x=rep(c(1, 2, 3, 4, 5), 2), y=1:10, d=rep(c("a", "b"), 5)), ggplot2::aes(x=x, y=y, fill=d))+
#'   ggplot2::geom_bar(stat="identity", position="dodge")+
#'   themehemeEmpty()
themeEmpty <- function(){

  theme_gray() %+replace% {
    theme(plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks=element_blank(),
          axis.text = element_blank(),
          plot.background = element_blank(), #nothing
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y =  element_blank(),
          legend.position = "none",
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.background = element_blank(),
          legend.text =  element_blank()

    )
  }

}


#' Replicates DN's RevPAR figure in ggplot2
#'
#' The function generates a figure often used by Dagens Næringsliv (DN), a Norwegian newspaper specializing in business news,
#' to present data on RevPAR and changes in RevPAR for diferent regions/cities.
#'
#' This function is early development. Expect revisions.
#'
#' @param df input data.frame containg place, revpar and change in decimals.
#' @param outputReady Logical. If TRUE, uses device size and is better suited for ouputs. Default is FALSE - fixed aspect ration.
#' @param customFont Logical. If TRUE, use the font family Montserrat (if available). Default is 'sans'
#' @param header Vector. Set header above columns. Takes a vector of length 3 with strings.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' exampleFrame <- data.frame(Sted = c("Tromsø", "Stanvager", "Bodø", "Trondheim", "Bergen", "Lillehammer", "Gardermoen", "Haugesund", "Kristiansund", "Oslo", "Kristiansand"),
#'                            RevPAR = c(925, 432, 605, 526, 391, 463, 601, 367, 370, 613, 335),
#'                            Endring = c(0.153, 0.137, 0.036, 0.014, 0.008, -0.005, -0.029, -0.054, -0.074, -0.082, -0.179))
#'
#' ggnewsPlot(exampleFrame)
ggnewsPlot <-  function(df, outputReady = FALSE, customFont = FALSE,
                        header = c("City", "RevPAR", "Change from same period last year")){


  #Error handling:

  if(length(header) != 3) stop("header must be of length 3. For no header, set empty entries in vector")

  normZ <- function(x){

    noSign <- x >= 0

    newMin <- min(abs(x))
    newMax <- 1
    x <- abs(x)
    scaledNums <- (newMax-newMin) * (x-min(x))/(max(x)-min(x))+newMin

    scaledNums[!noSign] <- -1*scaledNums[!noSign]

    scaledNums

  }

  df <- df %>%
    arrange(desc(Endring)) %>%
    mutate(yplace = nrow(df):1,
           xplace1 = 0,
           xplace2 = 1.5,
           xplace3 = 2.6,
           xplaceBar = 4,
           #TODO: Ad hoc løsning på scaling for bars. Undersøk mer
           normChange = ifelse(normZ(Endring)>0, normZ(Endring)+0.025, normZ(Endring)-0.025),
           segmentEnd = normChange+4,
           segmentCols = (ifelse(Endring > 0, "opp", "ned"))) %>%
    mutate(EndringOut = paste0(100*Endring, "%"))

  yAdjust <- 0.6
  xMax <- 5.5
  if(customFont){
    fontFam <- "Montserrat"
  } else {
    fontFam <- "sans"
  }

  ggOut <- ggplot(df, aes(x=xplace1, y=yplace, label = Sted))+
    ylim(0, (max(df$yplace)+1)*1.1)+
    xlim(0,xMax)+
    geom_text(hjust=0, family = fontFam)+
    annotate("text", x=c(0, 1.5, 2), y=max(df$yplace)+yAdjust*2, label = header, hjust=c(0, 0.5, 0), family = fontFam, fontface="bold")+
    geom_rect(aes(xmin=2, ymin=1-0.5, xmax=max(df$segmentEnd), ymax=max(df$yplace)+yAdjust), fill=adjustcolor("turquoise", 0.03), inherit.aes = FALSE)+
    geom_text(aes(x=xplace2, y=yplace, label=RevPAR), fontface="bold", family = fontFam)+
    geom_text(aes(x=xplace3, y=yplace, label=EndringOut), hjust=1, family = fontFam)+
    themeEmpty()+
    geom_segment(aes(x=0, y = max(df$yplace)+yAdjust, xend = max(df$segmentEnd), yend = max(df$yplace)+yAdjust), lwd=1)+
    geom_segment(aes(x=0, y = df$yplace-0.5, xend = max(df$segmentEnd), yend = df$yplace-0.5), lty=2, color="darkgray")+
    geom_segment(aes(x = xplaceBar, y = yplace, xend = segmentEnd, yend = yplace, color=segmentCols), lwd=5)+
    geom_segment(aes(x = xplaceBar, y = min(df$yplace)-0.5, xend = xplaceBar, yend=max(df$yplace+yAdjust-0.025)), lwd=0.1, col="gray")

  if(outputReady){
    ggOut
  } else {
    ggOut+
      coord_fixed(ratio=1/6)
  }

}


#' Define weights based on the proportion of shared borders
#'
#' @description Given a spatial polygon dataframe, this function provides a list of weights
#'  given for each area by the proportion of borders the neighbourhood of a that area
#'
#'  @param \code{spdf} An object of class \code{sp} I think. A spatial polygon data frame, however.
#'
#' @return a list
#'
#'
#'
#'
#'
#'
#'
#'

borders.w <- function(spdf){
  intersections <- sf::st_as_sf(spdf) %>%
    sf::st_intersection() %>% dplyr::filter(.data$n.overlaps == 2)
  intersections$SBL <- sf::st_length(intersections)

  borders.df <- intersections %>% sf::st_drop_geometry() %>%
    dplyr::select(-.data$n.overlaps) %>% cbind(matrix(unlist(.$origins), byrow = TRUE, ncol = 2)) %>%
    SchoolDataIT:::rename_by_idx(c(ncol(.)-1, ncol(.)), into = c("Mun1", "Mun2")) %>%
    dplyr::select(.data$Mun1, .data$Mun2, .data$SBL) %>%
    structure(class = c("tbl_df", "tbl", "data.frame"))

  if(!all(borders.df$Mun2 > borders.df$Mun1)){
    borders.df %<>% apply(MARGIN = 1, FUN = function (x){
      if(x[2] < x[1])  x[1,2] <- x[2,1]
      return(x)
    })
    borders.df %<>% t() %>% as.data.frame() %>% structure(class = c("tbl_df", "tbl", "data.frame"))
  }

  isles <- sf::st_as_sf(spdf) %>%
    sf::st_intersection() %>% sf::st_drop_geometry() %>%
    dplyr::filter(.data$n.overlaps == 1) %>%
    dplyr::select(.data$origins) %>%
    dplyr::filter(!.data$origins %in% c(borders.df$Mun1, borders.df$Mun2)) %>%
    unname() %>% unlist()

  instances <- sort(unique(c(borders.df$Mun1, borders.df$Mun2, isles)))

  for(i in instances){
    if(i == 1){
      borders <- list(NULL)
    } else borders <- append(borders[1:(i-1)], list(NULL))

    add1 <- borders.df %>% dplyr::filter(.data$Mun2 == i) %>% dplyr::select(-.data$Mun2) %>%
      dplyr::arrange(.data$Mun1) %>% dplyr::select(.data$SBL) %>% unname %>% unlist()
    add2 <- borders.df %>% dplyr::filter(.data$Mun1 == i) %>% dplyr::select(-.data$Mun1) %>%
      dplyr::arrange(.data$Mun2) %>% dplyr::select(.data$SBL) %>% unname %>%  unlist()
    borders[[i]] <- unlist(c(borders[[i]], add1, add2))
  }

  borders %<>% lapply(function(x) return(x/sum(x)))

  return(borders)
}

#' Compute the spatial correlaiton between two variables
#'
#' @description This function calculates the spatial correlation index between two fields of a given \code{data} object, associated with a shapefile.
#'
#' @param data  Object of class \code{data.frame}.The input data object.
#' @param X1 Character. The first of the two variables whose spatial correlation is sought for.
#' @param X1 Character. The first of the two variables whose spatial correlation is sought for.
#' @param input_shp Object of class \code{sf}, \code{tbl.df}, \code{tbl} and \code{data.frame}.
#' The relevant shapefiles of Italian administrative boundaries,
#' at the selected level of detail (LAU or NUTS-3), from which the adjacency structure is derived.
#' If \code{NULL}, it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param level Character. The administrative level of detail at which data are defined.
#' Either \code{"LAU"}/\code{"Municipality"} or \code{"NUTS-3"}/\code{"Province"}. \code{"LAU"} by default.
#' @param ... Additional arguments to the function \code{\link{Get_shapefile}}, if \code{input_shp} is not provided.
#'
#'
#' @export
#'


Util_Spatial_Correlation <- function(data, X1 = NULL, X2 = NULL, level = "LAU", input_shp, ...){
  if(is.null(data)){
    stop("Please, provide an input object")
  }
  if(is.null(input_shp)){
    cat("Downloading shapefile ... \n")
    input_shp <- Get_Shapefile(level, ...)
  }
  while(is.null(X1) || !X1 %in% names(data)){
    message("Please, insert the first variable")
    X1 <- readline(prompt = "  > ")
  }
  while(is.null(X2) || !X1 %in% names(data)){
    message("Please, insert the first variable")
    X1 <- readline(prompt = "  > ")
  }

  if(level %in% c("Municipality", "LAU", "NUTS-4")){
    DB <- input_shp %>% dplyr::select(.data$PRO_COM_T) %>%
      dplyr::rename(Municipality_code = .data$PRO_COM_T) %>%
      dplyr::left_join(data, by = "Municipality_code")
  } else {
    DB <- input_shp %>% dplyr::select(.data$COD_PROV) %>%
      dplyr::rename(Province_code = .data$COD_PROV) %>%
      dplyr::left_join(data, by = "Province_code")
  }
  DB <- DB %>%
    dplyr::filter(!is.na(.data[[X1]])) %>%
    dplyr::filter(!is.na(.data[[X2]]))


  n <- nrow(DB)
  M <- matrix(0, nrow = n, n)

  nn <- sf::st_intersects(sf::st_geometry(DB))

  lst <- lapply(nn, function(x){
    V <- rep(0, nrow(DB))
    V[x] <- 1
    return(V)
  })
  W <- do.call(rbind, lst) - diag(1, n)

  X.1 <- scale(DB[[X1]]) *sqrt(n/(n-1))
  X.1 <- scale(DB[[X2]]) *sqrt(n/(n-1))


  R <- t(X.1) %*% (W/(sum(colSums(W)))) %*% X.2

  return(R)


}


X1 <- "Gross_building_volume"
X2 <- "Floors_number"
data <- DB23_MIUR$Province_data %>% dplyr::filter(.data$Order == "Middle") %>%
  dplyr::left_join(Invalsi_prov[,c(1,3)], by = "Province_code")
input_shp <- SchoolDataIT::Get_Shapefile(2022, level = "NUTS-3")
level <- "NUTS-3"

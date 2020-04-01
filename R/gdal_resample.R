#' gdal_resample
#'
#' Function does a system call to run gdal functions (works on unix systems where gdal can be called from terminal).
#' Use only file path, function will not work with raster objects loaded in R environment.
#'
#' @param infile character  Raster to be cropped (file path or Raster object). Input raster should be stored on disk for GDAL access.
#' @param outfile character Path and filename of the mosaicked output.
#' @param resolutions  numeric ResX e ResY as vector.
#' @param target_extent character New extent as xmin ymin xmax ymax".
#' @param method character Resampling method. One in c("near", "bilinear", "cubic", "cubicspline", "lanczos",
#' "average", "mode", "max", "min", "med", "q1", "q3").
#' @param large_tif logical  Use \code{large_tif = TRUE} for large rasters (>4GB).
#' @param return_raster logical The function stores the raster in the \code{filename} argument path as a side-effect.
#'
#' @return Raster object. Only if \code{return_raster = TRUE}. Otherwise, the function side-effect is to save the file locally.
#' @export
#'
#' @importFrom raster raster
#' @importFrom tools file_ext
#'

gdal_resample <- function(infile, outfile, resolutions, target_extent, method, large_tif = FALSE, return_raster = FALSE)
{

 if (!method %in% c("near", "bilinear", "cubic", "cubicspline", "lanczos",
                     "average", "mode", "max", "min", "med", "q1", "q3")) {
    stop("Resampling method not available.")
    }

  # If input is raster, extract the file path
  if (inherits(infile, "Raster"))
{
  infile <- infile@file@name
 }

resample_command <- paste0("gdalwarp -multi -of vrt -tr ", " ", resolution[1], " ", resolution[2], " -r ", method, " -te ", target_extent, " ",
 infile, " ", gsub(tools::file_ext(outfile), "vrt", outfile))

  if (large_tif == TRUE)
  {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES", " ", gsub(tools::file_ext(outfile), "vrt", outfile),
                      " ", gsub(tools::file_ext(outfile), "tif", outfile))
  } else {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW", " ", gsub(tools::file_ext(outfile), "vrt", outfile),
                      " ", gsub(tools::file_ext(outfile), "tif", outfile))
  }

  system(resample_command)
  system(VRT2TIF)
  # Remove vrt
  unlink(gsub(tools::file_ext(outfile), "vrt", outfile))

  if (isTRUE(return_raster))
  {
    outfile <- raster::raster(outfile)
	return(outfile)
  }
}




#' Detects Image Bytes by File Signature
#'
#' This function inspects a raw vector and reports whether its leading bytes match the file signature of a common image format (PNG, JPEG, GIF or BMP) for API result image rendering.
#'
#' @param x Raw vector. This is typically the body of an API response decoded by \code{\link{decode_api_response}}. Required.
#' @return This function returns the logical scalar \code{TRUE} when \code{x} begins with a recognised image signature, \code{FALSE} otherwise.
#' @details
#' ChatRBox centralizes image handling, since generated client functions return raw image bytes rather than rendering graphics themselves. Client functions downloaded from the \code{client_fns} endpoint of APIs provided via \code{services_list} provide users with more control such that image rendering may be facilitated within client functions. However, otherwise centralizing image rendering within \code{llm_api_result} acts as the most robust and standardized ChatRBox workflow. This function lets \code{\link{decode_api_response}} and \code{\link{render_api_image}} recognize raw image payloads even when an API omits or mislabels its content type, so that \code{\link{llm_api_result}} can render the image consistently in the active graphics device.
#' @seealso \code{\link{image_bytes_format}}, \code{\link{render_api_image}}, \code{\link{llm_api_result}}
#' @example man/examples/examples_image_handling.R
#' @export
is_image_bytes <- function(x) {
  
  if (!is.raw(x) || length(x) < 4L) {
    return(FALSE)
  }
  
  sig <- function(hex) {
    length(x) >= length(hex) && all(x[seq_along(hex)] == as.raw(hex))
  }
  
  sig(c(0x89, 0x50, 0x4E, 0x47)) || # PNG
    sig(c(0xFF, 0xD8, 0xFF)) ||     # JPEG
    sig(c(0x47, 0x49, 0x46)) ||     # GIF
    sig(c(0x42, 0x4D))              # BMP
}

#' Detects the Image Format of Raw Bytes
#'
#' This function inspects the leading bytes (file signature) of a raw vector and returns the name of the image format it encodes. This function is used by \code{\link{render_api_image}} to select the correct decoder when rendering raw image bytes returned by a generated client function.
#'
#' @param x Raw vector. This is typically the body of an API response decoded by \code{\link{decode_api_response}}. Required.
#' @return One of \code{"png"}, \code{"jpeg"}, \code{"gif"}, \code{"bmp"} based on extracted image signature, or \code{NA_character_} when no known image signature is detected.
#' @seealso \code{\link{is_image_bytes}}, \code{\link{render_api_image}}
#' @example man/examples/examples_image_handling.R
#' @export
image_bytes_format <- function(x) {
  if (!is.raw(x) || length(x) < 4L) {
    return(NA_character_)
  }
  
  sig <- function(hex) {
    length(x) >= length(hex) && all(x[seq_along(hex)] == as.raw(hex))
  }
  
  if (sig(c(0x89, 0x50, 0x4E, 0x47))) return("png")
  if (sig(c(0xFF, 0xD8, 0xFF))) return("jpeg")
  if (sig(c(0x47, 0x49, 0x46))) return("gif")
  if (sig(c(0x42, 0x4D))) return("bmp")
  
  NA_character_
}

#' Detects Already-Decoded Image Arrays
#'
#' This function detects common image-like R objects returned by image decoders. This function intentionally avoids treating arbitrary numeric matrices as images to maintain generalizability, since a data matrix may be a legitimate analysis result. This helper function is used alongside \code{\link{is_image_bytes}} within \code{\link{render_api_image}} in order to most suitably render image outputs of API services.  
#'
#' @param x Potential image array. Object to test. Required. 
#' @return Logical scalar. \code{TRUE} when \code{x} is an already-decoded image object (a \code{raster}/\code{nativeRaster}, or a numeric 3D array whose third dimension is 3 (RGB) or 4 (RGBA)); \code{FALSE} otherwise.
#' @seealso \code{\link{is_image_bytes}}, \code{\link{render_api_image}}
#' @example man/examples/examples_image_handling.R
#' @export
is_image_array <- function(x) {
  
  if (inherits(x, "nativeRaster") || inherits(x, "raster")) {
    return(TRUE)
  }
  
  is.array(x) &&
    length(dim(x)) == 3L &&
    dim(x)[3L] %in% c(3L, 4L) &&
    is.numeric(x)
}

#' Renders an API Image or Plot Result
#'
#' This function attempts to render image-like API outputs in the active graphics device and returns \code{TRUE} if rendering occurred and \code{FALSE} otherwise. This function only has side effects when rendering succeeds, meaning it does not change or return the original API result.
#' @param x Image output. API result object to render in graphics device. Required.
#' @return This function returns the logical scalar \code{TRUE} if \code{x} is rendered and \code{FALSE} otherwise.
#' @details
#' This function is the main facilitator of ChatRBox centralized image rendering since it determines and enables image rendering based on the API output type. This function supports raw PNG bytes, raw JPEG bytes, GIF/BMP, already-decoded raster/nativeRaster/image arrays, \pkg{ggplot2} objects and \code{recordedplot} objects, all of which render via specific methods.
#' @importFrom grid grid.newpage grid.raster
#' @importFrom grDevices as.raster replayPlot
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom magick image_read
#' @example man/examples/examples_image_handling.R
#' @export
render_api_image <- function(x) {
  
  if (is.null(x)) {
    return(FALSE)
  }
  
  if (inherits(x, "ggplot")) {
    print(x)
    return(TRUE)
  }
  
  if (inherits(x, "recordedplot")) {
    grDevices::replayPlot(x)
    return(TRUE)
  }
  
  if (inherits(x, "nativeRaster") || inherits(x, "raster")) {
    grid::grid.newpage()
    grid::grid.raster(x)
    return(TRUE)
  }
  
  if (is_image_array(x)) {
    grid::grid.newpage()
    grid::grid.raster(grDevices::as.raster(x))
    return(TRUE)
  }
  
  if (is.raw(x) && is_image_bytes(x)) {
    fmt <- image_bytes_format(x)
    
    img <- switch(
      fmt,
      png = {
        if (!requireNamespace("png", quietly = TRUE)) {
          return(FALSE)
        }
        tryCatch(png::readPNG(x), error = function(e) NULL)
      },
      jpeg = {
        if (!requireNamespace("jpeg", quietly = TRUE)) {
          return(FALSE)
        }
        tryCatch(jpeg::readJPEG(x), error = function(e) NULL)
      },
      gif = {
        if (!requireNamespace("magick", quietly = TRUE)) {
          return(FALSE)
        }
        tryCatch(as.raster(magick::image_read(x)[1]), error = function(e) NULL)
      },
      bmp = {
        if (!requireNamespace("magick", quietly = TRUE)) {
          return(FALSE)
        }
        tryCatch(as.raster(magick::image_read(x)), error = function(e) NULL)
      },
      NULL
    )
    
    if (is.null(img)) {
      return(FALSE)
    }
    
    grid::grid.newpage()
    
    if (inherits(img, "raster") || inherits(img, "nativeRaster")) {
      grid::grid.raster(img)
    } else {
      grid::grid.raster(grDevices::as.raster(img))
    }
    
    return(TRUE)
  }
  
  FALSE
}

#' Converts API Results to User-Facing Display Value
#'
#' This function converts special API result types into concise placeholders for the named list returned by \code{\link{llm_api_result}}. This affects only the value shown to the user; it does not change what is stored in \code{object_env}, so chained API calls always reuse the original result object. Hence, rendered images will be stored as image bytes in \code{object_env} but intuitive placeholders in the AI prompt via \code{past_outputs}. 
#' @param result API output. Original API result object, as returned by a client function. Required.
#' @param rendered_image Logical. \code{TRUE} when \code{\link{render_api_image}} has already rendered \code{result} in the active graphics device. Hence, users see the relevant placeholder in the R console.
#' @return The object to place in the named results list returned by \code{\link{llm_api_result}}: the short placeholder string \code{"Image output"} for rendered images and raw/binary payloads, or \code{result} unchanged otherwise.
#' @details
#' API services provided via \code{services_list} must have an endpoint named \code{client_fns} containing client function source code. This gives users more control over API calling, such that these client functions may involve image rendering. However, in general, image rendering is centralized within the ChatRBox package to maximize reproducibility and scalability. Therefore, generated client functions for API services provided via \code{openapi_list} will return image bytes, whereby image rendering occurs within \code{\link{llm_api_result}}. Therefore, this function provides placeholder names for the R console based on result type, whilst image bytes are presented as images on a graphics panel. 
#' @seealso \code{\link{render_api_image}}, \code{\link{llm_api_result}}
#' @example man/examples/examples_image_handling.R
#' @export
format_api_result_for_display <- function(result, 
                                          rendered_image = FALSE) {
  
  # Any rendered image gets the same placeholder
  if (isTRUE(rendered_image) || 
      is.raw(result) || 
      inherits(result, c("nativeRaster", "raster"))) {
    return("Image output")
  }
  
  # NULL gets explicit placeholder
  if (is.null(result)) {
    return("NULL")
  }
  
  # Everything else returns as-is
  result
}

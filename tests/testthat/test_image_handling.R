testthat::test_that("render_api_image renders images and ignores non-images", {
  testthat::expect_false(render_api_image("just text"))
  testthat::expect_false(render_api_image(list(a = 1)))
  testthat::expect_false(render_api_image(as.raw(c(0x01, 0x02, 0x03, 0x04))))
})

testthat::test_that("is_image_bytes detects common image signatures", {
  png_bytes  <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  jpeg_bytes <- as.raw(c(0xFF, 0xD8, 0xFF, 0xE0))
  gif_bytes  <- as.raw(c(0x47, 0x49, 0x46, 0x38))
  bmp_bytes  <- as.raw(c(0x42, 0x4D, 0x00, 0x00))
  
  testthat::expect_true(is_image_bytes(png_bytes))
  testthat::expect_true(is_image_bytes(jpeg_bytes))
  testthat::expect_true(is_image_bytes(gif_bytes))
  testthat::expect_true(is_image_bytes(bmp_bytes))
  
  testthat::expect_false(is_image_bytes(charToRaw("not an image")))
  testthat::expect_false(is_image_bytes(as.raw(c(0x01, 0x02))))
  testthat::expect_false(is_image_bytes("a string"))
  testthat::expect_false(is_image_bytes(1:5))
})

test_that("image_bytes_format correctly identifies image formats", {
  # PNG signature test
  png_signature <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  expect_equal(image_bytes_format(png_signature), "png")
  
  # JPEG signature test
  jpeg_signature <- as.raw(c(0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46))
  expect_equal(image_bytes_format(jpeg_signature), "jpeg")
  
  # GIF signature test
  gif_signature <- as.raw(c(0x47, 0x49, 0x46, 0x38, 0x37, 0x61))
  expect_equal(image_bytes_format(gif_signature), "gif")
  
  # BMP signature test
  bmp_signature <- as.raw(c(0x42, 0x4D, 0x66, 0x00, 0x00, 0x00))
  expect_equal(image_bytes_format(bmp_signature), "bmp")
})

test_that("image_bytes_format handles edge cases", {
  # Empty raw vector
  expect_equal(image_bytes_format(raw()), NA_character_)
  
  # Too short raw vector
  short_vector <- as.raw(c(0x89, 0x50))
  expect_equal(image_bytes_format(short_vector), NA_character_)
  
  # Non-raw input
  expect_equal(image_bytes_format(c(137, 80, 78, 71)), NA_character_)
})

test_that("image_bytes_format correctly handles partial or incorrect signatures", {
  # Partial signatures
  partial_png <- as.raw(c(0x89, 0x50, 0x4E))
  expect_equal(image_bytes_format(partial_png), NA_character_)
  
  # Incorrect signatures
  incorrect_sig1 <- as.raw(c(0x00, 0x00, 0x00, 0x00))
  expect_equal(image_bytes_format(incorrect_sig1), NA_character_)
})

test_that("image_bytes_format works with longer byte sequences", {
  # PNG with additional bytes
  png_with_extra <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 
                             rep(0x00, 100)))
  expect_equal(image_bytes_format(png_with_extra), "png")
  
  # JPEG with additional bytes
  jpeg_with_extra <- as.raw(c(0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46, 
                              rep(0x00, 100)))
  expect_equal(image_bytes_format(jpeg_with_extra), "jpeg")
})

describe("render_api_image", {
  test_that("it returns FALSE for NULL input", {
    expect_false(render_api_image(NULL))
  })
  
  test_that("it handles raster and nativeRaster objects", {
    raster_img <- matrix(c("red", "blue", "green", "yellow"), nrow = 2, ncol = 2)
    class(raster_img) <- "raster"
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    expect_true(render_api_image(raster_img))
  })
  
  test_that("it handles image arrays", {
    mock_image_array <- array(runif(3*3*3), dim = c(3, 3, 3))
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    with_mocked_bindings(
      is_image_array = function(x) TRUE,
      code = {
        expect_true(render_api_image(mock_image_array))
      }
    )
  })
  
  test_that("it returns FALSE for unsupported inputs", {
    expect_false(render_api_image(42))
    expect_false(render_api_image("not an image"))
    expect_false(render_api_image(list(1, 2, 3)))
  })
})

describe("render_api_image raw image bytes", {
  # PNG Test
  test_that("it handles PNG bytes", {
    skip_if_not_installed("png")
    
    # Create PNG
    png_matrix <- matrix(runif(9), nrow = 3, ncol = 3)
    png_bytes <- tryCatch({
      png::writePNG(png_matrix)
    }, error = function(e) skip("Could not create PNG bytes"))
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    with_mocked_bindings(
      is_image_bytes = function(x) TRUE,
      image_bytes_format = function(x) "png",
      code = {
        expect_true(render_api_image(png_bytes))
      }
    )
  })
  
  # JPEG Test
  test_that("it handles JPEG bytes", {
    skip_if_not_installed("jpeg")
    
    # Create JPEG
    jpeg_matrix <- matrix(runif(9), nrow = 3, ncol = 3)
    jpeg_bytes <- tryCatch({
      jpeg::writeJPEG(jpeg_matrix)
    }, error = function(e) skip("Could not create JPEG bytes"))
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    with_mocked_bindings(
      is_image_bytes = function(x) TRUE,
      image_bytes_format = function(x) "jpeg",
      code = {
        expect_true(render_api_image(jpeg_bytes))
      }
    )
  })
  
  # GIF Test
  test_that("it handles GIF bytes", {
    skip_if_not_installed("magick")
    
    # Create GIF
    gif_bytes <- tryCatch({
      # Use a sample image from a package
      img_path <- system.file("img", "Rlogo.png", package="png")
      magick::image_write(magick::image_read(img_path), format = "gif")
    }, error = function(e) skip("Could not create GIF bytes"))
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    with_mocked_bindings(
      is_image_bytes = function(x) TRUE,
      image_bytes_format = function(x) "gif",
      code = {
        expect_true(render_api_image(gif_bytes))
      }
    )
  })
  
  # BMP Test
  test_that("it handles BMP bytes", {
    skip_if_not_installed("magick")
    
    # Create BMP
    bmp_bytes <- tryCatch({
      img_path <- system.file("img", "Rlogo.png", package="png")
      magick::image_write(magick::image_read(img_path), format = "bmp")
    }, error = function(e) skip("Could not create BMP bytes"))
    
    pdf(nullfile())
    on.exit(dev.off(), add = TRUE)
    with_mocked_bindings(
      is_image_bytes = function(x) TRUE,
      image_bytes_format = function(x) "bmp",
      code = {
        expect_true(render_api_image(bmp_bytes))
      }
    )
  })
})

testthat::test_that("is_image_array returns TRUE for nativeRaster and raster objects", {
  native_raster <- structure(
    matrix(as.raw(rep(0:9, 10)), nrow = 10, ncol = 10),
    class = "nativeRaster",
    dim = c(10, 10)
  )
  
  testthat::expect_true(is_image_array(native_raster))
  raster_obj <- structure(
    list(ncols = 10, nrows = 10, crs = NA),
    class = "raster"
  )
  testthat::expect_true(is_image_array(raster_obj))
})

testthat::test_that("format_api_result_for_display returns 'Image output' for rendered images", {
  result1 <- format_api_result_for_display(result = list(data = "ignored"), rendered_image = TRUE)
  testthat::expect_equal(result1, "Image output")
})

testthat::test_that("format_api_result_for_display returns 'Image output' for raw bytes", {
  raw_bytes <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  result2 <- format_api_result_for_display(result = raw_bytes, rendered_image = FALSE)
  testthat::expect_equal(result2, "Image output")
})

testthat::test_that("format_api_result_for_display returns 'Image output' for nativeRaster/raster", {
  native_raster <- structure(
    matrix(as.raw(rep(0:9, 10)), nrow = 10, ncol = 10),
    class = "nativeRaster",
    dim = c(10, 10)
  )
  result3 <- format_api_result_for_display(result = native_raster, rendered_image = FALSE)
  testthat::expect_equal(result3, "Image output")
  raster_obj <- structure(
    list(ncols = 10, nrows = 10, crs = NA),
    class = "raster"
  )
  result4 <- format_api_result_for_display(result = raster_obj, rendered_image = FALSE)
  testthat::expect_equal(result4, "Image output")
})

testthat::test_that("format_api_result_for_display returns 'NULL' for null results", {
  result5 <- format_api_result_for_display(result = NULL, rendered_image = FALSE)
  testthat::expect_equal(result5, "NULL")
})

testthat::test_that("format_api_result_for_display returns result unchanged for non-image data", {
  regular_data <- list(key = "value")
  result6 <- format_api_result_for_display(result = regular_data, rendered_image = FALSE)
  testthat::expect_equal(result6, regular_data)
})

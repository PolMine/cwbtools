
rng_file <- file.path(data_dir, "tmp.rng")
rng_file_size <- file.info(rng_file)$size
rng <- readBin(con = rng_file, what = integer(), size = 4L, n = rng_file_size, endian = "big")

avx_file <- file.path(data_dir, "date.avx")
avx_file_size <- file.info(avx_file)$size
avx <- readBin(con = avx_file, what = integer(), size = 4L, n = avx_file_size, endian = "big")

avs_file <- file.path(data_dir, "date.avs")
avs_file_size <- file.info(avs_file)$size
avs <- readBin(con = avs_file, what = character(), n = avx_file_size)

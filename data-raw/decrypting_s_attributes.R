data_dir <- "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/GermaParl/extdata/cwb/indexed_corpora/germaparl"

rng_file <- file.path(data_dir, "cap.rng")
rng_file_size <- file.info(rng_file)$size
rng <- readBin(con = rng_file, what = integer(), size = 4L, n = rng_file_size, endian = "big")

avx_file <- file.path(data_dir, "cap.avx")
avx_file_size <- file.info(avx_file)$size
avx <- readBin(con = avx_file, what = integer(), size = 4L, n = avx_file_size, endian = "big")

avs_file <- file.path(data_dir, "cap.avs")
avs_file_size <- file.info(avs_file)$size
avs <- readBin(con = avs_file, what = character(), n = avx_file_size)

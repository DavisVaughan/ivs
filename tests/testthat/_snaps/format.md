# logical formatting

    Code
      iv(c(FALSE, NA), TRUE)
    Output
      <iv<logical>[2]>
      [1] [FALSE, TRUE) [NA, NA)     

# integer formatting

    Code
      iv(c(1L, 100L), 200L)
    Output
      <iv<integer>[2]>
      [1] [1, 200)   [100, 200)
    Code
      iv(100000000L, 1000000000L)
    Output
      <iv<integer>[1]>
      [1] [100000000, 1000000000)
    Code
      iv_pairs(c(1L, 5L), c(NA, NA))
    Output
      <iv<integer>[2]>
      [1] [1, 5)   [NA, NA)

# double formatting

    Code
      iv(c(1.5, 1000.5), 2000.5)
    Output
      <iv<double>[2]>
      [1] [1.5, 2000.5)    [1000.5, 2000.5)
    Code
      iv(1e+08, 1e+09)
    Output
      <iv<double>[1]>
      [1] [100000000, 1000000000)
    Code
      iv_pairs(c(1, 5), c(NA, NA))
    Output
      <iv<double>[2]>
      [1] [1, 5)   [NA, NA)
    Code
      iv(100.555, 200)
    Output
      <iv<double>[1]>
      [1] [100.555, 200)
    Code
      iv(100000.5555, 2e+05)
    Output
      <iv<double>[1]>
      [1] [100000.6, 200000)
    Code
      with_options(digits = 12, print(iv(100000.5555, 2e+05)))
    Output
      <iv<double>[1]>
      [1] [100000.5555, 200000)

# character formatting

    Code
      iv(c("x", "xxxxxxx"), "zzzzzzz")
    Output
      <iv<character>[2]>
      [1] [x, zzzzzzz)       [xxxxxxx, zzzzzzz)
    Code
      iv_pairs(c("x", "z"), c(NA, NA))
    Output
      <iv<character>[2]>
      [1] [x, z)   [NA, NA)

# factor formatting

    Code
      start <- factor(c("x", "xxxxxxx"), levels = c("x", "xxxxxxx", "zzzzzzz"))
      end <- factor("zzzzzzz", levels = c("x", "xxxxxxx", "zzzzzzz"))
      iv(start, end)
    Output
      <iv<factor<8f086>>[2]>
      [1] [x, zzzzzzz)       [xxxxxxx, zzzzzzz)
    Code
      iv_pairs(factor(c("x", "z")), factor(c(NA, NA)))
    Output
      <iv<factor<e7195>>[2]>
      [1] [x, z)   [NA, NA)

# data frame formatting

    Code
      iv(vctrs::data_frame(x = 1:5), vctrs::data_frame(x = 2:6))
    Output
      <iv<data.frame<x:integer>>[5]>
      [1] [df[1,], df[1,]) [df[2,], df[2,]) [df[3,], df[3,]) [df[4,], df[4,])
      [5] [df[5,], df[5,])
    Code
      iv(vctrs::data_frame(x = 1:5), vctrs::data_frame(x = c(2, 3, NA, 5, NA)))
    Output
      <iv<data.frame<x:double>>[5]>
      [1] [df[1,], df[1,]) [df[2,], df[2,]) [NA, NA)         [df[4,], df[4,])
      [5] [NA, NA)        
    Code
      start <- vctrs::data_frame(x = 1, y = "x", z = 4L)
      end <- vctrs::data_frame(x = 2, y = "z", z = 5L)
      iv(start, end)
    Output
      <iv<data.frame<
        x: double
        y: character
        z: integer
      >>[1]>
      [1] [df[1,], df[1,])

# Date formatting

    Code
      iv(as.Date(c("2019-01-01", "2019-01-05")), as.Date("2019-01-10"))
    Output
      <iv<date>[2]>
      [1] [2019-01-01, 2019-01-10) [2019-01-05, 2019-01-10)
    Code
      iv_pairs(as.Date(c("2019-01-01", "2019-01-05")), as.Date(c(NA, NA)))
    Output
      <iv<date>[2]>
      [1] [2019-01-01, 2019-01-05) [NA, NA)                

# POSIXt formatting

    Code
      iv(as.POSIXct(c("2019-01-01", "2019-01-05"), tz = "UTC"), as.POSIXct(
        "2019-01-10", tz = "UTC"))
    Output
      <iv<datetime<UTC>>[2]>
      [1] [2019-01-01 00:00:00, 2019-01-10 00:00:00)
      [2] [2019-01-05 00:00:00, 2019-01-10 00:00:00)
    Code
      iv_pairs(as.POSIXct(c("2019-01-01", "2019-01-05"), tz = "UTC"), as.POSIXct(c(NA,
        NA), tz = "UTC"))
    Output
      <iv<datetime<UTC>>[2]>
      [1] [2019-01-01 00:00:00, 2019-01-05 00:00:00)
      [2] [NA, NA)                                  

# difftime formatting

    Code
      iv(as.difftime(c(1, 100), units = "secs"), as.difftime(200, units = "secs"))
    Output
      <iv<duration<secs>>[2]>
      [1] [1 secs, 200 secs)   [100 secs, 200 secs)
    Code
      iv(as.difftime(1e+08, units = "secs"), as.difftime(1e+09, units = "secs"))
    Output
      <iv<duration<secs>>[1]>
      [1] [100000000 secs, 1000000000 secs)
    Code
      iv(as.difftime(NA_real_, units = "secs"), as.difftime(NA_real_, units = "secs"))
    Output
      <iv<duration<secs>>[1]>
      [1] [NA secs, NA secs)

# integer64 formatting

    Code
      start <- bit64::as.integer64(c(1, 100))
      end <- bit64::as.integer64(200)
      iv(start, end)
    Output
      <iv<integer64>[2]>
      [1] [1, 200)   [100, 200)
    Code
      start <- bit64::as.integer64(100000000L)
      end <- bit64::as.integer64(1000000000L)
      iv(start, end)
    Output
      <iv<integer64>[1]>
      [1] [100000000, 1000000000)
    Code
      start <- bit64::as.integer64(NA)
      end <- bit64::as.integer64(NA)
      iv(start, end)
    Output
      <iv<integer64>[1]>
      [1] [NA, NA)

# iv formatting

    Code
      iv(iv(1:2, 5:6), iv(100, 200))
    Output
      <iv<iv<double>>[2]>
      [1] [[1, 5), [100, 200)) [[2, 6), [100, 200))
    Code
      iv(iv(c(1, NA), c(5, NA)), iv(100, 200))
    Output
      <iv<iv<double>>[2]>
      [1] [[1, 5), [100, 200)) [[NA, NA), [NA, NA))


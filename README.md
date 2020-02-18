
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iqtigfunctions

<!-- badges: start -->

<!-- badges: end -->

The goal of `iqtigfunctions` is to provide an implementation of IQTIG
specific R functions as used in the published indicator specifications
(QIDB). For a detailed, technical reference please use the `help`
function, or take a look at the IQTIG R readme (in German), distributed
with the QIDB.

## Operators

### Comparison

A number of binary comparison operators have been defined to compare two
numbers with a special handling of NAs.

  - `%!=%`
  - `%<%`
  - `%<=%`
  - `%==%`
  - `%>%`
  - `%>=%`

<!-- end list -->

``` r
5 %<=% 10
#> [1] TRUE
2 %==% NA_real_
#> [1] FALSE
```

### Between

In addition `%between%` checks element-wise if a vector is within an
interval.

``` r
1:5 %between% c(2, 4)
#> [1] FALSE  TRUE  TRUE  TRUE FALSE
```

### Matching

Some computional rules check values against a list of codes (e.g. ICD or
OPS codes). The following four operators have been defined to make it
easier to match values against a list of values. Theses values can also
contain so-called wildcards (`%`).

  - `%all_in%`
  - `%all_like%`
  - `%any_in%`
  - `%any_like%`

<!-- end list -->

``` r
data <- data.frame(
  OPSCHLUESSEL_1 = c("5-123.5", "5-134", "5-156"),
  OPSCHLUESSEL_2 = c("5-001", "5-100", NA_character_),
  OPSCHLUESSEL_3 = NA_character_,
  TDS_B = c(1, 2, 3),
  stringsAsFactors = FALSE
)

# the following function gathers all fields spread over multiple columns
# and converts it to a list column
data <- gather_composite_fields(data)
data$OPSCHLUESSEL
#> [[1]]
#> [1] "5-123.5" "5-001"  
#> 
#> [[2]]
#> [1] "5-134" "5-100"
#> 
#> [[3]]
#> [1] "5-156"
```

``` r
with(data, OPSCHLUESSEL %any_in% c("5-12", "5-123.2", "5-156"))
#> [1] FALSE FALSE  TRUE
```

``` r
with(data, OPSCHLUESSEL %all_like% c("5-123%", "5-1%", "5-168.12:R"))
#> [1] FALSE  TRUE  TRUE
```

### Grouping

Often it is necessary to compute a value on subsets of records defined
by a certain property. For example, the latest date of operation among
all operations of a patient.

  - `%group_by%`

<!-- end list -->

``` r
with(mtcars, maximum(hp) %group_by% cyl)
#>  [1] 175 175 113 175 335 175 335 113 113 175 175 335 335 335 335 335 335 113 113
#> [20] 113 113 335 335 335 335 113 113 113 335 175 335 113
```

``` r
with(mtcars, maximum(hp) %group_by% c(cyl, drat))
#>  [1] 110 110  93 110 175 105 245  62  95 123 123 180 180 180 205 215 230  66  52
#> [20]  65  97 150 175 245 175  66  91 113 264 175 335 109
```

## General functions

There is also a small number of general functions used in computational
rules.

  - `maximum`
  - `minimum`
  - `replace_na`
  - `row_sums`
  - `to_year`

## License

MIT

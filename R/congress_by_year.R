congress_by_year <- function(year, year_type)
{
  if (year_type == "served") {
    congress <- ceiling((year - 1788) / 2)
  } else if (year_type == "elected") {
    congress <- ceiling((year - 1788) / 2) + 1
  }
  else {
    stop("set year_type to served or elected")
  }
}

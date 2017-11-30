
angle <- function(pt1, pt2) {
  conv = 180 / pi # conversion radians / degrees
  diff <- pt2 - pt1
  left <- sign(diff[1]) # is X diff < 0?
  left[left == 0] <- -1 # sign(0) = 0, need to keep as nonzero
  angle <- left * (diff[2] * pi + atan(diff[1] / diff[2]))
  angle[angle < 0] <- angle[angle < 0] + 2 * pi
  return(angle * conv)
}


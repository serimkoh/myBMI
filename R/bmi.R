#' Compute your body mass index
#'
#' This function computes your body mass index(bmi) and shows your status through a graph.
#'
#' @examples
#'
#' bmi('female', 166, 53)
bmi <- function(sex, height, weight) {

  if(sex == 'male')
  {
    total_weight_without_fat <- (1.10*weight)-(128*weight^2/height^2)
  }
  else if(sex == 'female')
  {
    total_weight_without_fat <- (1.07*weight)-(128*weight^2/height^2)
  }

  fat_mass <- weight - total_weight_without_fat

  fat_mass_ratio <- fat_mass*100/weight


  cat("본인의 현재 BMI 지수는 ",fat_mass_ratio,"입니다.\n")


  if(18.5<fat_mass_ratio && fat_mass_ratio<=25)
  {
    cat("현재 상태는 정상체중입니다.\n\n")
  }
  else
  {

    if(fat_mass_ratio<=18.5)
    {
      cat("현재 상태는 저체중입니다.\n")
      cat("정상 BMI지수에 도달하려면 ",18.5-fat_mass_ratio,"남았습니다.\n\n")
    }
    else{

      if(25<fat_mass_ratio &&fat_mass_ratio<=30)
      {
        cat("현재 상태는 비만1단계입니다.\n")
      }
      else if(30<fat_mass_ratio &&fat_mass_ratio<=40)
      {
        cat("현재 상태는 비만2단계입니다.\n")
      }
      else if(fat_mass_ratio>40)
      {
        cat("현재 상태는 비만3단계입니다.\n")
      }
      cat("정상 BMI지수에 도달하려면 ",fat_mass_ratio-25,"남았습니다.\n\n")
    }

  }


  x1<-fat_mass_ratio


  plot(x1, 0.5, type = 'o',col="red", xlim = c(0,50), ylim = c(0,1), ann=FALSE)
  title(main="비만도 측정 그래프", xlab="BMI")
  text(x1, 0.6, labels = "My BMI", col='red')
  abline( v = 18.5, lty = 1)
  abline( v = 25, lty = 1)
  abline( v = 30, lty = 1)
  abline( v = 40, lty = 1)
  text(9.5, 0.95, labels = "저체중", col='blue')
  text(22, 0.95, labels = "정상", col='Green')
  text(27.4, 0.95, labels = "1단계", col='orange')
  text(35, 0.95, labels = "2단계", col='orange')
  text(46, 0.95, labels = "3단계", col='red')

}




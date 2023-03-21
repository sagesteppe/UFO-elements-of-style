num_pr <- function(x){
  if(x > 10){y = x} else
    if(x == 9){y = 'nine'} else
      if(x == 8){y = 'eight'} else
        if(x == 7){y = 'seven'} else
          if(x == 6){y = 'six'} else
            if(x == 5){y = 'five'} else
              if(x == 4){y = 'four'} else
                if(x == 3){y = 'three'} else
                  if(x == 2){y = 'two'} else
                    if(x == 1){y = 'one'} else
                      if(x == 0){y = 'no'} else
                        if(x < 0){y = x}
  return(y)
}


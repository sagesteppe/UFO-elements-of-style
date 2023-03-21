num_pr <- function(x){
  if(x > 10){x = x} else
    if(x == 9){x = 'nine'} else
      if(x == 8){x = 'eight'} else
        if(x == 7){x = 'seven'} else
          if(x == 6){x = 'six'} else
            if(x == 5){x = 'five'} else
              if(x == 4){x = 'four'} else
                if(x == 3){x = 'three'} else
                  if(x == 2){x = 'two'} else
                    if(x == 1){x = 'one'} else
                      if(x == 0){x = 'no'} else
                        if(x < 0){x = x}
  print(x)
}

fun scoping a =
    (let val n = 1 
      in 
        let 
          val n = 2 
        in n 
      end 
    end)
    a + n;
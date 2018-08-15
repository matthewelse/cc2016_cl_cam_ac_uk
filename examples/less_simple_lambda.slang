    let f ( y : int ) : unit * unit = 
       if y = 0 then
          ((), ())
       else
          (
             fst (f ( y - 1 )), 
             snd (f ( y - 1 ))
          )
       end
in
    f (?)
end


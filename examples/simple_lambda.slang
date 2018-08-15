let f (x : int) : int =
  if x = 0 then 1
  else f ( x - 1 ) + f ( x - 1 )
  end
in
f(?)
end 

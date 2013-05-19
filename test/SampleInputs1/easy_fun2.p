define levelTwo
proc(b)
    return := b * 2
end;

define easy
proc(a)
   return := levelTwo(a)
end;

define main
proc(x)
result := easy(17)
end

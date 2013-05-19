define fact
proc(n)
	i := n;
	f := 1;
	while i do
		f := f * i;
		i := i - 1
		od;
	return := f
end;

define main
proc(n)
return := fact(3)
end

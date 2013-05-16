define fact
proc(n)
	i := n;
	f := 1;
	while i do
		f := f * i;
		i := i - 1
		od;
	return := f
end
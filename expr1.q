group by 0,1 {
	aggregate sum(0), sum(1)
}
flatten
group by 0 {}

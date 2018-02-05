group by 0,4 {
	aggregate count(0), sum(1), sum(2)
}
flatten
group by 0 {}

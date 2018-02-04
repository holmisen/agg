group by 0,4 {
	aggregate 1 by sum, 2 by sum
}
flatten
group by 0 {}

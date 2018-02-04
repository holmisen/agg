group by 0,1 {
	aggregate 0 by sum, 1 by sum
}
flatten
group by 0 {}

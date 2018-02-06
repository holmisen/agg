columns [Date, Author, I, D, File]

group by [Date, Author] {
	aggregate [sum(I), sum(D)]
}
flatten
group by [Date] {}

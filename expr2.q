columns [Date, Author, I, D, File]

group by [Date, File] {
	aggregate [count(Count), sum(I), sum(D)]
}
flatten
group by [Date] {}

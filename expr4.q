columns [Date, Author, I, D, File]

group by [Date, File] {
	aggregate [count(N), sum(I), sum(D)]
}
flatten
group by [Date] {
	sort by [N desc, File]
}

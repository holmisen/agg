columns [Date, Author, I, D, File]

group by [File] {
	sort by [I desc]
	take 1
}
take 2

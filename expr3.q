columns [Date, Author, I, D, File]

# Let's start
group by [File] {
	group by [Author] {
		aggregate [count(N), sum(I), sum(D)]
	}
}

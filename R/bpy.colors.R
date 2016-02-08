"bpy.colors" <-
function (n = 100, cutoff.tails = 0.1, alpha = 1)
{
    n <- as.integer(n[1])
    if (n <= 0)
        return(character(0))

	if (cutoff.tails >= 1 || cutoff.tails < 0)
		stop("cutoff.tails should be in [0, 1]")
	i = seq(0.5 * cutoff.tails, 1 - 0.5 * cutoff.tails, length = n)
    r = ifelse(i < .25, 0, ifelse(i < .57, i / .32 - .78125, 1))
    g = ifelse(i < .42, 0, ifelse(i < .92, 2 * i - .84, 1))
    b = ifelse(i < .25, 4 * i, ifelse(i < .42, 1,
        ifelse(i < .92, -2 * i + 1.84, i / .08 - 11.5)))
    rgb(r, g, b, alpha)
}

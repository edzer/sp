degreeLabelsNS = function(x) {
	pos = sign(x) + 2
	dir = c("*S", "", "*N")
	paste(abs(x), "*degree", dir[pos])
}
degreeLabelsEW = function(x) {
	x <- ifelse(x > 180, x - 360, x)
	pos = sign(x) + 2
	if (any(x == -180))
		pos[x == -180] = 2
	if (any(x == 180))
		pos[x == 180] = 2
	dir = c("*W", "", "*E")
	paste(abs(x), "*degree", dir[pos])
}

gridlines = function(x, easts = pretty(bbox(x)[1,]),
	norths = pretty(bbox(x)[2,]), ndiscr = 20)
{
	bb = bbox(x)
	easts <- easts[easts > bb[1,1] & easts < bb[1,2]]
	eastlist <- vector(mode="list", length=length(easts))
	for (i in 1:length(easts))
		eastlist[[i]] <- Line(cbind(rep(easts[i], ndiscr),
			seq(bb[2,1], bb[2,2], length.out=ndiscr)))
	norths <- norths[norths > bb[2,1] & norths < bb[2,2]]
	northlist <- vector(mode="list", length=length(norths))
	for (i in 1:length(norths))
		northlist[[i]] <- Line(cbind(seq(bb[1,1], bb[1,2], length.out=ndiscr),
			rep(norths[i], ndiscr)))
	SpatialLines(list(Lines(northlist, "NS"), Lines(eastlist, "EW")),
		CRS(proj4string(x)))
}

gridat <- function(x, easts = pretty(bbox(x)[1,]),
	norths = pretty(bbox(x)[2,]), offset=0.5, side="WS")
{
	isp = is.projected(x)
	if (is.na(isp) || isp) stop("x must not be projected")
	bb = bbox(x)
        ac <- ifelse (side == "WS", 1L, 2L)
	easts <- easts[easts > bb[1,1] & easts < bb[1,2]]
	norths <- norths[norths > bb[2,1] & norths < bb[2,2]]
	a1 <- cbind(easts, rep(bb[2,ac], length(easts)))
	a1lab <- degreeLabelsEW(a1[,1])
	a2 <- cbind(rep(bb[1,ac], length(norths)), norths)
	a2lab <- degreeLabelsNS(a2[,2])
	as <- SpatialPoints(rbind(a1, a2), CRS(proj4string(x)))
	res <- SpatialPointsDataFrame(as,
		data.frame(labels = c(a1lab, a2lab),
			pos = c(rep(1L+((ac-1)*2), length(easts)),
                        rep(2L+((ac-1)*2), length(norths))),
			offset = rep(offset, length(easts)+length(norths)),
                           stringsAsFactors = FALSE
		)
	)
	res
}

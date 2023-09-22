

MgToMol <- function (x, mol_mass = 32)
	{
	#millgrams to grams
	y <- x / 1000
	#grams to moles
	z <- y / mol_mass
	}







MolToMg <- function (x, mol_mass = 32)
	{
	#moles to grams
	y <- x * mol_mass
	#grams to millgrams
	z <- y * 1000
	}

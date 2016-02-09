HOW THE MAP FORMAT WORKS:


	The game maps are completely modular. There is no hard-coded default map but the map you want to use must be placed in the maps folder and named whatever name the program is set to use [currently ‘river_frontier’]. A map consists of two files: one text file containing all its data, and a paired picture with the same name (formats .txt and .png, respectively). If the map file is properly written and its dimensions agree with the resolution of the image (currently 35 pixels and 100 points per cell, see existing map files for an example), the map will be successfully loaded by the game and displayed by the GUI. Please note, however, that this part has not been tested very much and may lead to some unexpected changes in the game. Especially the AI may be easily confused by a very odd-shaped map, so please use the current map river_frontier as reference and create maps that are somewhat similar unless you are a lover of bugs.

Below the entire river_frontier.txt format:




AgeOfLords_MapFile
mapWidth: 4000
mapHeight: 2500
pointsPerCell: 100
map:
R R R R R R R R R R R R R R R R F F F R R R R R R R R R R R R R R R R R R R R R
R R R R R R R R R R R R R R R F F F F O R R R R R R O P P P P P P P P P P P P R
R R P P P P O R R R R R R O O F F F P P P P P P P P P P P P P P P P P P P P P P
R P P P P P P P O P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P R P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F P P P P C F F F F P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P F F F F F F C F F F F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P F F F F F C P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P R P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P
P P P P P P P P P P P P P P P P P P P P P P P F F P P P P P P P P P P P P P P P
territories:
{
type: player
cells:
(0:14, 0:19)
(0:18, 20:24)
}
{
type: ai
cells:
(21:39, 0:4)
(25:39, 5:24)
}
{
type: province
flag: (19, 8)
cells:
(15:20, 0:4)
(15:24, 5:11)
(15:19, 12)
}
{
type: province
flag: (20, 18) 
cells:
(20:24, 12)
(15:24, 13:19)
(19:24, 20:24)
}
end




As you may see, the format is text-based and human-readable. The top contains the format type and map dimensions, followed by a character representation of the map’s terrain. The map dimensions must be the same as the map representation, and must work together with the map image (where one cell is typically represented by 35x35 pixels). In this version of the program, the different terrain types are represented by the following characters:
	P – Plains		(accessible by anything)
	S – Saltwater		(accessible by nothing)
	F – Freshwater		(accessible by nothing)
	O – Obstacle		(accessible by characters and fortifications)
	C – Construction	(accessible by characters)
	R – Rock		(accessible by nothing)
Any other characters will break the game. After this comes the list of territories. There must always be a player and an AI territory (see the example above), and provinces may be added in any number as long as no territory overlaps another. The provinces (neutral territories that can be occupied for an increased resource production) have a flag, which is at the cell that must be conquered in order to conquer the province itself. The file must end with the word ‘end’.

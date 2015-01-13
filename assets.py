#
# Assets.py
# Generates assets for our Chess game
# 
# Jonatan H Sundqvist
# January 11 2015

# TODO | -
#        -

# SPEC | -
#        -



import pygame
import subprocess


glyphs = { '♙': 'whitePawn',
           '♖': 'whiteRook',
           '♘': 'whiteKnight',
           '♗': 'whiteBishop',
           '♕': 'whiteQueen',
           '♔': 'whiteKing',

           '♟': 'blackPawn',
           '♜': 'blackRook',
           '♞': 'blackKnight',
           '♝': 'blackBishop',
           '♛': 'blackQueen',
           '♚': 'blackKing' }



def saveGlyph(glyph, typeface, size, color, fn):

	'''
	Saves a glyph as an image in the working directory

	'''

	# print('\n'.join(sorted(pygame.font.get_fonts())))
	# print(font)
	assert typeface in pygame.font.get_fonts()

	font = pygame.font.SysFont(typeface, size)
	surface = font.render(glyph, True, color, None)
	print('Saving glyph of width {0} and {1}'.format(surface.get_size()[0]//12, surface.get_size()[1]))
	pygame.image.save(surface, fn)



def main():

	'''
	Docstring goes here

	'''

	pygame.init()

	saveGlyph('♙♖♘♗♕♔♟♜♞♝♛♚', 'dejavusans', 80, (255, 255, 255), 'assets/pieces.bmp') # Deja Vu Sans works!

	# for glyph, name in glyphs.items():
		# saveGlyph(glyph, 'Times New Roman', 80, (255, 255, 255), '{0}.bmp'.format(name))



if __name__ == '__main__':
	main()
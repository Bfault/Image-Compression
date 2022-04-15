##
## EPITECH PROJECT, 2021
## compressor
## File description:
## Makefile
##

NAME			=			imageCompressor

RPNAME			=			compressor

BINARY_PATH 	:=			$(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/$(RPNAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
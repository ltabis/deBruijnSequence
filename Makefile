##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## makefile for deBruijn
##

NAME=	deBruijn
NAME_BIN=	deBruijn-exe

PA := $(shell stack path --local-install-root)

all: $(NAME)

tests_run:
		@stack test --coverage
		@firefox --new-window $(PA)/hpc/index.html

$(NAME):
		stack build
		cp $(PA)/bin/$(NAME_BIN) .
		mv $(NAME_BIN) $(NAME)

clean:
	@find -type f -name '*~' -delete
	@find -type f -name '#*#' -delete
	@find -type f -name '*.gcda' -delete
	@find -type f -name '*.gcno' -delete

fclean: clean
	rm -f $(NAME)

re:	fclean all

.PHONY: all $(NAME) clean fclean re

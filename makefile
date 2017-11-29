Kaesekaestchen: Kaesekaestchen-exe elm.js


start: Kaesekaestchen
	stack exec Kaesekaestchen -- start


install: Kaesekaestchen
	stack install	


clean:
	rm -rf .stack-work/
	rm ./static/elm.js
	rm -rf ./src/client/elm-stuff/
	rm ./src/client/Api/Game.elm


elm.js : Game.elm
	cd ./src/client && \
	elm make --yes Main.elm --output ../../static/elm.js


Game.elm: Kaesekaestchen-exe
	stack exec Kaesekaestchen -- export


Kaesekaestchen-exe: setup
	stack build


setup:
	stack setup
	